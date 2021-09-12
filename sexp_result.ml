(* Very basic S-expression parser implemented using Result *)
#mod_use "stream.ml"

type t =
  | Symbol of string
  | String of string
  | Number of float
  | List of t list

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

let implode chars = chars |> List.map (String.make 1) |> String.concat ""

let reverse_implode chars = chars |> List.rev |> implode

let is_symbol c =
  match c with
  | '*' | '+' | '-' | '/' | '_' -> true
  | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' -> true
  | _ -> false

let is_symbol_body c =
  match c with
  | c when is_symbol c -> true
  |'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let parse_number first_char stream =
  let rec loop ~has_dot acc =
    match Stream.peek stream with
    | Some '.' when has_dot -> errorf "Found second dot in number: %s" (reverse_implode ('.' :: acc))
    | Some '.' ->
      Stream.junk stream;
      loop ~has_dot:true ('.' :: acc)
    | Some ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c) ->
      Stream.junk stream;
      loop ~has_dot (c :: acc)
    | Some _ | None ->
      let s = reverse_implode acc in
      (match float_of_string_opt s with
      | None -> errorf "Invalid number: %s" s
      | Some n -> Ok (Number n))
  in loop ~has_dot:(first_char = '.') [first_char]

let parse_symbol first_char stream =
  let make_symbol chars = Ok (Symbol (reverse_implode chars)) in
  let rec loop acc =
    match Stream.peek stream with
    | Some c when is_symbol_body c ->
      Stream.junk stream;
      loop (c :: acc)
    | None | Some _ -> make_symbol acc
  in loop [first_char]

let parse_string stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> errorf "Unterminated string"
    | '"' -> Ok (String (reverse_implode acc))
    | '\\' ->
      (match Stream.next stream with
      | exception Stream.Failure -> errorf "Unterminated string"
      | 'n' -> loop ('\n' :: acc)
      | 't' -> loop ('\t' :: acc)
      | 'r' -> loop ('\r' :: acc)
      | '"' -> loop ('\"' :: acc)
      | c -> errorf "Unrecognized escape sequence: \\%c" c
      )
    | c -> loop (c :: acc)
  in loop []

let rec parse_expr stream fn =
  match Stream.next stream with
  | exception Stream.Failure -> fn (errorf "EOF")
  | c when is_space c -> parse_expr stream fn
  | '-' | '.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    fn (parse_number c stream)
  | '"' -> fn (parse_string stream)
  | c when is_symbol c -> fn (parse_symbol c stream)
  | '(' -> parse_list stream [] fn
  | c -> fn (errorf "Unexpected character %c" c)

and parse_list stream acc fn =
  match Stream.peek stream with
  | None -> fn (errorf "Unterminated list")
  | Some c when is_space c ->
    Stream.junk stream;
    parse_list stream acc fn
  | Some ')' ->
    Stream.junk stream;
    fn (Ok (List (List.rev acc)))
  | Some _ -> parse_expr stream (function
    | Error _ as err -> fn err
    | Ok x -> parse_list stream (x :: acc) fn)

let parse s =
  let stream = Stream.of_string s in
  let rec loop acc =
    match parse_expr stream (fun x -> x) with
    | Error "EOF" -> Ok (List.rev acc)
    | Error _ as err -> err
    | Ok thing -> loop (thing :: acc)
  in loop []

let cases = [
  "1", [Number 1.];
  "-1", [Number (-1.)];
  "801.122", [Number 801.122];
  ".8", [Number 0.8];
  "8.", [Number 8.];
  {|"wumpus"|}, [String "wumpus"];
  {|"It's not a \"bear\"\nit's a doggie"|}, [String "It's not a \"bear\"\nit's a doggie"];
  "foo-bar-baz", [Symbol "foo-bar-baz"];
  "*", [Symbol "*"];
  {|()|}, [List []];
  {|(a 1 "batarang")|}, [List [Symbol "a"; Number 1.; String "batarang"]];
  {|(a (1 ("batarang")) b)|}, [List [Symbol "a"; List [Number 1.; List [String "batarang"]]; Symbol "b"]];
  {|foo (bar) (baz 22)|}, [Symbol "foo"; List [Symbol "bar"]; List [Symbol "baz"; Number 22.]];
]

let () = cases |> List.iter (fun (s, value) -> assert (parse s = Ok value))
