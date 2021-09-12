(* Very basic S-expression parser *)
#mod_use "stream.ml"

type t =
  | Symbol of string
  | String of string
  | Number of float
  | List of t list

exception ParseError of string

exception EOF

let errorf fmt = Printf.ksprintf (fun s -> raise (ParseError s)) fmt

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
  let rec loop acc =
    match Stream.peek stream with
    | Some ('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' as c) ->
      Stream.junk stream;
      loop (c :: acc)
    | Some _ | None ->
      let s = reverse_implode acc in
      (match float_of_string_opt s with
      | None -> errorf "Invalid number: %s" s
      | Some n -> Number n)
  in loop [first_char]

let parse_symbol first_char stream =
  let rec loop acc =
    match Stream.peek stream with
    | Some c when is_symbol_body c ->
      Stream.junk stream;
      loop (c :: acc)
    | None | Some _ -> Symbol (reverse_implode acc)
  in loop [first_char]

let parse_string stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> errorf "Unterminated string: %s" (reverse_implode acc)
    | '"' -> String (reverse_implode acc)
    | '\\' ->
      (match Stream.next stream with
      | exception Stream.Failure -> errorf "Unterminated string: %s" (reverse_implode acc)
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
  | exception Stream.Failure -> raise EOF
  | c when is_space c -> parse_expr stream fn
  | '-' | '.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    fn (parse_number c stream)
  | '"' -> fn (parse_string stream)
  | c when is_symbol c -> fn (parse_symbol c stream)
  | '(' -> parse_list stream [] fn
  | c -> errorf "Unexpected character: %c" c

and parse_list stream acc fn =
  match Stream.peek stream with
  | None -> errorf "Unterminated list"
  | Some c when is_space c ->
    Stream.junk stream;
    parse_list stream acc fn
  | Some ')' ->
    Stream.junk stream;
    fn (List (List.rev acc))
  | Some _ -> parse_expr stream (fun x -> parse_list stream (x :: acc) fn)

let parse s =
  let stream = Stream.of_string s in
  let rec loop acc =
    match parse_expr stream (fun x -> x) with
    | exception EOF -> List.rev acc
    | x -> loop (x :: acc)
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
  {|(())|}, [List [List []]];
  {|(a 1 "batarang")|}, [List [Symbol "a"; Number 1.; String "batarang"]];
  {|(a (1 ("batarang")) b)|}, [List [Symbol "a"; List [Number 1.; List [String "batarang"]]; Symbol "b"]];
  {|foo (bar1) (baz 22)|}, [Symbol "foo"; List [Symbol "bar1"]; List [Symbol "baz"; Number 22.]];
]

let () = cases |> List.iter (fun (s, value) -> assert (parse s = value))

let error_cases = [
  "100.00.000", ParseError "Invalid number: 100.00.000";
  "-", ParseError "Invalid number: -";
  {|"Hey there sailor|}, ParseError "Unterminated string: Hey there sailor";
  "\"Hey there sailor\\", ParseError "Unterminated string: Hey there sailor";
  {|"Hey there\m sailor|}, ParseError "Unrecognized escape sequence: \\m";
  "abc def1 true~", ParseError "Unexpected character: ~";
  "(a b c", ParseError "Unterminated list";
]

let () = error_cases |> List.iter (fun (s, exc) ->
  match parse s with
  | exception e -> assert (e = exc)
  | _ -> assert false)
