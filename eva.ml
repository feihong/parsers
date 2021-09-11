(* Lexer for Eva language from http://dmitrysoshnikov.com/courses/essentials-of-interpretation/ *)
#mod_use "stream.ml"

type t =
  | Symbol of string
  | String of string
  | Number of float
  | EList of t list

let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

let implode chars = chars |> List.map (String.make 1) |> String.concat ""

let reverse_implode chars = chars |> List.rev |> implode

let is_symbol c =
  match c with
  | '*' | '+' | '-' | '/' | '_' -> true
  | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' -> true
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
    | Some c when is_symbol c ->
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

let rec parse_one stream =
  match Stream.next stream with
  | exception Stream.Failure -> errorf "No input"
  | c when is_space c -> parse_one stream
  | '-' | '.' | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
    parse_number c stream
  | c when is_symbol c -> parse_symbol c stream
  | '"' -> parse_string stream
  | '(' -> parse_list stream
  | c -> errorf "Unexpected character %c" c

and parse_list stream =
  let rec loop acc =
    match Stream.peek stream with
    | None -> errorf "Unterminated list"
    | Some ')' ->
      Stream.junk stream;
      Ok (EList (List.rev acc))
    | Some _ ->
      (match parse_one stream with
      | Error _ as err -> err
      | Ok x -> loop (x :: acc))
  in loop []

let parse s =
  let stream = Stream.of_string s in
  let rec loop acc =
    match parse_one stream with
    | Error "No input" -> Ok (List.rev acc)
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
  {|(a 1 "batarang")|}, [EList [Symbol "a"; Number 1.; String "batarang"]];
]

let () = cases |> List.iter (fun (s, value) -> assert (parse s = Ok value))
