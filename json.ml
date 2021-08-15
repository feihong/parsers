#mod_use "stream.ml"

type token =
  | True
  | False
  | Number of int
  | String of string
  | Colon
  | Comma
  | LBrace
  | RBrace
  | LBracket
  | RBracket

let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

let is_digit c = List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let implode chars = chars |> List.map (String.make 1) |> String.concat ""

let make_string chars = String (chars |> List.rev |> implode)

let make_number chars =
  match chars |> List.rev |> implode |> int_of_string_opt with
  | None -> Error "Invalid number"
  | Some n -> Ok (Number n)

let lex_number first_char stream =
  let rec loop acc =
    match Stream.peek stream with
    | Some c when is_digit c -> Stream.junk stream; loop (c :: acc)
    | (None | Some _) -> make_number acc
  in loop [first_char]

let lex_string stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> Error "unterminated string"
    | '\\' ->
      (match Stream.peek stream with
      | None -> Error "unterminated string"
      | Some c -> Stream.junk stream; loop (c :: '\\' :: acc))
    | '"' -> Ok (make_string acc)
    | c -> loop (c :: acc)
  in loop []

let tokenize stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> Ok (List.rev acc)
    | c when is_space c -> loop acc
    | '{' -> loop (LBrace :: acc)
    | '}' -> loop (RBrace :: acc)
    | '[' -> loop (LBracket :: acc)
    | ']' -> loop (RBracket :: acc)
    | ':' -> loop (Colon :: acc)
    | ',' -> loop (Comma :: acc)
    | 't' ->
      (match Stream.nnext 3 stream with
      | exception Stream.Failure -> Error "Unexpected end of input"
      | ['r'; 'u'; 'e'] -> loop (True :: acc)
      | _ -> Error "Unrecognized token")
    | 'f' ->
      (match Stream.nnext 4 stream with
      | exception Stream.Failure -> Error "Unexpected end of input"
      | ['a'; 'l'; 's'; 'e'] -> loop (False :: acc)
      | _ -> Error "Unrecognized token")
    | c when is_digit c || c == '-' ->
      (match lex_number c stream with
      | Error _ as err -> err
      | Ok token -> loop (token :: acc))
    | '"' ->
      (match lex_string stream with
      | Error _ as err -> err
      | Ok token -> loop (token :: acc))
    | c -> Error "Unexpected character"
  in loop []
