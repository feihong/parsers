#mod_use "stream.ml"

module Lexer = struct
  type t =
    | True
    | False
    | Null
    | Number of float
    | String of string
    | Colon
    | Comma
    | LBrace
    | RBrace
    | LBracket
    | RBracket

  let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

  let digits = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

  let is_digit_or_dot c = List.mem c ('.' :: digits)

  let is_start_of_number c = List.mem c ('-' :: '.' :: digits)

  let implode chars = chars |> List.map (String.make 1) |> String.concat ""

  let make_string chars = String (chars |> List.rev |> implode)

  let errorf fmt = Printf.ksprintf (fun s -> Error s) fmt

  let make_number chars =
    let s = chars |> List.rev |> implode in
    match float_of_string_opt s with
    | None -> errorf "Invalid number %s" s
    | Some n -> Ok (Number n)

  let lex_number first_char stream =
    let rec loop acc =
      match Stream.peek stream with
      | Some c when is_digit_or_dot c -> Stream.junk stream; loop (c :: acc)
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

  let lex stream =
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
      | 'n' ->
        (match Stream.nnext 3 stream with
        | None -> Error "Unexpected end of input"
        | Some ['u'; 'l'; 'l'] -> loop (Null :: acc)
        | Some chars -> errorf "Unexpected characters %s" (implode chars))
      | 't' ->
        (match Stream.nnext 3 stream with
        | None -> Error "Unexpected end of input"
        | Some ['r'; 'u'; 'e'] -> loop (True :: acc)
        | Some chars -> errorf "Unexpected characters %s" (implode chars))
      | 'f' ->
        (match Stream.nnext 4 stream with
        | None -> Error "Unexpected end of input"
        | Some ['a'; 'l'; 's'; 'e'] -> loop (False :: acc)
        | Some chars -> errorf "Unexpected characters %s" (implode chars))
      | c when is_start_of_number c ->
        (match lex_number c stream with
        | Error _ as err -> err
        | Ok token -> loop (token :: acc))
      | '"' ->
        (match lex_string stream with
        | Error _ as err -> err
        | Ok token -> loop (token :: acc))
      | c -> errorf "Unexpected character %c" c
    in loop []
end

type json =
  | Number of float
  | Bool of bool
  | Null
  | String of string
  | Array of json list
  | Object of (string * json) list

module Parser = struct
  let rec parse (tokens : Lexer.t list) =
    match tokens with
    | True :: rest -> Ok (Bool true, rest)
    | False :: rest -> Ok (Bool false, rest)
    | Null :: rest -> Ok (Null, rest)
    | Number n :: rest -> Ok (Number n, rest)
    | String s :: rest -> Ok (String s, rest)
    | LBracket :: rest -> parse_array rest
    | LBrace :: rest -> parse_object rest
    | [] -> Error "No tokens"
    | _ -> Error "Unexpected token"

  and parse_array tokens =
    let rec loop acc tokens =
      match tokens, acc with
      | Lexer.RBracket :: rest, _ -> Ok (Array (List.rev acc), rest)
      | tokens, []
      | Comma :: tokens, _ :: _ ->
        (match parse tokens with
        | Ok (element, rest) -> loop (element :: acc) rest
        | Error _ as err  -> err)
      | _ -> Error "Unexpected token"
    in loop [] tokens

  and parse_object tokens =
    let rec loop acc tokens =
      match tokens, acc with
      | Lexer.RBrace :: rest, _ -> Ok (Object (List.rev acc), rest)
      | String key :: Colon :: rest, []
      | Comma :: String key :: Colon :: rest, _ :: _ ->
        (match parse rest with
        | Ok (value, rest) -> loop ((key, value) :: acc) rest
        | Error _ as err -> err)
      | _ -> Error "Unexpected token"
    in loop [] tokens
end

let parse stream =
  let (>>=) = Result.bind in
  Lexer.lex stream >>= fun tokens ->
  Parser.parse tokens >>= fun (json, tokens) -> Ok json

let parse_string s = parse (Stream.of_string s)
