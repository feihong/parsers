type token =
  | Word of string
  | Int of int
  | String of string

let implode chars = chars |> List.map (String.make 1) |> String.concat ""

let make_string chars = String (chars |> List.rev |> implode)

let make_word_or_int chars =
  let value = chars |> List.rev |> implode in
  match int_of_string_opt value with
  | Some i -> Int i
  | None -> Word value

let is_space c = List.mem c ['\n'; '\t'; ' '; '\r']

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

let lex_word_or_int stream first_char =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> make_word_or_int acc
    | c when is_space c -> make_word_or_int acc
    | c -> loop (c :: acc)
  in loop [first_char]

let tokenize_stream stream =
  let rec loop acc =
    match Stream.next stream with
    | exception Stream.Failure -> Ok (List.rev acc)
    | c when is_space c -> loop acc
    | '"' ->
      (match lex_string stream with
      | Error _ as err -> err
      | Ok token -> loop (token :: acc))
    | c -> loop (lex_word_or_int stream c :: acc)
  in loop []

let tokenize s = tokenize_stream (Stream.of_string s)
