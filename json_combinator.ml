#use "opal.ml"
#mod_use "stream.ml"

type t =
  | Number of float
  | Bool of bool
  | String of string
  | Array of t list
  | Object of (string * t) list

let exactly_s s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  loop s 0

let whole_number =
  let* whole_part = many1 digit => implode in
  let* decimal_part = option "" (exactly '.' >> many digit => implode) in
  return (whole_part ^ "." ^ decimal_part)

let partial_number = exactly '.' >> many1 digit => fun x -> "0." ^ implode x

let number =
  let* negative_part = option "" (exactly '-' >> return "-") in
  let* number = whole_number <|> partial_number in
  let value = float_of_string (negative_part ^ number) in
  return (Number value)

let boolean =
  let true' = exactly_s "true" >> return (Bool true) in
  let false' = exactly_s "false" >> return (Bool false) in
  true' <|> false'

let string_body =
  any >>= function
  | '"' -> mzero (* terminate string *)
  | '\\' -> any >>= fun c2 -> return ['\\'; c2]
  | c -> return [c]

let pure_string =
  let* _ = exactly '"' in
  let* body = many string_body in
  let* _ = exactly '"' in
  return (body |> List.flatten |> implode)

let string = pure_string => fun s -> String s

let comma = lexeme (exactly ',')

let rec whole input = (json >>= fun x -> lexeme (eof x)) input

and json input = lexeme (choice [number; boolean; string; array; object_]) input

and array input =
  let aux =
    let* _ = exactly '[' in
    let* elements = sep_by json comma in
    let* _ = lexeme (exactly ']') in
    return (Array elements)
  in aux input

and object_ input =
  let key_value =
    let* key = pure_string in
    let* _ = lexeme (exactly ':') in
    let* value = json in
    return (key, value)
  in
  let aux =
    let* _ = exactly '{' in
    let* pairs = sep_by (lexeme key_value) comma in
    let* _ = lexeme (exactly '}') in
    return (Object pairs)
  in aux input

let parse_string p s = parse p (LazyStream.of_string s)
