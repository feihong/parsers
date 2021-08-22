#use "opal.ml"
#mod_use "stream.ml"

type json =
  | Number of float
  | Bool of bool
  | String of string
  | Array of json list
  | Object of (string * json) list

let exactly_not x = satisfy ((<>) x)

let exactly_s s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  loop s 0

let number =
  let dot_or_digit = exactly '.' <|> digit in
  let* first = exactly '-' <|> dot_or_digit in
  let* res = many dot_or_digit in
  match first :: res |> implode |> float_of_string_opt with
  | None -> mzero
  | Some x -> return (Number x)

let boolean =
  let true' = exactly_s "true" >> return (Bool true) in
  let false' = exactly_s "false" >> return (Bool false) in
  true' <|> false'

let string_body =
  let* c = any in
  match c with
  | '"' -> mzero (* terminate string *)
  | c when c <> '\\' -> return [c]
  | c ->
    let* c2 =  any in
    return [c; c2]

let string =
  let* _ = exactly '"' in
  let* body = many string_body in
  let* _ = exactly '"' in
  let value = body |> List.flatten |> implode in
  return (String value)

let json_parser = choice [number; boolean; string]

let parse_string p s = parse p (LazyStream.of_string s)
