#use "opal.ml"
#mod_use "stream.ml"

type t =
  | Number of float
  | Bool of bool
  | String of string
  | Array of t list
  | Object of (string * t) list

let exactly_not x = satisfy ((<>) x)

let exactly_s s =
  let rec loop s i =
    if i >= String.length s
    then return s
    else exactly s.[i] >> loop s (i + 1)
  in
  loop s 0

let whole_number =
  let decimal_part = exactly '.' >>= fun _ -> many digit => implode in
  let* whole_part = many1 digit => implode in
  let* decimal_part = option "" (decimal_part) in
  return (whole_part ^ "." ^ decimal_part)

let partial_number =
  let* _ = exactly '.' in
  let* decimal_part = many1 digit => implode in
  return ("0." ^ decimal_part)

let number =
  let* negative_part = option "" (exactly '-' >> return "-") in
  let* number = whole_number <|> partial_number in
  let value = negative_part ^ number |> float_of_string in
  return (Number value)

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
    let* c2 = any in
    return [c; c2]

let string =
  let* _ = exactly '"' in
  let* body = many string_body in
  let* _ = exactly '"' in
  let value = body |> List.flatten |> implode in
  return (String value)

let json = choice [lexeme number; lexeme boolean; lexeme string]

let parse_string p s = parse p (LazyStream.of_string s)
