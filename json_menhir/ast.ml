type json =
  | Number of float
  | Bool of bool
  | Null
  | String of string
  | Array of json list
  | Object of (string * json) list

let rec show json =
  match json with
  | Bool true -> "true"
  | Bool false -> "false"
  | Null -> "null"
  | Number n -> string_of_float n
  | String s -> Printf.sprintf "%S" s
  | Array xs -> show_array xs
  | Object xs -> show_object xs
and show_array xs =
  let body = xs |> List.map show |> String.concat ", " in
  "[ " ^ body ^ " ]"
and show_object xs =
  let body = xs |> List.map (fun (k, v) -> "\"" ^ k ^ "\"" ^ ": " ^ show v) |> String.concat ", " in
  "{ " ^ body ^ " }"
