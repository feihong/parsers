type json =
  | Number of float
  | Bool of bool
  | String of string
  | Array of json list
  | Object of (string * json) list
