{
open Parser
}

let space = [' ' '\n' '\r' '\t']+
let digit = ['0'-'9']
let whole_number = digit+ ('.' digit*)?
let partial_number = '.' digit+
let number = '-'? (whole_number | partial_number)
let string_body = '\\' _ | [^ '"']
let string = '"' (string_body* as body) '"'

rule read =
  parse
  | space { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "," { COMMA }
  | ":" { COLON }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | number { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING body }
  | eof { EOF }
