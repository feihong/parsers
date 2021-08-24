{
open Parser
}

let space = [' ' '\n' '\r' '\t']+
let digit = ['0'-'9']
let number = '-'? digit+
let escape_sequence = '\\' _
let string = '"' ([^ '"']* as body) '"'

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
