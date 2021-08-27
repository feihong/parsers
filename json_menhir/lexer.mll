{
open Parser

exception SyntaxError of string

let next_line (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let space = [' '  '\t']+
let newline = '\n' | '\r' | "\r\n"
let digit = ['0'-'9']
let whole_number = digit+ ('.' digit*)?
let partial_number = '.' digit+
let number = '-'? (whole_number | partial_number)

rule read =
  parse
  | space { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "null" { NULL }
  | '"' { read_string (Buffer.create 20) lexbuf }
  | "," { COMMA }
  | ":" { COLON }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | number { NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_string buf =
  parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' '\''  { Buffer.add_char buf '\''; read_string buf lexbuf }
  | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
