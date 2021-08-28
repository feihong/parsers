module Ast = Main__Ast

let get_position ({lex_curr_p = {pos_lnum; pos_cnum; pos_bol; _}; _} : Lexing.lexbuf) =
  (pos_lnum, pos_cnum - pos_bol + 1)

let parse (s : string) : (Ast.json, string) result =
  let lexbuf = Lexing.from_string s in
  match Parser.prog Lexer.read lexbuf with
  | exception Lexer.SyntaxError mesg ->
    let line, col = get_position lexbuf in
    Error (Printf.sprintf "Syntax error at %d, %d: %s" line col mesg)
  | ast -> Ok ast
