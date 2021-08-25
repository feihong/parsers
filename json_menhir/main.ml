module Ast = Main__Ast

let parse (s : string) : Ast.json =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
