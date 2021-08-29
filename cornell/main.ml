module Ast = Main__Ast

let parse (s: string) : Ast.expr =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let revised_lexer = Lexer.lexer lexbuf in
  MenhirLib.Convert.Simplified.traditional2revised Parser.prog revised_lexer

