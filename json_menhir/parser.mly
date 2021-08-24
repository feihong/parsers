%{
open Ast
%}

%token <float> NUMBER
%token <string> STRING
%token TRUE
%token FALSE
%token COLON
%token COMMA
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token EOF

%start <Ast.json> prog

%%

prog:
  | j = json; EOF { j }
  ;

json:
  | n = NUMBER { Number n }
  | s = STRING { String s }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LBRACKET; elements = separated_list(COMMA, json); RBRACKET { Array elements }
  ;
