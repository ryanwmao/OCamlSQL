(* #load "Str.cma" *)
(* grammar: https://forcedotcom.github.io/phoenix/index.html#expression*)
open Ast
let parse_expr s = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.expressions Lexer.read lexbuf in 
  ast

let parse_query s = 
  let lexbuf = Lexing.from_string s in 
  let ast = Parser.query Lexer.read lexbuf in 
  ast

let test_query = "SELECT A, B, C * D, \"C FROM D\" FROM table1    WHERE A.a = B.b AND C.c = D.d GROUP BY C * 2 ORDER BY C"
