(* #load "Str.cma" *)
(* grammar: https://forcedotcom.github.io/phoenix/index.html#expression*)
(* Parses commands into Ast expressions and queries *)
open Ast

(* Parses string [s] into an Ast expression *)
let parse_expr s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expressions Lexer.read lexbuf in
  ast

(* Parses string [s] into an Ast query *)
let parse_query s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.query Lexer.read lexbuf in
  ast

(* Example query *)
let test_query =
  "SELECT A, B, C * D, \"C FROM D\" FROM table1    WHERE A.a = B.b AND \
   C.c = D.d GROUP BY C * 2 ORDER BY C"
