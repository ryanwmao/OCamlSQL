(** Parses commands into Ast expressions and queries *)
open Ast

(** Parses a command into an Ast expression *)
val parse_expr : string -> Ast.expressions

(** Parses a command into an Ast query *)
val parse_query : string -> Ast.query
