(** Evaluation of queries *)

open Ast
open Database
open Table

(** Evaluates a query and returns the result as a table *)
val evaluate_query : Ast.query -> Database.k -> Table.t

(** Executes a query and returns the result as a table *)
val execute_query : string -> Database.k -> Table.t
