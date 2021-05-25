(** Evaluation of queries *)

open Ast
open Database
open Table

(** Evaluates a query and returns the result as a table *)
val evaluate_query : Ast.query -> Database.t -> Table.t
