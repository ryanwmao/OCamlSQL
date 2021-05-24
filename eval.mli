open Ast
open Database
open Table

val evaluate_query : Ast.query -> Database.t -> Table.t
