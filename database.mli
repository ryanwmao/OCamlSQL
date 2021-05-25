(** Represents a database of tables *)

open Table

type k

(** Creates a database out of a list of tables *)
val make_database : string list -> Table.t list -> k

(** Executes a query on a table, returning the result as a table *)
val execute_query : string -> k -> Table.t

(** Gets a table from the database *)
val get_table : string -> k -> Table.t
