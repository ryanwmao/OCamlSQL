(** Represents a database of tables *)

open Table

type t

(** Creates a database out of a list of tables *)
val make_database : string list -> Table.t list -> t

(** Executes a query on a table, returning the result as a table *)
val execute_query : string -> t -> Table.t

(** Gets a table from the database *)
val get_table : string -> t -> Table.t
