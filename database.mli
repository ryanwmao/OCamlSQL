open Table

type t

val make_database : string list -> Table.t list -> t

val execute_query : string -> t -> Table.t

val get_table : string -> t -> Table.t
