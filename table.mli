(* idk *)

(** Abstract type of values representing the database data *)
type t

(** Abstract type of values representing table columns *)
type c

(** Converts column data into the abstract type representing table
    columns*)
val convert_to_c : string array -> c

(** [from_csv s] is the CSV data at the file in path [s]. Requires: [s]
    is a valid path name to a CSV file *)
val from_csv : string -> t

(** Print a database table to a local file *)
val export_csv : t -> string -> unit

(** Converts a database table to a CSV-formatted string *)
val export_string : t -> string

(* Finds a column with name [name] in table [t], raising
   [Column_not_found] if the column does not exist within [t]. *)
val column : t -> string -> string array

(** Returns length of table [t] *)
val length_of_t : t -> int

(** An empty database table *)
val empty : t

exception Column_not_found

(** Takes a column and creates a new column of the same length, with all
    values initialized to an integer *)
val col_of_int : c -> int -> c

(** Takes a list of columns and converts it to a [t] *)
val t_of_columns : c list -> t

(** Selects columns from list of column names (string list) from a list
    of tables; returns result as a column list *)
val select : t list -> string list -> c list

(** Evaluates less than relation for 2 columns using [<] operator *)
val ( <: ) : c -> c -> bool array

(** Evaluates greater than relation for 2 columns using [>] operator *)
val ( >: ) : c -> c -> bool array

(** Evaluates greater than or equal to relation for 2 columns using [>=]
    operator *)
val ( >=: ) : c -> c -> bool array

(** Evaluates less than or equal to relation for 2 columns using [<=]
    operator *)
val ( <=: ) : c -> c -> bool array

(** Evaluates equivalence relation for 2 columns using [=] operator *)
val ( =: ) : c -> c -> bool array

(** Evaluates non-equivalence relation for 2 columns using [<>] operator *)
val ( !=: ) : c -> c -> bool array

(** Takes a bool array (result of one of the relation operators above)
    and applies it to a column, returning a column with only entries
    corresponding to "true" *)
val where_col_filter : bool array -> c -> c

val where_and : bool array -> bool array -> c -> c -> c list

val where_or : bool array -> bool array -> c -> c -> c list

(** Addition for integer columns *)
val ( +: ) : c -> c -> c

(** Subtraction for integer columns *)
val ( -: ) : c -> c -> c

(** Multiplication for integer columns *)
val ( *: ) : c -> c -> c

(** Division for integer columns *)
val ( /: ) : c -> c -> c

(** Mod for integer columns *)
val ( %: ) : c -> c -> c

(** Addition for float columns *)
val ( +.: ) : c -> c -> c

(** Subtraction for float columns *)
val ( -.: ) : c -> c -> c

(** Multiplication for float columns *)
val ( *.: ) : c -> c -> c

(** Division for float columns *)
val ( /.: ) : c -> c -> c

(** Applies a float function (ex, sin(x)) to a float column*)
val function_of_float : c -> (float -> float) -> c

(** Applies an int function to an int column*)
val function_of_int : c -> (int -> int) -> c

(** Applies the order_by sortings to the given table [tbl] given the
    specified column name [col_name] and whether or not the order is
    ascending or descending [asc] *)
val order_by : bool -> t -> string -> t

(* Performs groupings based on a column in a table, and returns the bins
   created from those groupings *)
val bins_of_col : t -> string -> (string, int) Hashtbl.t

(* Given a table, column name, aggregate function, and bin groupings,
   performs the aggregate function on the selected column and returns a
   new column *)
val group_aggregate :
  t ->
  string ->
  (string -> string -> string) ->
  (string, int) Hashtbl.t ->
  c

(* Given a table, column name, and bin groupings, performs the grouping
   and returns a new column *)
val group_no_aggregate : t -> string -> (string, int) Hashtbl.t -> c

(* Count aggregate function *)
val count : string -> 'a -> string

(* Sum aggregate function (integers) *)
val sum_int : string -> string -> string

(* Sum aggregate function (floats) *)
val sum_float : string -> string -> string

(* Min aggregate function *)
val min : 'a -> 'a -> 'a

(* Max aggregate function*)
val max : 'a -> 'a -> 'a
