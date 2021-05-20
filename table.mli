(* idk *)

(** Abstract type of values representing the database data *)
type t

(* Abstrac type of values representing table columns *)
type c

(** [from_csv s] is the CSV data at the file in path [s]. Requires: [s]
    is a valid path name to a CSV file *)
val from_csv : string -> t

(* Print a database table to a local file *)
val export_csv : t -> string -> unit

(* Converts a database table to a CSV-formatted string *)
val export_string : t -> string

(* An empty database table *)
val empty : t

(* Takes a column and creates a new column of the same length, with all
   values initialized to an integer *)
val col_of_int : c -> int -> c

(* Takes a list of columns and converts it to a [t] *)
val t_of_columns : c list -> t

(* Selects columns from list of column names (string list) from a list
   of tables; returns result as a column list *)
val select : t list -> string list -> c list

(* Evaluates less than relation for 2 columns using [<] operator *)
val ( <: ) : c -> c -> bool array

(* Evaluates greater than relation for 2 columns using [>] operator *)
val ( >: ) : c -> c -> bool array

(* Evaluates greater than or equal to relation for 2 columns using [>=]
   operator *)
val ( >=: ) : c -> c -> bool array

(* Evaluates less than or equal to relation for 2 columns using [<=]
   operator *)
val ( <=: ) : c -> c -> bool array

(* Evaluates equivalence relation for 2 columns using [=] operator *)
val ( =: ) : c -> c -> bool array

(* Evaluates non-equivalence relation for 2 columns using [<>] operator *)
val ( !=: ) : c -> c -> bool array

(* Takes a bool array (result of one of the relation operators above)
   and applies it to a column, returning a column with only entries
   corresponding to "true" *)
val where_col_filter : bool array -> c -> c

(* Addition for integer columns *)
val ( +: ) : c -> c -> c

(* Subtraction for integer columns *)
val ( -: ) : c -> c -> c

(* Multiplication for integer columns *)
val ( *: ) : c -> c -> c

(* Division for integer columns *)
val ( /: ) : c -> c -> c

(* Mod for integer columns *)
val ( %: ) : c -> c -> c

(* Addition for float columns *)
val ( +.: ) : c -> c -> c

(* Subtraction for float columns *)
val ( -.: ) : c -> c -> c

(* Multiplication for float columns *)
val ( *.: ) : c -> c -> c

(* Division for float columns *)
val ( /.: ) : c -> c -> c

(* Applies a float function (ex, sin(x)) to a float column*)
val function_of_float : c -> (float -> float) -> c

(* Applies an int function to an int column*)
val function_of_int : c -> (int -> int) -> c
