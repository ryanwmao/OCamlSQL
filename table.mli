(* idk *)

(** Abstract type of values representing the database data *)
type t

(** [from_csv s] is the CSV data at the file in path [s]. Requires: [s]
    is a valid path name to a CSV file *)
val from_csv : string -> t

(* Print a database table to a local file *)
val export_csv : t -> string -> unit

(* An empty database table *)
val empty : t

(* SELECT: Goal is to have 1 function here to call select and return a
   [t] *)
