(** Read a CSV file into usable data

  Handles reading a CSV file, and 
*)

(** Abstract type of values representing the CSV data *)
type t

(** [from_csv s] is the CSV data at the file in path [s]. 
    Requires: [s] is a valid path name to a CSV file *)
val from_csv : string -> t

val print_csv : t -> string -> unit

val read_workspace : string -> t

val empty: t

val column : t -> string -> string array