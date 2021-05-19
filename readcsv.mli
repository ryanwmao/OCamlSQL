(** Read a CSV file into usable data

    Handles reading a CSV file, and *)

val readcsv : string -> (int, string array) Hashtbl.t

val readstring : string -> (int, string array) Hashtbl.t
