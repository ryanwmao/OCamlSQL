open Readcsv

(* Representative type of a database table *)
type t = { mutable lines : (int, string array) Hashtbl.t }

(* uses Readcsv.from_csv to read in a database table from a CSV file *)
let from_csv file = { lines = readcsv file }

(* NOT SURE IF NEEDED *)
let read_workspace name = { lines = readcsv (*"../workspace/"^*) name }

(* An empty database table *)
let empty = { lines = Hashtbl.create 0 }

exception Column_not_found

exception Length_mismatch

(* Print a database table to a local file [name] *)
let export_csv t name =
  let oc =
    open_out_gen
      [ Open_creat; Open_append ]
      0o640 (*"../workspace/"^*) name
  in
  for i = 1 to Hashtbl.length t.lines do
    Array.iter
      (fun x -> Printf.fprintf oc " %s," x)
      (Hashtbl.find t.lines i);
    Printf.fprintf oc "\n"
  done;
  close_out_noerr oc

(* Given a table [t], column name [name], and starting [index],
   recursively searches for a column whose first entry (i.e., column
   name) matches [name], and returns that name. *)
let rec search t name index =
  if Array.get (Hashtbl.find t.lines index) 0 = name then
    Hashtbl.find t.lines index
  else search t name (index + 1)

(* Finds a column with name [name] in table [t], raising
   [Column_not_found] if the column does not exist within [t]. *)
let column t name =
  try search t name 1 with Not_found -> raise Column_not_found

(* Finds a column with name [name] from a list of tables [tables],
   raising [Column_not_found] if the column does not exist within
   [tables]. *)
let rec mult_table_col_search tables name =
  match tables with
  | [] -> raise Column_not_found
  | h :: t -> (
      try column h name
      with Column_not_found -> mult_table_col_search t name)

(* Finds multiple columns in [col_lst], from multiple tables in
   [tbl_lst]. [acc] is the accumulative list of selected columns. Raises
   Column_not_found if a column does not exist. *)
let rec multi_search tbl_lst col_lst acc =
  match col_lst with
  | [] -> acc
  | h :: t -> (
      try
        multi_search tbl_lst t (mult_table_col_search tbl_lst h :: acc)
      with Column_not_found -> raise Column_not_found)

(* Given a [bool_lst] of booleans and a [col] database column, returns a
   column with entries corresponding to the true entries in [bool_lst].
   Raises [Length_mismatch] if length bool_lst <> length col *)
let rec where_col_filter bool_lst col acc =
  match (bool_lst, col) with
  | [], [] -> List.rev acc
  | h1 :: t1, h2 :: t2 ->
      if h1 then where_col_filter t1 t2 (h2 :: acc)
      else where_col_filter t1 t2 acc
  | _ -> raise Length_mismatch

(* Given a [col] database column and a boolean expression cond, returns
   a column of booleans where true entries match a true evaluation of
   [cond] on the column entry (at the same index) *)
let rec eval_col_condition col cond = failwith "TODO"
