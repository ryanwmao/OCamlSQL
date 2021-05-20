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

let export_string t =
  let str = ref "" in
  for i = 1 to Hashtbl.length t.lines do
    let temp =
      Array.fold_left
        (fun x -> ( ^ ) (x ^ ", "))
        ""
        (Hashtbl.find t.lines i)
    in
    let line = String.sub temp 2 (String.length temp - 2) in
    str := !str ^ "\n" ^ line
  done;
  !str

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

let select tbl_lst col_lst = multi_search tbl_lst col_lst []

(* Given a [bool_lst] of booleans and a [col] database column, returns a
   column with entries corresponding to the true entries in [bool_lst].
   REQUIRES length bool_lst = length col *)
let rec where_col_filter bool_lst col =
  let return = ref [||] in
  Array.iteri
    (fun i a ->
      if Array.get bool_lst i then
        return := Array.append !return [| Array.get col i |])
    col;
  !return

(* Given a [col1] database column, relation [rel], and [col2] database
   column, returns a column of booleans where true entries match a true
   evaluation of [rel] between [col1] and [col2] (at the same index) *)
let rec eval_col_condition col1 rel col2 =
  let return = ref [||] in
  Array.iteri
    (fun i a ->
      if rel (Array.get col1 i) (Array.get col2 i) then
        return := Array.append !return [| true |]
      else return := Array.append !return [| false |])
    col1;
  !return

(* Takes an integer [i] and converts it to a column of same length as
   [col] *)
let col_of_int col i =
  let length = Array.length col in
  Array.make length (string_of_int i)

(* Less than relation for columns *)
let ( <: ) col1 col2 = eval_col_condition col1 ( < ) col2

(* Greater than relation for columns *)
let ( >: ) col1 col2 = eval_col_condition col1 ( > ) col2

(* Less than or equal to relation for columns *)
let ( <=: ) col1 col2 = eval_col_condition col1 ( <= ) col2

(* Greater than or equal to relation for columns *)
let ( >=: ) col1 col2 = eval_col_condition col1 ( >= ) col2

(* Equal relation for columns *)
let ( =: ) col1 col2 = eval_col_condition col1 ( = ) col2

(* Not equal relation for columns *)
let ( !=: ) col1 col2 = eval_col_condition col1 ( <> ) col2

(* Takes a list of columns (including all columnn data) and transforms
   into a type [t] *)
let t_of_columns str_arr_lst =
  let table = Hashtbl.create 15 in
  List.iteri (fun i a -> Hashtbl.replace table i a) str_arr_lst;
  table
