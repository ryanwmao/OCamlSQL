open Readcsv

(* Representative type of a database table *)
type t = { mutable lines : (int, string array) Hashtbl.t }

type c = string array

type c_orig = (int, string) Hashtbl.t

(* uses Readcsv.from_csv to read in a database table from a CSV file *)
let from_csv file = { lines = readcsv file }

(* NOT SURE IF NEEDED *)
let read_workspace name = { lines = readcsv (*"../workspace/"^*) name }

(** Returns length of table [t] *)
let length_of_t t = Hashtbl.length t.lines

(** An empty database table *)
let empty = { lines = Hashtbl.create 0 }

exception Column_not_found

exception Length_mismatch

let convert_to_c (arr : string array) : c = arr

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

(* Returns a boolean list that contains true in the indices where
   [bool_lst1] and [bool_lst2] both contain true, contains false
   otherwise *)
let rec where_and_helper acc i bool_lst1 bool_lst2 =
  match bool_lst1 with
  | [] -> Array.of_list (List.rev acc)
  | h :: t ->
      if h && List.nth bool_lst2 i then
        where_and_helper (true :: acc) (i + 1) t (List.tl bool_lst2)
      else where_and_helper (false :: acc) (i + 1) t (List.tl bool_lst2)

(* Given database columns [col1] and [col2], and list of booleans
   [bool_lst1] and [bool_lst2], returns a list of columns with entries
   corresponding to the overlapping true entries in the two boolean
   lists *)
let where_and bool_lst1 bool_lst2 col1 col2 =
  let bool_lst =
    where_and_helper [] 0
      (Array.to_list bool_lst1)
      (Array.to_list bool_lst2)
  in
  [ where_col_filter bool_lst col1; where_col_filter bool_lst col2 ]

(* Given database columns [col1] and [col2], and list of booleans
   [bool_lst1] and [bool_lst2], returns a list of columns with entries
   corresponding to the true entries in the boolean lists *)
let where_or bool_lst1 bool_lst2 col1 col2 =
  [ where_col_filter bool_lst1 col1; where_col_filter bool_lst2 col2 ]

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
  {
    lines =
      (let table = Hashtbl.create 15 in
       List.iteri
         (fun i a -> Hashtbl.replace table (i + 1) a)
         str_arr_lst;
       (*change back to i*)
       table);
  }

(* Given a [col1] database column, binary operation [bop], and [col2]
   database column, returns a column of values of [bop col1 col2] (at
   the same index) *)
let rec eval_bop_int col1 bop col2 =
  let return = ref [||] in
  Array.iteri
    (fun i a -> if i = 0 then () else
      return :=
        Array.append !return
          [|
            string_of_int
              (bop
                 (int_of_string (Array.get col1 i))
                 (int_of_string (Array.get col2 i)));
          |])
    col1;
  !return

(* Binary operation evaluation for floats *)
let rec eval_bop_float col1 bop col2 =
  let return = ref [||] in
  Array.iteri
    (fun i a -> if i = 0 then () else
      return :=
        Array.append !return
          [|
            string_of_float
              (bop
                 (float_of_string (Array.get col1 i))
                 (float_of_string (Array.get col2 i)));
          |])
    col1;
  !return

(* Binary operation evaluation for booleans *)
let rec eval_bop_bool col1 bop col2 =
  let return = ref [||] in
  Array.iteri
    (fun i a -> if i = 0 then () else
      return :=
        Array.append !return
          [|
            string_of_bool
              (bop
                 (bool_of_string (Array.get col1 i))
                 (bool_of_string (Array.get col2 i)));
          |])
    col1;
  !return

(* Add (float) operator for columns *)
let ( +.: ) col1 col2 = eval_bop_float col1 ( +. ) col2

(* Subtract (float) operator for columns *)
let ( -.: ) col1 col2 = eval_bop_float col1 ( -. ) col2

(* Multiply (float) operator for columns *)
let ( *.: ) col1 col2 = eval_bop_float col1 ( *. ) col2

(* Divide (float) operator for columns *)
let ( /.: ) col1 col2 = eval_bop_float col1 ( /. ) col2

(* Add operator for columns *)
let ( +: ) col1 col2 = 
  try eval_bop_int col1 ( + ) col2 
  with Failure _ -> eval_bop_float col1 ( +. ) col2

(* Subtract operator for columns *)
let ( -: ) col1 col2 = 
  try eval_bop_int col1 ( - ) col2 
  with Failure _ -> eval_bop_float col1 ( -. ) col2

(* Multiply operator for columns *)
let ( *: ) col1 col2 = 
  try eval_bop_int col1 ( * ) col2 
  with Failure _ -> eval_bop_float col1 ( *. ) col2

(* Divide operator for columns *)
let ( /: ) col1 col2 = 
  try eval_bop_int col1 ( / ) col2 
  with Failure _ -> eval_bop_float col1 ( /. ) col2

(* AND operator for columns *)
let ( &&: ) col1 col2 = 
  eval_bop_bool col1 ( && ) col2

(* OR operator for columns *)
let ( ||: ) col1 col2 = 
  eval_bop_bool col1 ( || ) col2

(* NOT function for columns *)
let not_fn col1 = 
  Array.mapi (fun i x -> if i > 0 then x |> bool_of_string |> not |> string_of_bool else x) col1

(* Mod operator for columns *)
let ( %: ) col1 col2 = 
  try eval_bop_int col1 ( mod ) col2 
  with Failure _ -> 
    let f = (fun a b -> float_of_int ((int_of_float a) mod (int_of_float b))) 
    in
    eval_bop_float col1 f col2

(* apply fx to col1 *)
let rec function_of_float col1 fx =
  let return = ref [||] in
  Array.iteri
    (fun i a ->
      return :=
        Array.append !return
          [|
            string_of_float (fx (float_of_string (Array.get col1 i)));
          |])
    col1;
  !return

(* apply fx to col1 *)
let rec function_of_int col1 fx =
  let return = ref [||] in
  Array.iteri
    (fun i a ->
      return :=
        Array.append !return
          [| string_of_int (fx (int_of_string (Array.get col1 i))) |])
    col1;
  !return

(* Comparator used by order_by statements depending on if [asc] is
   ascending (true) or descending (false)*)
let compare asc s1 s2 =
  if asc then if s1 = s2 then 0 else if s1 > s2 then 1 else -1
  else if s1 = s2 then 0
  else if s1 > s2 then -1
  else 1

(* Applies the comparator to the specified [col_name] by the order_by
   statement for the given table [tbl] and depending on if [asc] is
   ascending (true) or descending (false) *)
let order_by_column asc tbl col_name =
  let col = search tbl col_name 1 in
  let col1 = Array.sub col 1 (Array.length col - 1) in
  Array.sort (compare asc) col1;
  Array.append (Array.sub col 0 1) col1

(* Creates an initial list of pairs consisting of the column position
   and value of the specified column [col] in an order_by statement *)
let rec create_pos_list i acc col =
  match col with
  | [] -> List.rev acc
  | h :: t -> create_pos_list (i + 1) ((h, i) :: acc) t

(* Reorders the initial list of pairs between initial positions and
   values [pos] in the array [arr] based on how order_by affects the
   order and returns the changed list of positions *)
let rec reorder_pos_list acc pos arr =
  match arr with
  | [] -> (
      match List.split acc with [], [] -> failwith "" | a, b -> a)
  | h :: t -> reorder_pos_list ((List.assoc h pos, h) :: acc) pos t

(* Creates a list of positions representing where values in columns are
   moved to after order_by is applied to the specified column [arr] in
   the table [tbl]*)
let position_list tbl arr =
  let col = search tbl (Array.get arr 0) 1 in
  let pos_lst = create_pos_list 0 [] (Array.to_list col) in
  reorder_pos_list [] pos_lst (Array.to_list arr)

(* Creates a list of arrays from the given table [tbl] *)
let rec create_tbl_list i acc tbl =
  if i = Hashtbl.length tbl.lines + 1 then acc
  else create_tbl_list (i + 1) (Hashtbl.find tbl.lines i :: acc) tbl

(* Returns an array of the values in [arr] after being modified using
   the position map specified by [pos_lst] *)
let rec change_order acc arr pos_lst =
  match pos_lst with
  | [] -> Array.of_list acc
  | h :: t -> change_order (List.assoc h arr :: acc) arr t

(* Swaps the values in the list of pairs [lst] *)
let rec swap_list lst acc =
  match lst with
  | [] -> acc
  | (a, b) :: t -> swap_list t ((b, a) :: acc)

(* Reorders all of the arrays in [tbl_list] according to the list of
   position mappings [pos_lst and returns a list of the modified arrays] *)
let rec reorder_tbl acc tbl_list pos_lst =
  match tbl_list with
  | [] -> acc
  | h :: t ->
      reorder_tbl
        (change_order []
           (swap_list (create_pos_list 0 [] (Array.to_list h)) [])
           pos_lst
         :: acc)
        t pos_lst

(* Applies the order_by sortings to the given table [tbl] given the
   specified column name [col_name] and whether or not the order is
   ascending or descending [asc] *)
let order_by asc tbl col_name =
  let pos_lst = position_list tbl (order_by_column asc tbl col_name) in
  t_of_columns (reorder_tbl [] (create_tbl_list 1 [] tbl) pos_lst)

(* helper function for group_by *)
let bins_of_col tbl col_name =
  let bins = Hashtbl.create 0 in
  Array.iteri (fun i a -> Hashtbl.add bins a i) (column tbl col_name);
  bins

(* Takes in a table, column name, aggregate function, and bin groupings,
   returns the column with the aggregate function applied to those
   groupings *)
let group_aggregate tbl col_name fx bins =
  let col = column tbl col_name in
  let new_col = Hashtbl.create 10 in
  Hashtbl.iter
    (fun k v ->
      if Hashtbl.mem new_col k then
        Hashtbl.replace new_col k (fx (Hashtbl.find new_col k) col.(v))
      else Hashtbl.add new_col k (fx "" col.(v)))
    bins;
  let str_array = ref [||] in
  Hashtbl.iter
    (fun k v -> str_array := Array.append !str_array [| v |])
    new_col;
  !str_array

(* Takes in a table, column name, and groupings, and applies the
   groupings *)
let group_no_aggregate tbl col_name bins =
  let col = column tbl col_name in
  let new_col = Hashtbl.create 10 in
  Hashtbl.iter
    (fun k v ->
      if not (Hashtbl.mem new_col k) then Hashtbl.add new_col k col.(v))
    bins;
  let str_array = ref [||] in
  Hashtbl.iter
    (fun k v -> str_array := Array.append !str_array [| v |])
    new_col;
  !str_array

let col_of_bool_array b_array = 
  Array.map (fun x -> string_of_bool x) b_array

let bool_array_of_col col = 
  Array.map (fun x -> bool_of_string x) col


(* [inner_join tbl1 col_orig1 tbl2 col_orig2] is tbl1 inner joined with tbl2.
    col_orig1 describes which tables that the columns in tbl1 are originally from.
    col_orig2 is similar. *)
let inner_join tbl1 c_orig1 tbl2 c_orig = 
  failwith "unimplemented"

(* [rename tbl c_orig] renames tbl as necessary to remove duplicate column names.
 It does the renaming in place. *)
let rename tbl c_orig = 
  failwith "unimplemented"

(* count aggregate function *)
let count p n =
  if p = "" then "0" else p |> int_of_string |> ( + ) 1 |> string_of_int

(* sum aggregate function for int *)
let sum_int a b =
  a |> int_of_string |> ( + ) (int_of_string b) |> string_of_int

(* sum aggregate function for float *)
let sum_float a b =
  a |> float_of_string |> ( +. ) (float_of_string b) |> string_of_float

(* min aggregate function *)
let min p n = if p < n then p else n

(* max aggregate function *)
let max p n = if p > n then p else n
