open Ast
open Table
open Database

let evaluate_query q db = 
  failwith "TODO"

let get_name name name_tracker = 
  let count = try Hashtbl.find name_tracker name with Not_found -> 0 in
  let _ = Hashtbl.add name_tracker name (count + 1) in
  if count = 0 then name else name ^ ":" ^ (string_of_int count)

let int_to_col i length name_tracker = 
  let res = Array.make length (string_of_int i) in
  let name = get_name (string_of_int i) name_tracker in
  Array.set res 0 name; Table.convert_to_c res

let float_to_col i length name_tracker = 
  let res = Array.make length (string_of_float i) in
  let name = get_name (string_of_float i) name_tracker in
  Array.set res 0 name; Table.convert_to_c res

let string_to_col s length name_tracker = 
  let res = Array.make length s in
  let name = get_name s name_tracker in
  Array.set res 0 name; Table.convert_to_c res

let bool_to_col b length name_tracker = 
  let res = Array.make length (string_of_bool b) in
  let name = get_name (string_of_bool b) name_tracker in
  Array.set res 0 name; Table.convert_to_c res

let evaluate_expr expr table name_tracker = 
  let length = 100 in
  match expr with 
  | Boolean b -> bool_to_col b length name_tracker 
  | Integer i -> int_to_col i length name_tracker
  | Float f -> float_to_col f length name_tracker
  | Column c -> Table.column c table
  | Str s -> try Table.column s table with Column_not_found -> string_to_col s length name_tracker
  | Binop of bop * expr * expr
  | Not of expr
  | Function of string * expressions
