open Ast
open Table
open Database

let rec eval_tables_helper tbls db = match tbls with 
  | Table s -> let tbl = Database.get_table s db in (tbl, Table.make_c_orig tbl s)
  | InnerJoin (t1, t2) -> 
    let (t1, c1) = eval_tables_helper t1 db in 
    let (t2, c2) = eval_tables_helper t2 db in 
    Table.inner_join t1 c2 t2 c2
  | OuterJoin (t1, t2) -> failwith "Outer join not implemented! sorry kiddo"
  | LeftJoin (t1, t2) -> failwith "Left join not implemented! sorry kiddo"
  | RightJoin (t1, t2) -> failwith "Right join implemented! sorry kiddo"

let eval_tables tbls db = 
  let (t, c) = eval_tables_helper tbls db in 
  Table.rename t c; t

let evaluate_query (sel, tables, where, group, order) db = failwith "TODO"

let get_name name name_tracker = 
  let count = try Hashtbl.find name_tracker name with Not_found -> 0 in
  let _ = Hashtbl.add name_tracker name (count + 1) in
  if count = 0 then name else name ^ ":" ^ (string_of_int count)

let int_to_col i length = 
  Table.convert_to_c (Array.make length (string_of_int i))

let float_to_col i length = 
  Table.convert_to_c (Array.make length (string_of_float i))

let string_to_col s length = 
  Table.convert_to_c (Array.make length s)

let bool_to_col b length = 
  Table.convert_to_c (Array.make length (string_of_bool b))

let rec evaluate_expr expr table = 
  let length = Table.length_of_t table in
  match expr with 
  | Boolean b -> bool_to_col b length 
  | Integer i -> int_to_col i length
  | Float f -> float_to_col f length
  | Column c -> Table.column table c
  | TableAndColumn (t, c) -> 
    let tc = t ^ "." ^ c in 
    (try Table.column table tc with Column_not_found -> Table.column table c)
  | Str s -> (try Table.column table s with Column_not_found -> string_to_col s length)
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2 table
  | Not b -> let b = evaluate_expr b table in Table.not_fn b
  | Function (fn_name, e) -> failwith "unimplemented"

and eval_bop bop e1 e2 table = 
  let v1 = evaluate_expr e1 table in 
  let v2 = evaluate_expr e2 table in 
  let cba = Table.col_of_bool_array in
  match bop with 
  | AND -> v1 &&: v2
  | OR -> v1 ||: v2
  | Add -> v1 +: v2 
  | Sub -> v1 -: v2 
  | Mult -> v1 *: v2 
  | Div -> v1 /: v2 
  | Mod -> v1 %: v2 
  | EQ -> cba (v1 =: v2)
  | NEQ -> cba ((!=:) v1 v2)
  | GT -> cba (v1 >: v2)
  | LT -> cba (v1 <: v2)
  | GEQ -> cba (v1 >=: v2)
  | LEQ -> cba(v1 <=: v2)
