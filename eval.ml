open Ast
open Table

let rec eval_tables_helper tbls db =
  match tbls with
  | Table s ->
      let tbl = Database.get_table s db in
      (tbl, Table.make_c_orig tbl s)
  | InnerJoin (t1, t2) ->
      let t1, c1 = eval_tables_helper t1 db in
      let t2, c2 = eval_tables_helper t2 db in
      Table.inner_join t1 c2 t2 c2
  | OuterJoin (t1, t2) -> failwith "Outer join not implemented! sorry "
  | LeftJoin (t1, t2) -> failwith "Left join not implemented! sorry "
  | RightJoin (t1, t2) -> failwith "Right join implemented! sorry "

let eval_tables tbls db =
  let t, c = eval_tables_helper tbls db in
  Table.rename t c;
  t

let get_name name name_tracker =
  let count =
    try Hashtbl.find name_tracker name with Not_found -> 0
  in
  let _ = Hashtbl.add name_tracker name (count + 1) in
  if count = 0 then name else name ^ ":" ^ string_of_int count

let int_to_col i length =
  Table.convert_to_c (Array.make length (string_of_int i))

let float_to_col i length =
  Table.convert_to_c (Array.make length (string_of_float i))

let string_to_col s length = Table.convert_to_c (Array.make length s)

let bool_to_col b length =
  Table.convert_to_c (Array.make length (string_of_bool b))

let rec evaluate_expr expr table =
  let length = Table.length_of_t table in
  match expr with
  | Boolean b -> bool_to_col b length
  | Integer i -> int_to_col i length
  | Float f -> float_to_col f length
  | Column c -> Table.column table c
  | TableAndColumn (t, c) -> (
      let tc = t ^ "." ^ c in
      try Table.column table tc
      with Column_not_found -> Table.column table c)
  | Str s -> (
      try Table.column table s
      with Column_not_found -> string_to_col s length)
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2 table
  | Not b ->
      let b = evaluate_expr b table in
      Table.not_fn b
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
  | NEQ -> cba (( !=: ) v1 v2)
  | GT -> cba (v1 >: v2)
  | LT -> cba (v1 <: v2)
  | GEQ -> cba (v1 >=: v2)
  | LEQ -> cba (v1 <=: v2)

let evaluate_where_clause exprs table =
  let exprs = Ast.expressions_to_expr_list exprs in
  let exprs = List.map (fun e -> evaluate_expr e table) exprs in
  match exprs with
  | [] -> failwith "UNKNOWN"
  | h :: t -> List.fold_left (fun a b -> a &&: b) h t


let evaluate_orderby_clause exprs table = 
  let exprs = Ast.expressions_to_expr_list exprs in
  let e = match exprs with 
    | h :: t -> h
    | [] -> failwith "unknown"
  in 
  let e = evaluate_expr e table in 
  e

let apply_where opt_exprs orig_table columns =
  match opt_exprs with
  | Some exprs ->
      let c_bool = evaluate_where_clause exprs orig_table in
      let c_bool = Table.bool_array_of_col c_bool in
      List.map (Table.where_col_filter c_bool) columns
  | None -> columns

let apply_orderby opt_exprs table = 
  match opt_exprs with 
  | Some (AS exprs) ->
    let c = evaluate_orderby_clause exprs table in 
    Table.order_by true table c
  | Some (DE exprs) ->
    let c = evaluate_orderby_clause exprs table in 
    Table.order_by false table c
  | None -> table

let evaluate_query (sel, tables, where, group, order) db =
  let tbl = eval_tables tables db in
  let sel_exprs = Ast.expressions_to_expr_list sel in
  let res_cols = List.map (fun e -> evaluate_expr e tbl) sel_exprs in
  let res_cols = List.map Table.copy_col res_cols in
  let col_names = List.map (fun e -> Ast.string_of_expr e) sel_exprs in
  let _ =
    List.map2
      (fun col name -> Table.as_name col name)
      res_cols col_names
  in
  let res_cols = apply_where where tbl res_cols in
  let table_result = Table.t_of_columns res_cols in
  let table_result = apply_orderby order table_result in 
  table_result

let execute_query query db = 
  let query = Command_parser.parse_query query in 
  evaluate_query query db


