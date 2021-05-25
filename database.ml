open Table

module Dad = Map.Make(String)

type k = Table.t Dad.t

let empty_database () = Dad.empty

let make_database table_names tables = 
  let d = ref (empty_database ()) in 
  List.iter2 (fun a b -> d := Dad.add a b !d) table_names tables;
  !d

let get_table tblname db = 
  Dad.find tblname db 

let execute_query query db = 
  let query = Command_parser.parse_query query in 
  Eval.evaluate_query query db

