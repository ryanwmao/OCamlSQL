open Table

module Database = Map.Make(String)

type t = Table.t Database.t

let empty_database () = Database.empty

let make_database table_names tables = 
  let d = ref (empty_database ()) in 
  List.iter2 (fun a b -> d := Database.add a b !d) table_names tables;
  !d

let get_table tblname db = 
  Database.find tblname db 

let execute_query query db = 
  let query = Command_parser.parse_query query in 
  Eval.evaluate_query query db

