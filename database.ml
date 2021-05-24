open Table

module Database = Map.Make(String)

type t = Table.t Database.t

let empty_database () = Database.empty

let make_database table_names tables = 
  let d = ref (empty_database ()) in 
  List.iter2 (fun a b -> d := Database.add a b !d) table_names tables;
  !d

let execute_query query db = 
  failwith "TODO"

