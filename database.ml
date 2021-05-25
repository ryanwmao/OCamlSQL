open Table
module Dad = Map.Make (String)

type k = Table.t Dad.t

let empty_database () = Dad.empty

let make_database table_names tables =
  let d = ref (empty_database ()) in
  List.iter2 (fun a b -> d := Dad.add a b !d) table_names tables;
  !d

let print_db db = Dad.iter (fun key value -> print_string key) db

let get_table tblname db = Dad.find tblname db
