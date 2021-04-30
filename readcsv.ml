
(* int = column number, string array = elements*)
type t = {
  mutable lines: (int, string array) Hashtbl.t
}

let empty = {
  lines = Hashtbl.create 0
}

let trim_quotes str = 
  if String.length str < 3 then str
  else if String.get str 0 = '"' 
    then String.sub str 1 ((String.length str) - 2)
  else str

let quote_escape str = 
  Str.global_replace (Str.regexp "\"\"") "\"" str 

let comma str =
  Str.split (Str.regexp ",") str |> List.map String.trim

let trim str = 
  if String.length str < 1 then str 
  else 
    let sub = String.sub str ((String.length str) - 1) 1 in
    if sub = "\r" || sub = "\n" 
      then String.sub str 0 ((String.length str) - 1)
    else 
      str
  
let handle_line str = 
  str |> comma |> List.map trim_quotes |> List.map quote_escape

(*let create_table channel = 
  let line = (input_line channel) in 
    let num_cols = List.length (comma line) in
      Hashtbl.create num_cols*)
  let create_table size = 
    Hashtbl.create size

(*let init_table channel = 
  try
    let tbl = create_table channel in
    for i = 1 to (*Hashtbl.length tbl*)11 do
      Hashtbl.add tbl i [||]
    done; close_in_noerr channel; tbl
  with End_of_file -> close_in_noerr channel; 
  raise End_of_file*)

  let init_table channel = 
    let size = List.length (comma (input_line channel)) in
    try 
      let tbl = create_table size in
      for i = 1 to size do
        Hashtbl.add tbl i [||]
      done; close_in_noerr channel; tbl
    with End_of_file -> close_in_noerr channel;
    raise End_of_file

let rec parseline tbl (str_list : string list) i = 
  match str_list with 
  | [] -> ()
  | h :: t -> 
      let arr = Array.make 1 h in 
      Hashtbl.replace tbl i (Array.append (Hashtbl.find tbl i) arr); 
      parseline tbl t (i + 1)

let trim_check tbl =
    let last = Hashtbl.length tbl in 
    let arr = Array.map trim (Hashtbl.find tbl last) in
    Hashtbl.replace tbl last arr

let odd_quotes str = failwith "Unimplemented"

let readlines file = 
  let c1 = open_in file in
  let table = init_table c1 in 
  (*close out the channel*)
  let channel = open_in file in
  try 
    while true do
      let line = ref (input_line channel) in 
      let line2 = 
        (*if odd_quotes line
        then (while odd_quotes line do 
                line := !line ^ "\n" ^ input_line channel
              done; !line)
        else*) !line in
      let line_list = handle_line line2 in
      parseline table line_list 1
    done; table
  with 
    End_of_file -> close_in_noerr channel; trim_check table; table

  
let from_csv file = {
  lines = readlines file
}

let print_csv t name = 
  let oc = open_out_gen [Open_creat; Open_append] 0o640 ((*"../workspace/"^*)name) in 
  for i = 1 to Hashtbl.length t.lines do    
    Array.iter (fun x -> Printf.fprintf oc " %s," x) (Hashtbl.find t.lines i);
    Printf.fprintf oc "\n";
  done;
  close_out_noerr oc

let read_workspace name = 
  { lines = readlines ((*"../workspace/"^*)name) }

let column t name = 
  let rec search index = if Array.get (Hashtbl.find t.lines index) 0 = name 
      then Hashtbl.find t.lines index else search (index + 1) in
  let column = search 1 in column