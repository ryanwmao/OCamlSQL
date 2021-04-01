
type t = {
  mutable lines: (int, string array) Hashtbl.t
}

let trim_quotes str = 
  if String.length str < 3 then str
  else if String.get str 0 = '"' 
    then String.sub str 1 ((String.length str) - 2)
  else str

let quote_escape str = 
  Str.global_replace (Str.regexp "\"\"") "\"" str 

let comma str =
  Str.split (Str.regexp ",") str
  
let handle_line str = 
  str |> comma |> List.map trim_quotes |> List.map quote_escape

let create_table channel = 
  let line = (input_line channel) in 
    let num_cols = List.length (comma line) in
      Hashtbl.create num_cols

let init_table channel = 
  try
    let tbl = create_table channel in
    for i = 1 to Hashtbl.length tbl do
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

let odd_quotes str = failwith "Unimplemented"

let readlines file = 
  let c1 = open_in file in
  let table = init_table c1 in 
  let channel = open_in file in
  try 
    while true do
      let line = ref (input_line channel) in 
      let line2 = 
        if odd_quotes line
        then (while odd_quotes line do 
                line := !line ^ "\n" ^ input_line channel
              done; !line)
        else !line in
      let line_list = handle_line line2 in
      parseline table line_list 1
    done; table
  with 
    End_of_file -> close_in_noerr channel; table

let from_csv file = {
  lines = readlines file
}
