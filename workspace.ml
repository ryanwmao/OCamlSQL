open Readcsv
open Command_parser

(*type t = {
  mutable lines: (int, string array) Hashtbl.t
} *)

let load_table name file = 
  let table = from_csv file in 
  print_csv table name

let rec clear_workspace = 
  match Sys.is_directory ("../workspace/") with
  | true ->
    Sys.readdir ("../workspace/") |> Array.iter 
        (fun name -> Unix.rmdir (Filename.concat ("../workspace/") name));
  | false -> ()

exception Table_not_found
exception Select_not_found

let print_array arr oc = 
  Array.iter (fun x -> Printf.fprintf oc " %s," x) arr;
  Printf.fprintf oc "\n" 

let rec find tables name i = 
  try 
    column tables.(i) name
  with  
    Invalid_argument n -> find tables name (i+1)


let execute_command q_tokens name = 
  let length = List.length (Command_parser.from_tokens q_tokens) in
  let tables = Array.make length empty in
  let index = ref 0 in
  let guard = ref true in
    while !guard do
    (  match Command_parser.from_tokens q_tokens with 
      | [] -> guard := false
      | h :: t -> tables.(!index) <- (read_workspace h);)
    done;

  let oc = open_out_gen [Open_creat] 0o640 ("../workspace/"^name) in 
  guard := true;
  while !guard do
    (match Command_parser.select_tokens q_tokens with 
    | [] -> guard := false
    | h :: t -> try print_array (find tables h 1) oc
                with Invalid_argument n -> raise Select_not_found) done;
  close_out_noerr oc

let read_command com =   
  let q_tokens = Command_parser.tokenizer com in 
  execute_command q_tokens