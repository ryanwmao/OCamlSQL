open Readcsv
open Command_parser


let load_table name file = 
  let table = from_csv file in 
  print_csv table name

(*let rec clear_workspace = 
  match Sys.is_directory ("../workspace/") with
  | true ->
    Sys.readdir ("../workspace/") |> Array.iter 
        (fun name -> Unix.rmdir (Filename.concat ("../workspace/") name));
  | false -> ()*)

exception Table_not_found
exception Select_not_found

let print_array arr outputfile = 
  let oc = open_out_gen [ Open_append] 0o640 outputfile in 
  Array.iter (fun x -> Printf.fprintf oc " %s," x) arr;
  Printf.fprintf oc "\n" 

let rec find tables name i = 
  try 
    column tables.(i) name
  with  
    Invalid_argument n -> find tables name (i+1)


let rec print_selects from oc selects = match selects with
  | [] -> ()
  | h :: t -> print_array (find from h 0) oc (*try print_array (find from h 0) oc; print_selects from oc t
      with Invalid_argument n -> raise Select_not_found *)

let execute_command q_tokens outputfile = 
  let from = (match Command_parser.from_tokens q_tokens with
  | [] -> failwith "no from "
  | h :: t -> h) in
  let from2 = Readcsv.from_csv from in
  let oc = open_out_gen [Open_creat] 0o640 outputfile in 
  let selects = Command_parser.select_tokens q_tokens in
    print_selects [|from2|] outputfile selects;
  close_out_noerr oc

let read_command com name =   
  let q_tokens = Command_parser.tokenizer com in 
  execute_command q_tokens name