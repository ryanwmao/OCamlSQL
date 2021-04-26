open Readcsv
open Command_parser

let read_csv file = 
  print_endline
    "Please enter the name of the file you want to output to.\n";
    match read_line () with 
    | exception a -> ()
    | file_name -> print_csv (from_csv file) file_name ""
  (*Readcsv.from_csv file;
  print_endline
    "Please enter the name of the file you want to output to.\n";
    match read_line () with 
    | exception End_of_file -> ()
    | file_name -> Readcsv.print_csv (Readcsv.from_csv file) file_name*)

let read_command =   
  print_endline
  "Please enter the command you want to parse. We will print it to the terminal. \n";
  match read_line () with 
  | exception a -> ()
  | com -> 
    let q_tokens = Command_parser.tokenizer com in 
    Command_parser.print_query_tokens q_tokens

let main () = 
  ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to OCamlSQL.\n";
  print_endline
    "Please enter the name of the file you want to load.\n";
    match read_line () with 
    | exception End_of_file -> ()
    | file_name -> read_csv file_name

let () = main ()
