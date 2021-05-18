(* open Readcsv open Table open Command_parser

   let exec_command com name = Workspace.read_command com name let
   read_command name = print_endline "Please enter the command. \n";
   match read_line () with | exception a -> () | com -> exec_command com
   name

   let read_csv file = print_csv (from_csv file) "demo.txt";
   read_command "demo.txt"

   let main () = ANSITerminal.print_string [ ANSITerminal.red ]
   "\n\nWelcome to OCamlSQL.\n"; print_endline "Please enter the
   command.\n"; match read_line () with | exception End_of_file -> () |
   com -> exec_command com "demo.txt"

   let () = main () *)
