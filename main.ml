open Readcsv

let read_csv file = 
  print_endline
    "Please enter the name of the file you want to output to.\n";
    match read_line () with 
    | exception a -> ()
    | file_name -> print_csv (from_csv file) file_name
  (*Readcsv.from_csv file;
  print_endline
    "Please enter the name of the file you want to output to.\n";
    match read_line () with 
    | exception End_of_file -> ()
    | file_name -> Readcsv.print_csv (Readcsv.from_csv file) file_name*)

let main () = 
  ANSITerminal.print_string [ ANSITerminal.red ]
  "\n\nWelcome to OCamlSQL.\n";
  print_endline
    "Please enter the name of the file you want to load.\n";
    match read_line () with 
    | exception End_of_file -> ()
    | file_name -> read_csv file_name

let () = main ()