open Readcsv
open Table
open Command_parser

let rec table_loop table_name tables =
  match read_line () with
  | "done" ->
      print_endline "Thank you. ";
      (!table_name, !tables)
  | com ->
      let temp = String.split_on_char ' ' com in
      table_name := List.hd temp :: !table_name;
      tables := Table.from_csv (List.hd (List.tl temp)) :: !tables;
      table_loop table_name tables

let rec command_loop db =
  match read_line () with
  | "done" -> print_endline "Thank you. \n"
  | com ->
      Eval.execute_query com db |> Table.export_string |> print_endline;
      print_endline "\n";
      command_loop db

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to OCamlSQL.\n";
  print_endline
    "Please enter files to load \n\
    \ \n\
    \  (format: [tablename] [filename].[extension]).\n\
    \  Type \"done\" when finished.";
  let table_name = ref [] in
  let tables = ref [] in
  let temp_pair = table_loop table_name tables in
  let db = Database.make_database (fst temp_pair) (snd temp_pair) in
  print_endline "\nPlease enter commands. Type \"done\" when satisfied.";
  command_loop db

let () = main ()
