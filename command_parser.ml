(* #load "Str.cma" *)
(* grammar: https://forcedotcom.github.io/phoenix/index.html#expression*)

let punc = [
  "(";
  ")";
  ";";
  ",";
  "=";
  ">";
  "<";
  "!";
];;

let quotes = [
  "\"";
  "\'";
]

let ignore = [
  " ";
  "\t";
  "\n";
  "\r"; 
]

type arithmetic_expr = 
  | Integer of int
  | Float of float
  | Add of arithmetic_expr * arithmetic_expr
  | Sub of arithmetic_expr * arithmetic_expr
  | Mult of arithmetic_expr * arithmetic_expr
  | Div of arithmetic_expr * arithmetic_expr
  | Pow of arithmetic_expr * arithmetic_expr
  | Mod of arithmetic_expr * arithmetic_expr

type conditional_expr = 
  | Bool of bool
  | AND of conditional_expr * conditional_expr
  | OR of conditional_expr * conditional_expr
  | NOT of conditional_expr * conditional_expr
  | EQ of arithmetic_expr * arithmetic_expr
  | NEQ of arithmetic_expr * arithmetic_expr
  | GT of arithmetic_expr * arithmetic_expr
  | GTE of arithmetic_expr * arithmetic_expr
  | LE of arithmetic_expr * arithmetic_expr
  | LTE of arithmetic_expr * arithmetic_expr


type expression = 
  | Arithmetic of arithmetic_expr
  | Condition of conditional_expr

type tables = 
  | Table of string
  | InnerJoin of tables * tables
  | OuterJoin of tables * tables
  | LeftJoin of tables * tables
  | RightJoin of tables * tables

type order_by = 
  | ASC of expression
  | DESC of expression

type query = {
  select : expression list;
  from : tables;
  where : conditional_expr list;
  group_by : expression list;
  order_by : order_by list
}

type query_tokens = {
  select : string list;
  from: string list;
  where : string list;
  group_by : string list;
  order_by : string list;
}

let contains lst x = List.fold_left (fun acc a -> if a = x then true else acc) false lst

let rec parse_query_list lst start_keyword end_keywords = 
  (*
  let _ =  print_string (String.concat " " lst) in 
  let _ = print_string ("\n\n") in 
  *)
  
  let rec helper lst acc end_keywords = 
    match lst with 
    | h :: t ->  
      if contains end_keywords (String.lowercase_ascii h) then (lst, List.rev acc)
      else helper t (h :: acc) end_keywords
    | [] -> (lst, List.rev acc) 
  in 
  match lst with 
  | h :: t -> if String.lowercase_ascii h = start_keyword then helper t [] end_keywords
    else (lst, [])
  | [] -> (lst, [])

let str_to_tokens str = 
  let length = String.length str in 
  let rec helper tokens start_idx len = 
    if start_idx + len >= length then (String.sub str start_idx len) :: tokens
    else 
      let x = String.sub str (start_idx + len) 1 in 
      let y = String.sub str start_idx len in
      if contains punc x
      then helper (x :: y:: tokens) 
          (start_idx + len + 1) 0
      else if contains ignore x
      then helper (y :: tokens) 
          (start_idx + len + 1) 0
      else if contains quotes x 
      then (
        let second_quote_index = String.index_from str (start_idx + len + 1) (String.get x 0) in
        let z = String.sub str (start_idx + len) (second_quote_index - (start_idx + len) + 1) in 
        helper (z :: y :: tokens) (second_quote_index + 1) 0
      )
      else helper tokens start_idx (len + 1)
  in List.filter (fun x -> x <> "") (List.rev (helper [] 0 0))

let tokenizer q = 
  let tokens = str_to_tokens q in
  let tokens, sel = parse_query_list tokens "select" ["from"] in 
  let tokens, from = parse_query_list tokens "from" ["where"; "group"; "order"] in
  let tokens, where = parse_query_list tokens "where" ["group"; "order"] in
  let tokens, group = parse_query_list tokens "group" ["order"] in 
  let tokens, order = parse_query_list tokens "order" [] in
  {
    select = sel;
    from = from;
    where = where;
    group_by = if group = [] then [] else List.tl group; (* the whole keyword is actually GROUP BY *)
    order_by = if order = [] then [] else List.tl order; (* the whole keyword is actually ORDER BY *)
  }


let print_query_tokens q_tokens = 
  print_string "SELECT Clause: ";
  print_string (String.concat " " (q_tokens.select));
  print_string "\n";
  print_string "FROM Clause: ";
  print_string (String.concat " " (q_tokens.from));
  print_string "\n";
  print_string "WHERE Clause: ";
  print_string (String.concat " " (q_tokens.where));
  print_string "\n";
  print_string "GROUP BY Clause: ";
  print_string (String.concat " " (q_tokens.group_by));
  print_string "\n";
  print_string "ORDER BY Clause: ";
  print_string (String.concat " " (q_tokens.order_by));
  print_string "\n"

let test = tokenizer "SELECT A, B, C * D, \"C FROM D\" FROM table1    WHERE A.a = B.b AND C.c = D.d GROUP BY C * 2 ORDER BY C"
