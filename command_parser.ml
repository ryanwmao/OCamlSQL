(* #load "Str.cma" *)
(* grammar: https://forcedotcom.github.io/phoenix/index.html#expression*)

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

type temp_query = {
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



let parse_query q = 
  let split = String.split_on_char ' ' q in
  let split, sel = parse_query_list split "select" ["from"] in 
  let split, from = parse_query_list split "from" ["where"; "group"; "order"] in
  let split, where = parse_query_list split "where" ["group"; "order"] in
  let split, group = parse_query_list split "group" ["order"] in 
  let split, order = parse_query_list split "order" [] in
  {
    select = sel;
    from = from;
    where = where;
    group_by = if group = [] then [] else List.tl group; (* the whole keyword is actually GROUP BY *)
    order_by = if order = [] then [] else List.tl order; (* the whole keyword is actually ORDER BY *)
  }




