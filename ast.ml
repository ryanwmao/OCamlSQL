(** The abstract syntax tree type *)

(** [bop] is the type for binary expressions *)
type bop =
  | AND
  | OR
  | EQ
  | NEQ
  | GT
  | LT
  | GEQ
  | LEQ
  | Add
  | Sub
  | Mult
  | Div
  | Mod

let string_of_bop b =
  match b with
  | AND -> "AND"
  | OR -> "OR"
  | EQ -> "="
  | NEQ -> "!="
  | GT -> ">"
  | LT -> "<"
  | GEQ -> ">="
  | LEQ -> "<="
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

(** [tables] is the type for tables *)
type tables =
  | Table of string
  | OuterJoin of tables * tables
  | LeftJoin of tables * tables
  | RightJoin of tables * tables
  | InnerJoin of tables * tables

(** [expressions] is the parent type for expressions *)
type expressions =
  | SingleExpr of expr
  | MultipleExpr of expr * expressions

and expr =
  | Boolean of bool
  | Integer of int
  | Float of float
  | Column of string
  | TableAndColumn of string * string
  | Str of string
  (* NOTE: a String could be a column name, or a string *)
  | Binop of bop * expr * expr
  | Not of expr
  | Function of string * expressions

type order_by =
  | AS of expressions
  | DE of expressions

(* type query = { select : expressions; from : tables; where :
   expressions option; groupby : expressions option; orderby : order_by
   option; }*)

(** [query] is the type for queries *)
type query =
  expressions
  * tables
  * expressions option
  * expressions option
  * order_by option

(** Breaks down an expression [exprs] into smaller expressions *)
let rec expressions_to_expr_list exprs =
  match exprs with
  | MultipleExpr (e, es) -> e :: expressions_to_expr_list es
  | SingleExpr e -> [ e ]

(** Converts an expression [expr] into a string *)
let rec string_of_expr expr =
  match expr with
  | Boolean b -> string_of_bool b
  | Integer i -> string_of_int i
  | Float f -> string_of_float f
  | Column c -> c
  | TableAndColumn (t, c) ->
      add_quotes_if_needed t ^ "." ^ add_quotes_if_needed c
  | Str s -> "\"" ^ s ^ "\""
  | Binop (b, e1, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_bop b ^ " "
      ^ string_of_expr e2 ^ ")"
  | Not e -> "NOT " ^ string_of_expr e
  | Function (fn_name, es) -> fn_name ^ "(" ^ string_of_exprs es ^ ")"

and string_of_exprs exprs =
  let exprs = expressions_to_expr_list exprs in
  let res = List.map (fun a -> string_of_expr a) exprs in
  String.concat ", " res

and add_quotes_if_needed s =
  if String.contains s ' ' || String.contains s '\t' then
    "\"" ^ s ^ "\""
  else s
