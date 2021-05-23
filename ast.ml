
type bop = 
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

type tables = 
  | Table of string
  | OuterJoin of tables * tables
  | LeftJoin of tables * tables
  | RightJoin of tables * tables
  | InnerJoin of tables * tables

type expr = 
  | Boolean of bool
  | Integer of int
  | Float of float
  | Column of string
  | Str of string (* NOTE: a String could be a column name, or a string *)
  | Binop of bop * expr * expr
  | Not of expr

type expressions = 
  | SingleExpr of expr
  | MultipleExpr of expr * expressions

type order_by = 
  | AS of expressions
  | DE of expressions

  (*
type query = {
  select : expressions;
  from : tables;
  where : expressions option;
  groupby : expressions option;
  orderby : order_by option;
}*)

type query = expressions * tables * expressions option * expressions option * order_by option

