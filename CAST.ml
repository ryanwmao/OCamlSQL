type tables = 
  | Table of string
  | InnerJoin of tables * tables
  | OuterJoin of tables * tables
  | LeftJoin of tables * tables
  | RightJoin of tables * tables

type bop = 
  | Add
  | Sub
  | Mult
  | Div
  | Mod

type boolop = 
  | And
  | Or

type relation = 
  | EQ
  | NEQ
  | GT
  | GTE
  | LT
  | LTE

type arithmetic_expr = 
  | Integer of int
  | Float of float
  | ArithTableField of string
  | Binop of bop * arithmetic_expr * arithmetic_expr

type conditional_expr = 
  | Bool of bool
  | CondTableField of string
  | Negation of conditional_expr
  | BoolOp of boolop * conditional_expr * conditional_expr
  | Relation of relation * arithmetic_expr * arithmetic_expr

type expression = 
  | Arithmetic of arithmetic_expr
  | Condition of conditional_expr

type order_by = 
  | ASC of expression
  | DESC of expression
