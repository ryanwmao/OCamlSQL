open Cast

let is_arith_value = function
  | Integer _ | Float _ | ArithTableField _ -> true
  | Binop _ -> false

let is_cond_value = function
  | Bool _ | CondTableField _ | CondTableExpr _ -> true
  | Negation _ | BoolOp _ | Relation _ -> false

let rec step_arith = function
  | Integer _ | Float _ | ArithTableField _ -> failwith "No step"
  | Binop (bop, e1, e2) when is_arith_value e1 && is_arith_value e2
    -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_arith_value e1 -> Binop (bop, e1, step_arith e2)
  | Binop (bop, e1, e2) -> Binop (bop, step_arith e1, e2)

and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Integer a, Integer b -> Integer (a + b)
  | Add, Float a, Float b -> Float (a +. b)
  | Sub, Integer a, Integer b -> Integer (a - b)
  | Sub, Float a, Float b -> Float (a -. b)
  | Mult, Integer a, Integer b -> Integer (a * b)
  | Mult, Float a, Float b -> Float (a *. b)
  | Div, Integer a, Integer b -> Integer (a / b)
  | Div, Float a, Float b -> Float (a /. b)
  | Mod, Integer a, Integer b -> Integer (a mod b)
  | Mod, Float a, Float b -> Float (mod_float a b)
  | Add, _, _ | Sub, _, _ | Mult, _, _ | Div, _, _ | Mod, _, _ 
    -> failwith "argument error"

let rec step_cond = function
  | Bool _ | CondTableField _ -> failwith "No step"
  | Negation e1 when is_cond_value e1 -> step_neg e1
  | Negation e1 -> Negation ()
  | BoolOp _ | Relation _ -> false

and step_neg e1 = failwith ""


let parse (s : string) = failwith ""

let eval (t: tables) (s : string) = failwith ""

let readcsv_of_columns = failwith ""

let interp (s : string) (t: tables) = 
  s |> parse |> eval t |> readcsv_of_columns