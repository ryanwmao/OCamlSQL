%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token PLUS
%token TIMES
%token DIV
%token SUB
%token MOD
%token EOF
%token QUOTE
%token LPAREN
%token RPAREN
%start <CAST.arithmetic_expr> arith_expr

%%

arith_expr:
  | i = INT { Integer i }
  | i = FLOAT { Float i }
  | e1 = arith_expr;PLUS; e2 = arith_expr { Binop (Add, e1, e2)}
  | e1 = arith_expr;TIMES; e2 = arith_expr { Binop (Mult, e1, e2)}
  | e1 = arith_expr;DIV; e2 = arith_expr { Binop (Div, e1, e2)}
  | e1 = arith_expr;SUB; e2 = arith_expr { Binop (Sub, e1, e2)}
  | e1 = arith_expr;MOD; e2 = arith_expr { Binop (Mod, e1, e2)}
  | LPAREN; e = arith_expr; RPAREN { e }
  | i = STRING { ArithTableField i }

