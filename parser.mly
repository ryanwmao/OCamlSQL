%{
  open Ast
%}
%token <int> INT
%token <float> FLOAT
%token <bool> BOOL
%token <string> STRING
%token <string> COLUMN
%token EQ NEQ LT GT LEQ GEQ
%token DOT
%token NOT
%token PLUS
%token TIMES
%token DIV
%token SUB
%token MOD
%token EOF
%token LPAREN
%token AND OR
%token RPAREN
%token COMMA
%token RIGHTJOIN LEFTJOIN INNERJOIN OUTERJOIN
%token SELECT FROM GROUPBY WHERE ORDERBY
%token ASC DESC

%left PLUS SUB
%left TIMES DIV MOD
%left RIGHTJOIN LEFTJOIN INNERJOIN OUTERJOIN

%start <Ast.tables> tables
%start <Ast.expressions> expressions
%start <Ast.query> query
%%

query:
  | SELECT; i = expressions; FROM; j = tables; k = optional_where; 
    l = optional_groupby; m = optional_orderby; EOF
    { (i, j, k, l, m) }
    ; 
    
expressions:
	| e = expr { SingleExpr e }
  | e = expr; COMMA; e1 = expressions { MultipleExpr (e, e1) }
  | e = expr; COMMA; e1 = expressions; EOF { MultipleExpr (e, e1) }
	;
  
expr:
  | i = INT { Integer i }
  | i = FLOAT { Float i }
  | i = BOOL { Boolean i }
  | i = COLUMN { Column i }
  | i = STRING; DOT; j = STRING 
    { 
      let i = String.sub i 1 ((String.length i) - 2) in 
      let j = String.sub j 1 ((String.length j) - 2) in 
      TableAndColumn (i, j) 
    }
  | i = STRING; DOT; j = COLUMN 
    { 
      let i = String.sub i 1 ((String.length i) - 2) in TableAndColumn (i, j) 
    }
  | i = COLUMN; DOT; j = STRING 
    { 
      let j = String.sub j 1 ((String.length j) - 2) in TableAndColumn (i, j) 
    }
  | i = COLUMN; DOT; j = COLUMN { TableAndColumn (i, j) }
  | i = STRING { let s = String.sub i 1 ((String.length i) - 2) in Str s }
  | e1 = expr; AND ; e2 = expr { Binop (AND, e1, e2)}
  | e1 = expr; OR ; e2 = expr { Binop (OR, e1, e2)}
  | e1 = expr; EQ ; e2 = expr { Binop (EQ, e1, e2)}
  | e1 = expr; NEQ ; e2 = expr { Binop (NEQ, e1, e2)}
  | e1 = expr; GT ; e2 = expr { Binop (GT, e1, e2)}
  | e1 = expr; LT ; e2 = expr { Binop (LT, e1, e2)}
  | e1 = expr; GEQ ; e2 = expr { Binop (GEQ, e1, e2)}
  | e1 = expr; LEQ ; e2 = expr { Binop (LEQ, e1, e2)}
  | e1 = expr; NOT { Not e1 }
  | e1 = expr; PLUS ; e2 = expr { Binop (Add, e1, e2)}
  | e1 = expr; TIMES ; e2 = expr { Binop (Mult, e1, e2)}
  | e1 = expr; DIV ; e2 = expr { Binop (Div, e1, e2)}
  | e1 = expr; SUB ; e2 = expr { Binop (Sub, e1, e2)}
  | e1 = expr; MOD ; e2 = expr { Binop (Mod, e1, e2)}
  | fn_name = COLUMN; LPAREN; e = expressions; RPAREN { Function (fn_name, e) }
  | LPAREN; e = expr; RPAREN { e }
  ;

tables:
  | i = STRING { let i = String.sub i 1 ((String.length i) - 2) in Table i }
  | i = COLUMN { Table i }
  | i = tables; COMMA; j = tables { InnerJoin (i, j) }
  | i = tables; RIGHTJOIN; j = tables { RightJoin (i, j) }
  | i = tables; LEFTJOIN; j = tables { LeftJoin (i, j) }
  | i = tables; INNERJOIN; j = tables { InnerJoin (i, j) }
  | i = tables; OUTERJOIN; j = tables { OuterJoin (i, j) }
  | LPAREN; i = tables; RPAREN { i }
  ;

optional_where:
  | WHERE; i = expressions { Some i }
  | { None }
  ;

optional_groupby:
  | GROUPBY; i = expressions { Some i }
  | { None }
  ;

optional_orderby:
  | ORDERBY; i = order_by { Some i }
  | { None }
  ;

order_by:
  | i = expressions; ASC { AS i }
  | i = expressions; DESC { DE i }
  | i = expressions; { AS i }
  ;
