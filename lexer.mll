{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = ['-']? digit+
let float = ['-']? ((digit+['.']digit*) | (digit*['.']digit+))
let column = ['a'-'z' 'A'-'Z' '0'-'9']+
let any = ['a'-'z' 'A'-'Z' '0'-'9' ' ' '!' '"' '#' '$' '%' '&' ''' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' '\\' ']' '^' '_' '`' '{' '|' '}' '~']*
let str = '\"' any '\"'

rule read = 
  parse
  | white { read lexbuf }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUB }
  | "%" { MOD }
  | "/" { DIV }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "=" { EQ }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GEQ }
  | "<=" { LEQ }
  | "!=" { NEQ }
  | "NOT" { NOT }
  | "ASC" { ASC }
  | "DESC" { DESC }
  | "INNER JOIN" { INNERJOIN }
  | "OUTER JOIN" { OUTERJOIN }
  | "LEFT JOIN" { LEFTJOIN }
  | "RIGHT JOIN" { RIGHTJOIN }
  | "SELECT" { SELECT }
  | "FROM" { FROM }
  | "WHERE" { WHERE }
  | "GROUP BY" { GROUPBY }
  | "ORDER BY" { ORDERBY }
  | "TRUE" { BOOL (bool_of_string (String.lowercase (Lexing.lexeme lexbuf))) }
  | "FALSE" { BOOL (bool_of_string (String.lowercase (Lexing.lexeme lexbuf))) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | column { COLUMN (Lexing.lexeme lexbuf) }
  | str { STRING (Lexing.lexeme lexbuf)}
  | "," { COMMA }
  | eof { EOF }
