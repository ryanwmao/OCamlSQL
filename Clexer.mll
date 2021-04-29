{
  open Cparser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = ['+' '-']? digit+
let float = ['+' '-']? ((digit+['.']digit*) | (digit*['.']digit+))
let str = ['a'-'z' 'A'-'Z' '0'-'9']+

rule read = 
  parse
  | white { read lexbuf }
  | "*" { TIMES }
  | "+" { PLUS }
  | "-" { SUB }
  | "%" { MOD }
  | "/" { DIV }
  | "\"" { QUOTE }
  | "(" {LPAREN}
  | ")" {RPAREN}
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | str { STRING (Lexing.lexeme lexbuf) }
  | eof { EOF }