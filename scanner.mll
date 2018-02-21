(* Ocamllex scanner for Joel *)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '%'	   { MOD }
| "++"     { INCREMENT }
| "--"     { DECREMENT }
| '='      { ASSIGN }
| "+="     { PLUSASSIGN }
| "-="     { MINUSASSIGN }
| "*="     { TIMESASSIGN }
| "/="     { DIVIDEASSIGN }
| "%="	   { MODASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| '&'	   { AND }
| '|'	   { OR }
| '^'	   { XOR }
| '!'	   { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "foreach" { FOREACH }
| "in"     { IN }
| "while"  { WHILE }
| "return" { RETURN }
| "num"    { NUM }
| "string" { STRING }
| "bool"   { BOOL }
| "true"   { TRUE }
| "false"  { FALSE }
| digits as lxm { INT_LIT(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLOAT_LIT(lxm) }
| '"' (([^ '"'] | "\\\"")* as strlit) '"' { STRING_LIT(strlit) } 
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }