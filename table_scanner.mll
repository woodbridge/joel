(* Ocamllex .csv file scanner for Joel *)

{ open Table_parser }

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| '\n'					{ NEW }
| ','					{ COMMA }
| digits as lxm 		{ INT_LIT(int_of_string lxm) }
| digits '.'  digit* as lxm 	
						{ FLOAT_LIT(lxm) }
| [^ ',' '\n']* [^ '0' - '9' ',' '\n']+ [^ ',' '\n']* as strlit 
						{ STRING_LIT(strlit) }
| eof 					{ EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
