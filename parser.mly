/* Ocamlyacc parser for Joel */

%{
open Ast
%}

%token EOF
%token SEMI ASSIGN
%token PLUS MINUS TIMES DIVIDE MOD
%token NUM STRING BOOL TRUE FALSE
%token <int> INT_LIT
%token <string> ID FLOAT_LIT STRING_LIT

%start program
%type <Ast.program> program

%right ASSIGN

%left PLUS MINUS
%left TIMES DIVIDE MOD

%%

program:
 	decls EOF					{ $1	}

decls:
	  /* nothing */ 			{ ([], [])               }
 	| decls vdecl 				{ (($2 :: fst $1), snd $1) }
 	| decls stmt				{ (fst $1, ($2 :: snd $1)) }

stmt:
	expr SEMI 					{ Expr $1	}

expr:
	INT_LIT              { IntegerLiteral($1)	}
	| FLOAT_LIT					 { FloatLiteral($1)		}
  | STRING_LIT         { StringLiteral($1)  }
  | TRUE               { BoolLiteral(true)  }
  | FALSE              { BoolLiteral(false) }
  | expr PLUS expr     { Binop($1, Add, $3) }
  | expr MINUS expr    { Binop($1, Sub, $3) }
  | expr TIMES expr    { Binop($1, Mult, $3)}
  | expr DIVIDE expr   { Binop($1, Div, $3) }
  | expr MOD expr      { Binop($1, Mod, $3) }
	| ID				         { Id($1)				      }
	| ID ASSIGN expr 		 { Assign($1, $3)		  }

vdecl:
	typ ID SEMI					{ VarDecl($1, $2, Noexpr)	}
	| typ ID ASSIGN expr SEMI 	{ VarDecl($1, $2, $4)		}

typ:
	NUM							  { Num 	  }
  | STRING          { String  }
  | BOOL            { Bool    }