/* Ocamlyacc parser for Joel */

%{
open Ast
%}

/* Token Declaration */

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA LSQBRACE RSQBRACE COLON
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN
%token PLUS MINUS TIMES DIVIDE MOD INCREMENT DECREMENT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR FOREACH IN WHILE INT BOOL FLOAT VOID
%token AND OR XOR NOT


%token NUM STRING BOOL LIST TABLE DICT TRUE FALSE
%token <int> INT_LIT
%token <string> ID FLOAT_LIT STRING_LIT
%token EOF


/* Associativity and Precedence */

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN

%left XOR OR
%left AND
%left LT GT LEQ GEQ EQ NEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
 	decls EOF					{ $1	}

decls:
	  /* nothing */ 			{ ([], [])                 }
 	| decls fdecl   			{ (($2 :: fst $1), snd $1) }
 	| decls stmt 			    { (fst $1, ($2 :: snd $1)) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  	expr SEMI 					                    { Expr $1	}
  | vdecl                                   { StmtVDecl $1 }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }

  | FOR LPAREN vdecl expr SEMI expr_opt RPAREN stmt
                                            { ForDecl($3, $4, $6, $8)   }

  | FOREACH LPAREN typ expr IN expr RPAREN stmt
                                            { ForEach($3, $4, $6)  }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
	INT_LIT                 { IntegerLiteral($1)      }
	| FLOAT_LIT					    { FloatLiteral($1)	      }
  | STRING_LIT            { StringLiteral($1)       }
  | TRUE                  { BoolLiteral(true)       }
  | FALSE                 { BoolLiteral(false)      }
  | expr PLUS expr        { Binop($1, Add, $3)      }
  | expr MINUS expr       { Binop($1, Sub, $3)      }
  | expr TIMES expr       { Binop($1, Mult, $3)     }
  | expr DIVIDE expr      { Binop($1, Div, $3)      }
  | expr MOD expr         { Binop($1, Mod, $3)      }
  | expr EQ expr          { Binop ($1, Equal, $3)   }
  | expr NEQ expr         { Binop ($1, Neq,   $3)   }
  | expr LT expr          { Binop($1, Less, $3)     }
  | expr LEQ expr         { Binop($1, Leq, $3)      }
  | expr GT expr          { Binop($1, Greater, $3)  }
  | expr GEQ expr         { Binop($1, Geq, $3)      }
  | expr AND expr         { Binop($1, And, $3)      }
  | expr OR expr          { Binop($1, Or, $3)       }
  | expr XOR expr         { Binop($1, Xor, $3)      }
  | NOT expr              { Unop(Not, $2)           }
  | MINUS expr %prec NEG  { Unop(Neg, $2)           }
	| ID				            { Id($1)				          }
  | ID INCREMENT          { Pop($1, Inc)            }
  | ID DECREMENT          { Pop($1, Dec)            }
	| ID ASSIGN expr 		    { Assign($1, $3)		      }
  | ID PLUSASSIGN expr    { AssignOp($1, Add, $3)   }
  | ID MINUSASSIGN expr   { AssignOp($1, Sub, $3)   }
  | ID TIMESASSIGN expr   { AssignOp($1, Mult, $3)  }
  | ID DIVIDEASSIGN expr  { AssignOp($1, Div, $3)   }
  | ID MODASSIGN expr     { AssignOp($1, Mod, $3)   }
  | LSQBRACE list_literal RSQBRACE { ListLiteral(List.rev $2) }
  | LSQBRACE table_literal RSQBRACE { TableLiteral(List.rev $2) }
  | LSQBRACE dict_literal RSQBRACE { DictLiteral(List.rev $2) }

primitives:
    INT_LIT           { IntegerLiteral($1) }
  | FLOAT_LIT         { FloatLiteral($1) }
  | STRING_LIT        { StringLiteral($1) }
  | TRUE                  { BoolLiteral(true)       }
  | FALSE                 { BoolLiteral(false)      }
  | LSQBRACE list_literal RSQBRACE { ListLiteral(List.rev $2) }
  | LSQBRACE dict_literal RSQBRACE { DictLiteral(List.rev $2) }

list_literal:
    primitives                      { [$1] }
  | list_literal COMMA primitives { $3 :: $1 }

table_literal:
    list_literal                    { [$1] }
  | list_literal SEMI list_literal  { $3 :: $1 }

key_val:
    primitives COLON primitives   { ($1,$3) }

dict_literal:
    key_val   { [$1] }
  | dict_literal COMMA key_val { $3 :: $1 }

fdecl:
  typ ID LPAREN arg_list_toplevel RPAREN LBRACE stmt_list RBRACE { FuncDecl ($1, $2, List.rev $7, $4) }


arg_list_toplevel:
               { [] }
  | arg_list   { List.rev $1 }

arg_list:
    formal          { [$1] }
  | arg_list COMMA formal { $3 :: $1 }

formal:
  typ ID { Formal($1, $2) }

vdecl:
	  typ ID SEMI					      { VarDecl($1, $2, Noexpr)	}
	| typ ID ASSIGN expr SEMI 	{ VarDecl($1, $2, $4)		}

typ:
	NUM							  { Num 	  }
  | STRING          { String  }
  | BOOL            { Bool    }
  | LIST            { List    }
  | DICT            { Dict    }
  | TABLE           { Table    }
