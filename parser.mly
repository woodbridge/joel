/* Ocamlyacc parser for Joel */

%{
open Ast
%}

/* Token Declaration */
%token LPAREN RPAREN LBRACE RBRACE COMMA LSQBRACE RSQBRACE LPOINTY RPOINTY COLON SEMI
%token ASSIGN PLUSASSIGN MINUSASSIGN TIMESASSIGN DIVIDEASSIGN MODASSIGN
%token PLUS MINUS TIMES DIVIDE MOD INCREMENT DECREMENT
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR FOREACH IN WHILE INT BOOL FLOAT VOID
%token AND OR XOR NOT


%token NUM STRING BOOL LIST DICT TABLE TRUE FALSE
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
 	decls EOF					    { $1	}

decls:
	  /* nothing */ 			{ ([], [])                 }
 	| decls fdecl   			{ (($2 :: fst $1), snd $1) }
 	| decls stmt 			    { (fst $1, ($2 :: snd $1)) }

stmt_list:
    /* nothing */       { [] }
  | stmt_list stmt      { $2 :: $1 }

stmt:
  	expr SEMI 					                    { Expr $1	}
  | vdecl                                   { $1 }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }

  | FOR LPAREN typ ID ASSIGN expr SEMI expr SEMI expr_opt RPAREN stmt
                                            { ForDecl($3, $4, $6, $8, $10, $12)   }

  | FOREACH LPAREN typ expr IN expr RPAREN stmt
                                            { ForEach($3, $4, $6)  }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
	 primitives             { $1                      }
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
  | ID LPAREN args_opt RPAREN 
                          { Call($1, $3)        }
  | LPAREN expr RPAREN    { $2                      }
  | LPOINTY table_literal RPOINTY 
                          { TableLiteral(List.rev $2) }

primitives:
    INT_LIT               { IntegerLiteral($1) }
  | FLOAT_LIT             { FloatLiteral($1) }
  | STRING_LIT            { StringLiteral($1) }
  | TRUE                  { BoolLiteral(true)       }
  | FALSE                 { BoolLiteral(false)      }
  | LSQBRACE list_literal RSQBRACE 
                          { ListLiteral(List.rev $2) }
  | LSQBRACE dict_literal RSQBRACE 
                          { DictLiteral(List.rev $2) }

list_literal:
    primitives                    { [$1]      }
  | list_literal COMMA primitives { $3 :: $1  }

key_val:
    primitives COLON primitives   { ($1,$3)   }

dict_literal:
    key_val   { [$1] }
  | dict_literal COMMA key_val    { $3 :: $1  }

table_literal:
    list_literal                  { [$1]      }
  | table_literal SEMI list_literal  
                                  { $3 :: $1  }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { {  typ = $1;
          fname = $2;
          formals = $4;
          body = List.rev $7 } }

formals_opt:
    /* nothing */                 { []          }
  | formal_list                   { List.rev $1 }

formal_list:
    typ ID                        { [($1,$2)]     }
  | formal_list COMMA typ ID      { ($3,$4) :: $1 }

vdecl:
	  typ ID SEMI					          { StmtVDecl($1, $2, Noexpr)	}
	| typ ID ASSIGN expr SEMI 	    { StmtVDecl($1, $2, $4)		  }

typ:
	NUM							  { Num 	  }
  | STRING          { String  }
  | BOOL            { Bool    }
  | LIST            { List    }
  | DICT            { Dict    }
  | TABLE           { Table   }
  | VOID            { Void }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
