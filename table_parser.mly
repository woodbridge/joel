/* Ocamlyacc .csv parser for Joel */

%{
open Ast
%}

/* Token Declaration */
%token COMMA NEW
%token <int> INT_LIT
%token <string> FLOAT_LIT STRING_LIT
%token EOF

%start table
%type <Ast.expr> table

%%

table:
    table_literal EOF     { TableLiteral(List.rev $1) }

primitives:
    INT_LIT               { IntegerLiteral($1) }
  | FLOAT_LIT             { FloatLiteral($1) }
  | STRING_LIT            { StringLiteral($1) }

list_literal:
    primitives                    { [$1]      }
  | list_literal COMMA primitives { $3 :: $1  }
  | /* nothing */                 { []        }

table_literal:
    list_literal                  { [$1]      }
  | table_literal NEW list_literal
                                  { $3 :: $1  }