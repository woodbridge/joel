(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq | 
          And | Or | Xor

type uop = Not | Neg

type typ = Num | String | Bool

type expr = 
    IntegerLiteral of int
  | FloatLiteral of string
  | StringLiteral of string
  | BoolLiteral of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Noexpr

type stmt = 
    Expr of expr

type var_decl = VarDecl of typ * string * expr

type program = var_decl list * stmt list


(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&"
  | Or -> "|"
  | Xor -> "^"

let string_of_uop = function
    Not -> "!"
  | Neg -> "-"

let rec string_of_expr = function
    IntegerLiteral(l) -> string_of_int l
  | FloatLiteral(l) -> l
  | StringLiteral(l) -> "\"" ^ l ^ "\""
  | BoolLiteral(l) -> string_of_bool l
  | Id(s) -> s
  | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
  | Unop(uop, e) -> string_of_uop uop ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Noexpr -> ""

let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ ";\n";
  | _ -> "" (* Add other statements here *)

let string_of_typ = function
    Num -> "num"
  | String -> "string"
  | Bool -> "bool"

let string_of_vdecl = function 
   VarDecl(t, id, Noexpr) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | VarDecl(t, id, e) -> string_of_typ t ^ " " ^ id ^  "=" ^ string_of_expr e ^ ";\n"

let string_of_program (vars, stmts) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt stmts)
