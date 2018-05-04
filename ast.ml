(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or | Xor

type uop = Not | Neg

type pop = Inc | Dec

type typ = Num | String | Bool | List of typ | Dict | Table | Void

type bind = typ * string

type expr =
    IntegerLiteral of int
  | FloatLiteral of string
  | StringLiteral of string
  | BoolLiteral of bool
  | ListLiteral of expr list
  | DictLiteral of (expr * expr) list
  | TableLiteral of (expr list) list
  | ListAccess of expr * expr
  | Length of expr
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Pop of string * pop
  | Assign of string * expr
  | AssignOp of string * op * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Append of expr * expr
  | Alter of expr * expr * expr
  | StmtVDecl of typ * string * expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | ForDecl of typ * string * expr * expr * expr * stmt
  | ForEach of typ * string * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = func_decl list * stmt list


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

let string_of_pop = function
    Inc -> "++"
  | Dec -> "--"

let string_of_typ = function
    Num -> "num"
  | String -> "string"
  | Bool -> "bool"
  | List(_) -> "list"
  | Dict -> "dict"
  | Table -> "table"
  | Void -> "void"


let rec string_of_expr = function
    IntegerLiteral(l) -> string_of_int l
  | FloatLiteral(l) -> l
  | StringLiteral(l) -> "\"" ^ l ^ "\""
  | BoolLiteral(l) -> string_of_bool l
  | ListLiteral(_) -> "List"
  | ListAccess(_, _) -> "List Access"
  | Length(_) -> "List Length"
  | DictLiteral(_) -> "Dict"
  | TableLiteral(_) -> "Table"
  | Call(s, _) -> s ^ "()"
  | Id(s) -> s
  | Binop(e1, op, e2) -> string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2
  | Unop(uop, e) -> string_of_uop uop ^ string_of_expr e
  | Pop(s, pop) -> s ^ string_of_pop pop
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | AssignOp(v, op, e) -> v ^ " " ^ string_of_op op ^ "=" ^ " " ^ string_of_expr e
  | Noexpr -> ""

let rec string_of_stmt = function
    Expr(expr) -> string_of_expr expr ^ ";\n";
  | Block(l) -> String.concat "\n" (List.map string_of_stmt l) ^ "\n"
  | StmtVDecl(t, n, e) -> string_of_typ t ^ " " ^ n ^ " = " ^ string_of_expr e ^ ";"
  | Return(e) -> "return " ^ string_of_expr e
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s1 ^ "\n} else  {\n" ^ string_of_stmt s2 ^ "}\n"
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ "; " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^ ") {\n" ^ string_of_stmt s ^ "}\n"
  | _ -> "pretty-print not implemented\n"

let string_of_fdecl func = string_of_typ func.typ ^ " " ^ func.fname ^ " "

let string_of_program (funcs, stmts) =
  String.concat "" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_stmt (List.rev stmts))
