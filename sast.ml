(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

module StringMap = Map.Make(String)

type symbol_table = {
  variables: typ StringMap.t; (* Variables bound in current block *)
  parent: symbol_table option; (* Enclosing scope *)
}

type sexpr = typ * sx
and sx =
    SIntegerLiteral of int
  | SFloatLiteral of string
  | SStringLiteral of string
  | SBoolLiteral of bool
  | SListLiteral of sexpr list
  | SDictLiteral of (sexpr * sexpr) list
  | STableLiteral of (sexpr list) list
  | SListAccess of sexpr * sexpr
  | SId of string
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SPop of string * pop
  | SAssign of string * sexpr
  | SAssignOp of string * op * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type svar_decl = SVarDecl of typ * string * sexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SAppend of sexpr * sexpr
  | SAlter of sexpr * sexpr * sexpr
  | SStmtVDecl of typ * string * sexpr
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SForDecl of typ * string * sexpr * sexpr * sexpr * sstmt
  | SForEach of typ * sexpr * sexpr
  | SWhile of sexpr * sstmt

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = sfunc_decl list * sstmt list

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntegerLiteral(l) -> string_of_int l
  | SBoolLiteral(true) -> "true"
  | SBoolLiteral(false) -> "false"
  | SFloatLiteral(l) -> l
  | SStringLiteral(s) -> s
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
  | _ -> "not yet implemented\n" ) ^ ")"

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n\t" ^ String.concat "\t" (List.map string_of_sstmt stmts) ^ "}\n"
  | SStmtVDecl(_, id, e) -> "declared: " ^ id ^ " = " ^ string_of_sexpr e ^ "\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | _ -> "not yet implemented\n"

let string_of_sprogram (_, stmts) =
  String.concat "" (List.map string_of_sstmt stmts) ^ "\n" (* ^
  String.concat "\n" (List.map string_of_sfdecl funcs)*)

(* let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
 *)
