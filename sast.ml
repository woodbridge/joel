(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SIntegerLiteral of int
  | STableLiteral of (sexpr list) list
  | SStringLiteral of string
  | SId of string
  | SCall of string * sexpr list

type svar_decl = SVarDecl of typ * string * sexpr

type sstmt =
  | SExpr of sexpr
  | SStmtVDecl of svar_decl

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = sfunc_decl list * sstmt list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SIntegerLiteral(l) -> string_of_int l
  (*| SBoolLiteral(true) -> "true"
  | SBoolLiteral(false) -> "false"
  | SFloatLiteral(l) -> l *)
  | STableLiteral(ell) -> 
    let string_of_row row = 
      "\t" ^ String.concat ", " (List.map string_of_sexpr row) ^ ";\n"
    in "(\n" ^ String.concat "\n" (List.map string_of_row ell) ^ ")"
  | SId(s) -> s
  (*| SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e*)
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  (*| SNoexpr -> ""*)  
    | _ -> "unknown"
  ) ^ ")"

let string_of_sstmt = function
    SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SStmtVDecl(SVarDecl(ty, id, e)) -> string_of_typ ty ^ " " ^ id ^ " = " ^ string_of_sexpr e ^ "\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.styp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"  

let string_of_sprogram (funcs, statements) =
  String.concat "" (List.map string_of_sfdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sstmt statements)
