(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Check to see if a variable is in the given symbol table. *)

let convertToSAST (_, statements) = 

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void; fname = name; 
      formals = [(ty, "x")];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("printf", Table) ]
  in

  let function_decls = built_in_decls
  in

  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
    the given lvalue type *)
  let check_assign lvaluet rvaluet err =
      if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  (* Create a global symbol table *)
  let globals = StringMap.empty
  in 

  (* Add a binding to the global symbol table *)
  let add_bind t id = StringMap.add id t globals
  in

  (* Return the type of a given id in our symbol table *)
  let type_of_identifier s =
      try StringMap.find s globals
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  let rec expr = function
    IntegerLiteral i -> (Num, SIntegerLiteral i)
  | StringLiteral s -> (String, SStringLiteral s)
  | TableLiteral rows -> 
    let check_row row = 
      List.map expr row
    in (Table, STableLiteral(List.map check_row rows)) 
  | Call(fname, args) -> 
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^ 
                              " arguments in expression"))
      else let check_call (ft, _) e = 
        let (et, e') = expr e in 
        let err = "illegal argument found " ^ string_of_typ et ^
          " expected " ^ string_of_typ ft ^ " in expression"
        in (check_assign ft et err, e')
      in 
      let args' = List.map2 check_call fd.formals args
      in (fd.typ, SCall(fname, args'))
  | Id s -> (type_of_identifier s, SId s)
  | _ -> raise (Failure ("Error: Not Yet Implemented"))

  in

  let convert_vardecl = function
    VarDecl(t, id, e) -> 
    let _ = add_bind t id in SVarDecl((t, id, expr e))
  in 

  let convert_statement = function
    Expr e -> SExpr(expr e)
  | StmtVDecl v -> SStmtVDecl(convert_vardecl v) (* Returns a SStmtVDecl *)
  | _ -> raise (Failure ("Error: not yet Implemented"))
  
  in ([], List.map convert_statement statements)