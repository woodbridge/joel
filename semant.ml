(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Check to see if a variable is in the given symbol table. *)
let rec find_variable (scope: symbol_table) name = 
  try 
    StringMap.find name scope.variables
  with Not_found -> 
    match scope parent with
        Some(parent) -> find_variable parent name
      | _ -> raise Not_found

let convertToSAST (statements) = 

  let symbols = 
  {
    variables = StringMap.empty;
    parent = None;

  }

  let functions =
  {
    variables = StringMap.empty;
    parent = None;
  }

  let type_of_id s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let rec convert_expr = function
    StringLiteral s -> (String, SStringLiteral s)
  | TableLiteral rows -> 
    let check_row row = 
      List.map convert_expr row
    in (Table, STableLiteral(List.map check_row rows)) 
  | Call(id, args) as call -> 
      let fd = find_func fname in
      let param_length = List.length fd.formals in
      if List.length args != param_length then
        raise (Failure ("expecting " ^ string_of_int param_length ^ 
                              " arguments in " ^ string_of_expr call))
      else let check_call (ft, _) e = 
        let (et, e') = convert_expr e in 
        let err = "illegal argument found " ^ string_of_typ et ^
          " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
        in (check_assign ft et err, e')
      in 
      let args' = List.map check_call fd.formals args
      in (fd.typ, SCall(fname, args'))

  | Noexpr -> (Void, SNoexpr)
  | Id s -> (type_of_id s, SId s)

  in
  let convert_vardecl (t, id, e) = (t, convert_expr e) (* TODO: Add checking for id *)
  in 

  let rec convert_statement = function
    Expr e -> convert_expr e 
  | StmtVDecl v -> SStmtVDecl(convert_vardecl v) (* Returns a SStmtVDecl *)
  
  in List.map convert_statement statements
