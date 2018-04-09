(* Semantic checking for the Joel compiler *)

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

let convertToSAST (statements, funcs) =

(*   let symbols =
  {
    variables = StringMap.empty;
    parent = None;

  }
 *)
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

  let check_assign lvaluet rvaluet err =
     if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in

  let rec convert_expr = function
    StringLiteral s -> (String, SStringLiteral s)
  | IntegerLiteral s -> (Num, SIntegerLiteral s)
  | FloatLiteral s -> (Num, SFloatLiteral s)
  | BoolLiteral s -> (Bool, SBoolLiteral s)
  | TableLiteral rows ->
    let check_row row =
      List.map convert_expr row
    in (Table, STableLiteral(List.map check_row rows))
  | ListLiteral row -> (List, SListLiteral(List.map convert_expr row))
  | DictLiteral row ->
    let convert_pair (e1, e2) =
      (convert_expr e1, convert_expr e2)
    in (Dict, SDictLiteral(List.map convert_pair row))
  | Binop(e1, op, e2) as e ->
    let (t1, e1') = expr e1
    and (t2, e2') = expr e2 in
    let same_type = t1 = t2 in
    let ty = match op with
        Add | Sub | Mult | Div | Mod when same_type && t1 = Num -> Num
      | Equal | Neq when same -> Bool
      | Less | Leq | Greater | Geq when same && t1 = Num -> Bool
      | And | Or | Xor when same && t1 = Bool -> Bool
      | Add when same && t1 = String -> String
      | _ -> raise (Failure("illegal binary operator."))
	      (* Failure ("illegal binary operator " ^   <<<<< pretty print needed
                       string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                       string_of_typ t2 ^ " in " ^ string_of_expr e)) *)
      in (ty, SBinop((t1, e1'), op, (t2, e2')))
  | Unop(op, e1) as e ->
      let (t, e1') = expr e1 in
      let ty = match op with
        Neg when t = Num -> Num
      | Not when t = Bool -> Bool
      | _ -> raise (
          Failure("illegal unary operator.")
          (* Failure ("illegal unary operator " ^  <<<<< need pretty print
          string_of_uop op ^ string_of_typ t ^
                   " in " ^ string_of_expr ex) *)
        )
      in (ty, SUnop(op, (t, e')))
  | Pop(id, op) as e ->
    let t = type_of_id id in
    let ty = match op with
        Inc when t = Num -> Num
      | Dec when t = Num -> Num
      | _ -> raise (
          Failure("illegal use of pop operator.")
        )
    in (ty, Pop(id, op))
  | Call(fname, args) as call ->
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
  | Assign(id, e) as ex ->
      let lt = type_of_id id
      and (rt, e') = expr e in
      let err = "illegal assignment."
      (* let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
        string_of_typ rt ^ " in " ^ string_of_expr ex *)
      in (check_assign lt rt err, SAssign(var, (rt, e')))
  | AssignOp(id, op, e) as ex ->
      let lt = type_of_id id
      and (rt, e') = expr e in
      (* let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
        string_of_typ rt ^ " in " ^ string_of_expr ex *)
      let same_type = lt = rt in
      let ty = match op with
          Add | Sub | Mult | Div | Mod when same_type && lt = Num -> Num
        | Add when same && lt = String -> String
        | _ -> raise (Failure("illegal assignment."))
        in (ty, SAssign(var, (rt, e')))
  | Noexpr -> (Void, SNoexpr)
  | Id s -> (type_of_id s, SId s)

  in
  let convert_vardecl (t, id, e) = (t, id, convert_expr e) (* TODO: Add checking for id *)
  in

  let check_bool_expr e = 
    let (t', e') = convert_expr e
    and err = "expected Boolean expression."
    in if t' != Bool then raise (Failure err) else (t', e') 
  in

  let rec convert_statement = function
    Expr e -> convert_expr e
  | StmtVDecl v -> SStmtVDecl(convert_vardecl v) (* Returns a SStmtVDecl *)
  | Block sl ->
      let rec convert_statement_list = function
          [Return _ as s] -> [convert_statement s]
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> convert_statement (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> convert_statement s :: convert_statement_list ss
        | []              -> []
      in SBlock(convert_statement_list sl)
  | If(p, b1, b2) -> SIf(check_bool_expr p, convert_statement b1, convert_statement b2)
  | For(e1, e2, e3, st) ->
    SFor(convert_expr e1, check_bool_expr e2, convert_expr e3, convert_statement st)
  | ForDecl(e1, e2, e3, st) ->
    SForDecl(convert_vardecl e1, check_bool_expr e2, convert_expr e3, convert_statement st)
  | ForEach(t, e1, e2) ->
    SForEach(t, convert_expr e1, convert_expr e2)
  | While(e, st) ->
    SWhile(convert_expr e, convert_statement st)
  in

  let statements' = List.map convert_statement statements in 

  let rec convert_func func = 

    let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty formals
    in

    let rec convert_func_statement = function
      Expr e -> convert_expr e
    | StmtVDecl v -> SStmtVDecl(convert_vardecl v) (* Returns a SStmtVDecl *)
    | Block sl ->
        let rec convert_statement_list = function
            [Return _ as s] -> [convert_statement s]
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> convert_statement (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> convert_statement s :: convert_statement_list ss
          | []              -> []
        in SBlock(convert_statement_list sl)
    | If(p, b1, b2) -> SIf(check_bool_expr p, convert_statement b1, convert_statement b2)
    | For(e1, e2, e3, st) ->
      SFor(convert_expr e1, check_bool_expr e2, convert_expr e3, convert_statement st)
    | ForDecl(e1, e2, e3, st) ->
      SForDecl(convert_vardecl e1, check_bool_expr e2, convert_expr e3, convert_statement st)
    | ForEach(t, e1, e2) ->
      SForEach(t, convert_expr e1, convert_expr e2)
    | While(e, st) ->
      SWhile(convert_expr e, convert_statement st)
    | Return e -> let (t, e') = expr e in
      if t = func.typ then SReturn (t, e') 
      else raise (
        Failure("Function expects different return type.")
      (*Failure ("return gives " ^ string_of_typ t ^ " expected " ^
         string_of_typ func.typ ^ " in " ^ string_of_expr e) *)      
      )
      
    in
    { styp = func.typ;
      sfname = func.fname;
      sformals = formals;
      sbody = match convert_func_statement (Block func.body) with
          SBlock(sl) -> sl
        | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (statements', List.map convert_func funcs)
