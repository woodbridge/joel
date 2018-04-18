(* Semantic checking for the MicroC compiler *)
(* TODO: Does modulo reject if one or both operands are not integers? *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (functions, statements) =
  
(* for printing out a list of the statements for debugging
  let rec print_list = function
    [] -> ()
  | e::l -> Printf.printf "%s\n END\n" e ; print_list l
  in
  let s = List.map string_of_stmt statements
  in 
  let () = print_list s
  in
*)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void; fname = name;
      formals = [(ty, "x")];
      body = [] } map
    in List.fold_left add_bind StringMap.empty [  ("printf", Num);
                                                  ("printb", Bool) ]
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
         _ when StringMap.mem n built_in_decls -> make_err built_in_err
       | _ when StringMap.mem n map -> make_err dup_err
       | _ ->  StringMap.add n fd map
  in

  (* Collect all other function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* Define the global variable table. *)
  let variable_table = {
    variables = StringMap.empty;
    parent = None;
  }
  in

  (* Get a reference to the global table we just created.
    We will pass this scope via reference through recursive calls
    and mutate it when we need to add a new variable. *)
  let global_scope = ref variable_table 
  in

  (* Find a variable, beginning in a given scope and searching upwards. *)
  let rec find_variable (scope: symbol_table ref) name = 
    try StringMap.find name !scope.variables
    with Not_found ->
      match !scope.parent with
          Some(parent) -> find_variable (ref parent) name
        | _ -> print_string ("Semantic error: cound not find " ^ name ^ "\n"); raise Not_found
  in

  (* Map a variable's name to its type in the symbol table. *)
   let add_variable (scope: symbol_table ref) t n = 
      scope := {
        variables = StringMap.add n t !scope.variables;
        parent = !scope.parent;
      }
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
     the given lvalue type *)
  let check_assign lvaluet rvaluet err =
     if lvaluet = rvaluet then lvaluet else print_string ("Semantic error: bad assign\n"); raise (Failure err)
  in

  let rec convert_expr scope exp = match exp with
      StringLiteral s -> (String, SStringLiteral s)
    | IntegerLiteral s -> (Num, SIntegerLiteral s)
    | FloatLiteral s -> (Num, SFloatLiteral s)
    | BoolLiteral s -> (Bool, SBoolLiteral s)
    | TableLiteral rows ->
      let check_row row =
        List.map (convert_expr scope) row
      in (Table, STableLiteral(List.map check_row rows))
    | ListLiteral row -> (List, SListLiteral(List.map (convert_expr scope) row))
    | DictLiteral row ->
      let convert_pair (e1, e2) =
        (convert_expr scope e1, convert_expr scope e2)
      in (Dict, SDictLiteral(List.map convert_pair row))
    | Binop(e1, op, e2) as e ->
      let (t1, e1') = convert_expr scope e1
      and (t2, e2') = convert_expr scope e2 in
      let same_type = t1 = t2 in
      let ty = match op with
          Add | Sub | Mult | Div | Mod when same_type && t1 = Num -> Num
        | Equal | Neq when same_type -> Bool
        | Less | Leq | Greater | Geq when same_type && t1 = Num -> Bool
        | And | Or | Xor when same_type && t1 = Bool -> Bool
        | Add when same_type && t1 = String -> String
        | _ -> raise (Failure("illegal binary operator."))
  	      (* Failure ("illegal binary operator " ^   <<<<< pretty print needed
                         string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                         string_of_typ t2 ^ " in " ^ string_of_expr e)) *)
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Unop(op, e1) as e ->
        let (t, e1') = convert_expr scope e1 in
        let ty = match op with
          Neg when t = Num -> Num
        | Not when t = Bool -> Bool
        | _ -> raise (
            Failure("illegal unary operator.")
            (* Failure ("illegal unary operator " ^  <<<<< need pretty print
            string_of_uop op ^ string_of_typ t ^
                     " in " ^ string_of_expr ex) *)
          )
        in (ty, SUnop(op, (t, e1')))
    | Pop(id, op) as e ->
      let t = find_variable scope id in
      let ty = match op with
          Inc when t = Num -> Num
        | Dec when t = Num -> Num
        | _ -> raise (
            Failure("illegal postop.")
          )
      in (ty, SPop(id, op))
    (* | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          (* need pretty printing functions *)
          (* raise (Failure ("expecting " ^ string_of_int param_length ^
                                " arguments in " ^ string_of_expr call)) *)
          raise (Failure ("expecting a different number of arguments."))
        else let check_call (ft, _) e =
          let (et, e') = convert_expr scope e in
          let err = "illegal argument found."
          (* let err = "illegal argument found " ^ string_of_typ et ^
            " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e *)
          in (check_assign ft et err, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(fname, args')) *)
    | Assign(id, e) as ex ->
        let lt = find_variable scope id
        and (rt, e') = convert_expr scope e in
        let err = "illegal assignment."
        (* let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
          string_of_typ rt ^ " in " ^ string_of_expr ex *)
        in (check_assign lt rt err, SAssign(id, (rt, e')))
    | AssignOp(id, op, e) as ex ->
        let lt = find_variable scope id
        and (rt, e') = convert_expr scope e in
        (* let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
          string_of_typ rt ^ " in " ^ string_of_expr ex *)
        let same_type = lt = rt in
        let ty = match op with
            Add | Sub | Mult | Div | Mod when same_type && lt = Num -> Num
          | Add when same_type && lt = String -> String
          | _ -> raise (Failure("illegal assignment."))
          in (ty, SAssignOp(id, op, (rt, e')))
    | Noexpr -> (Void, SNoexpr)
    | Id s -> (find_variable scope s, SId s)
    | _ -> raise (Failure("semant: not yet implemented."))
  in

  let convert_vardecl scope (ty, id, e) =
    let (e_ty, e') = convert_expr scope e in
    let same_type = ty = e_ty in
    if same_type then 
    let _ = add_variable scope ty id 
    in SStmtVDecl(e_ty, id, (e_ty, e'))
    (* TODO: fix to add better error message *)
    else raise(Failure("Expected different type of expression."))
  in

  let get_first_item (a, _) = a
  in
  let get_second_item (_, a) = a
  in

  let rec check_statement (scope: symbol_table) stmt = match stmt with
  | Expr e -> scope
  | StmtVDecl (ty, id, exp) ->
    {
      variables = StringMap.add id ty scope.variables;
      parent = None;
    }
  | Block sl ->
      let rec check_statement_list = function
          [Return _ as s] -> [check_statement scope s]
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> check_statement_list (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> (check_statement scope s) :: check_statement_list ss
        | []              -> []
      in let table_lst = check_statement_list sl
      in if List.length table_lst > 0 then List.hd (List.rev (check_statement_list sl))
      else scope
  | If(p, b1, b2) ->
    let scope2 = check_statement scope b1 in
    check_statement scope2 b2
  | For(e1, e2, e3, st) -> scope
  | ForDecl(e1_ty, e1_id, e1_ex, e2, e3, st) -> scope
  | ForEach(t, e1, e2) -> scope
  | While(e, st) -> check_statement scope st
  | Return e ->  raise (
      Failure("Cannot return unless inside a function.")
    )
  in

  let check_bool_expr e =
    let (t', e') = convert_expr scope e
    and err = "expected Boolean expression."
    (* and err = "expected Boolean expression in " ^ string_of_expr e *)
    in if t' != Bool then raise (Failure err) else (t', e')
  in


  let rec convert_statement scope expr = match expr with
    Expr e -> SExpr(convert_expr scope e)
  | StmtVDecl(ty, id, exp) -> convert_vardecl scope (ty, id, exp) (* Returns a SStmtVDecl *)
  | Block sl ->
      let new_scope = {
              variables = StringMap.empty;
              parent = !scope.parent;
      }
      in let new_scope_r = ref new_scope in 
      let rec convert_statement_list = function
          [Return _ as s] -> [convert_statement new_scope s]
        | Return _ :: _   -> raise (Failure "nothing may follow a return")
        | Block sl :: ss  -> convert_statement_list (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> convert_statement new_scope s :: convert_statement_list ss
        | []              -> []
      in SBlock(convert_statement_list sl)
  | If(p, b1, b2) -> SIf(check_bool_expr p, convert_statement b1, convert_statement b2)
  | For(e1, e2, e3, st) ->
    SFor(convert_expr scope e1, check_bool_expr e2, convert_expr scope e3, convert_statement st)
  | ForDecl(e1_ty, e1_id, e1_ex, e2, e3, st) ->
      let (e_ty, e') = convert_expr scope e1_ex in
      let same_type = e1_ty = e_ty in
      if same_type then
        SForDecl(e_ty, e1_id, (e_ty, e'), check_bool_expr e2, convert_expr scope e3, convert_statement st)
        (* TODO: fix to add better error message *)
        else raise(Failure("Expected different type of expression."))
  | ForEach(t, e1, e2) ->
    SForEach(t, convert_expr scope e1, convert_expr scope e2)
  | While(e, st) ->
    SWhile(convert_expr scope e, convert_statement st)
  | Return e ->
    raise (
      Failure("Cannot return unless inside of a function.")
    (*Failure ("return gives " ^ string_of_typ t ^ " expected " ^
       string_of_typ func.typ ^ " in " ^ string_of_expr e) *)
    )
  in

  let convert_statements statement =
    convert_statement global_scope statement
  in

  let statements' = List.map convert_statements statements
  in

  let check_function func =


    (* Build local symbol table of variables for this function *)
    let function_table = {
      variables = StringMap.empty;
      parent = None;
    }
    in

    let function_table = List.fold_left check_statement function_table func.body;
    in

    let rec convert_func_statement = function
      Expr e -> SExpr(convert_expr function_table e)
    | StmtVDecl(ty, id, exp) -> convert_vardecl function_table (ty, id, exp) (* Returns a SStmtVDecl *)
    | Block sl ->
        let rec convert_statement_list = function
            [Return _ as s] -> [convert_func_statement s]
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> convert_statement_list (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> convert_func_statement s :: convert_statement_list ss
          | []              -> []
        in SBlock(convert_statement_list sl)
    | If(p, b1, b2) -> SIf(check_bool_expr p, convert_func_statement b1, convert_func_statement b2)
    | For(e1, e2, e3, st) ->
      SFor(convert_expr function_table e1, check_bool_expr e2, convert_expr function_table e3, convert_func_statement st)
    | ForDecl(e1_ty, e1_id, e1_ex, e2, e3, st) ->
        let (e_ty, e') = convert_expr function_table e1_ex in
        let same_type = e1_ty = e_ty in
        if same_type then
          SForDecl(e_ty, e1_id, (e_ty, e'), check_bool_expr e2, convert_expr function_table e3, convert_func_statement st)
          (* TODO: fix to add better error message *)
          else raise(Failure("Expected different type of expression."))
    | ForEach(t, e1, e2) ->
      SForEach(t, convert_expr function_table e1, convert_expr function_table e2)
    | While(e, st) ->
      SWhile(convert_expr function_table e, convert_func_statement st)
    | Return e ->
      raise (
        Failure("Cannot return unless inside of a function.")
      (*Failure ("return gives " ^ string_of_typ t ^ " expected " ^
         string_of_typ func.typ ^ " in " ^ string_of_expr e) *)
      )

    (* Return a semantically-checked expression, i.e., with a type *)


    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sbody = match convert_func_statement (Block func.body) with
	SBlock(sl) -> sl
      | _ -> let err = "internal error: block didn't become a block?"
      in raise (Failure err)
    }
  in (List.map check_function functions, statements')
