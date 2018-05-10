(* Semantic checking for the Joel compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)
module E = Exceptions

let check (funcs, statements) =
  let dummy = { typ = Num;
                fname = "_test";
                formals = [];
                body = [];
              }
  in
  let check_binds (to_check : bind list) = 
    let check_it checked binding = 
      let void_err = "illegal void " ^  snd binding
      and dup_err = "duplicate " ^ snd binding
      in match binding with
        (Void, _) -> raise (Failure void_err) 
      | (_, n1) -> match checked with
                    (* No duplicate bindings *)
                      ((_, n2) :: _) when n1 = n2 -> raise (Failure dup_err)
                    | _ -> binding :: checked
    in let _ = List.fold_left check_it [] (List.sort compare to_check) 
       in to_check
  in 
  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
      let add_bind map (name, ty) = StringMap.add name {
        typ = Void; fname = name;
        formals = [(ty, "x")];
        body = [] } map
      in List.fold_left add_bind StringMap.empty [  ("printf", Num);
                                                    ("printb", Bool);
                                                    ("print", String) ]
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
          | _ -> raise (E.UndefinedId(name))
    in

    (* Map a variable's name to its type in the symbol table. *)
    let add_variable (scope: symbol_table ref) t n =
      try let _ = StringMap.find n !scope.variables in raise (E.DuplicateVariable(n))
      with Not_found ->
        scope := {
          variables = StringMap.add n t !scope.variables;
          parent = !scope.parent;
        }
    in

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
    let functions =
      List.fold_left add_func built_in_decls funcs
    in

      (* Return a function from our symbol table *)
    let find_func s = 
      try StringMap.find s functions
      with Not_found -> raise (Failure ("unrecognized function " ^ s))
    in

    (* Raise an exception if the given rvalue type cannot be assigned to
     the given lvalue type *)
    let check_assign lvaluet rvaluet =
      if lvaluet = rvaluet then lvaluet
      else raise (E.InvalidAssignment)
    in

    (* Convert an expr to a sexpr. *)
    let rec convert_expr scope exp = match exp with
      StringLiteral s -> (String, SStringLiteral s)
    | IntegerLiteral s -> (Num, SIntegerLiteral s)
    | FloatLiteral s -> (Num, SFloatLiteral s)
    | BoolLiteral s -> (Bool, SBoolLiteral s)
    | TableLiteral rows ->
      if List.length rows < 1 then (Table([Void]), SNoexpr)
      else
        let rec transpose lst = match lst with
          | []             -> []
          | []   :: xss    -> transpose xss
          | (x::xs) :: xss ->
            (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
        in
        let rows' = List.map (List.rev) rows in
        let transposed_table = transpose rows' in
        (* let (test_ty, _) = convert_expr scope (List.hd (List.hd transposed_table)) in *)
        (* let () =
          let print_all item =
            let (test_ty, _) = convert_expr scope (List.hd item) in
            Printf.printf "%s\n" (string_of_typ (test_ty)) in
            ignore(List.map print_all transposed_table)
        in *)
        let check_row row = convert_expr scope (ListLiteral row) in
        let row_type item =
          let (ty, _) = convert_expr scope (List.hd item) in ty
        in
        (Table(List.map row_type transposed_table), STableLiteral(List.map check_row transposed_table))
    | ListLiteral row ->
      let (ty, _) =
        if List.length row > 0 then convert_expr scope (List.hd row)
        else (Void, SStringLiteral "")
      in
      let check_type e =
        let (ty2, e') = convert_expr scope e in
        if ty2 = ty then (ty2, e') else raise(E.MixedTypes)
      in
      (List(ty), SListLiteral(List.map check_type row))
    | ListAccess(e1, e2) ->
      let (t1, e1') = convert_expr scope e1
      and (t2, e2') = convert_expr scope e2 in
      let inner_ty = match t1 with
          List(ty) -> ty
        | _ -> raise( E.NonListAccess )
      in
      let is_valid = t2 = Num in
      if is_valid then (inner_ty, SListAccess((t1, e1'), (t2, e2')))
      else raise(E.NonNumIndex)
    | TableAccess(e1, i) ->
      let (t1, e1') = convert_expr scope e1 in
      let inner_ty = match t1 with
          Table(ty) -> ty
        | _ -> raise( E.NonListAccess )
      in
      let is_table = match t1 with
          Table(_) -> true
        | _ -> false
      in
      if is_table then
        (List(List.nth inner_ty i), STableAccess((t1, e1'), i))
      else raise(E.NonTableAccess)
    | Length(e) ->
      let (ty, e') =
        convert_expr scope e
      in let se = match ty with
          List(ty) -> ( Num, SLength (ty, e') )
          | _ -> raise( E.NonListLength )
      in se
    | Binop(e1, op, e2) ->
      let (t1, e1') = convert_expr scope e1
      and (t2, e2') = convert_expr scope e2 in
      let same_type = t1 = t2 in
      let ty = match op with
          Add | Sub | Mult | Div | Mod when same_type && t1 = Num -> Num
        | Equal | Neq when same_type -> Bool
        | Less | Leq | Greater | Geq when same_type && t1 = Num -> Bool
        | And | Or | Xor when same_type && t1 = Bool -> Bool
        | Add when same_type && t1 = String -> String
        | _ -> raise (E.InvalidBinaryOperation)
          (* Failure ("illegal binary operator " ^   <<<<< pretty print needed
                         string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                         string_of_typ t2 ^ " in " ^ string_of_expr e)) *)
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Unop(op, e1) ->
        let (t, e1') = convert_expr scope e1 in
        let ty = match op with
          Neg when t = Num -> Num
        | Not when t = Bool -> Bool
        | _ -> raise (E.InvalidUnaryOperation)
            (* Failure ("illegal unary operator " ^  <<<<< need pretty print
            string_of_uop op ^ string_of_typ t ^
                     " in " ^ string_of_expr ex) *)
        in (ty, SUnop(op, (t, e1')))
    | Pop(id, op) ->
      let t = find_variable scope id in
      let ty = match op with
          Inc when t = Num -> Num
        | Dec when t = Num -> Num
        | _ -> raise (E.InvalidPostOperation)
      in (ty, SPop(id, op))
    | Assign(id, e) ->
        let lt = find_variable scope id
        and (rt, e') = convert_expr scope e in
        (check_assign lt rt, SAssign(id, (rt, e')))
    | AssignOp(id, op, e) ->
        let lt = find_variable scope id
        and (rt, e') = convert_expr scope e in
        let same_type = lt = rt in
        let ty = match op with
            Add | Sub | Mult | Div | Mod when same_type && lt = Num -> Num
          | Add when same_type && lt = String -> String
          | _ -> raise (E.InvalidAssignmentOperation(id))
          in (ty, SAssignOp(id, op, (rt, e')))
    | Noexpr -> (Void, SNoexpr)
    | Id s -> (find_variable scope s, SId s)
    | In s -> (Table([Void]), SIn s)
    | Call(fname, args) ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          (* need pretty printing functions *)
          (*raise (Failure ("expecting " ^ string_of_int param_length ^
                                " arguments in " ^ string_of_expr call))*)
          raise (E.WrongNumberOfArguments)
        else let check_call (ft, _) e =
          let (et, e') = convert_expr scope e
          in (check_assign ft et, e')
        in
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(fname, args'))
  in

  (* Convert a vardecl to an SStmtVDecl; add the variable and its type
     to the symbol table. *)
  let convert_vardecl scope (ty, id, e) =
    let (e_ty, e') = convert_expr scope e in
    let same_type = ty = e_ty in
    if same_type then
    let _ = add_variable scope ty id
    in SStmtVDecl(e_ty, id, (e_ty, e'))
    (* TODO: fix to add better error message *)
    else
      match e_ty with
        List(t) ->
        if t = Void &&
           (ty = List(Num) || ty = List(Bool) || ty = List(String) )
        then
          let _ = add_variable scope ty id
        in SStmtVDecl(ty, id, (ty, e'))
        else raise(E.InvalidAssignment)
      | Table(lst) ->
        if lst = [Void] || lst = [] then
          let _ = add_variable scope ty id
          in SStmtVDecl(ty, id, (ty, e'))
        else raise(E.TestException(string_of_typ e_ty))
      | Void -> 
          let ty' = match e with 
        (* If we see a call to input, just let it pass through 
         * with the user assigned type, let it be handled at runtime
         * *)
            Call(fname, _) -> 
              if fname = "input" 
              then ty
              else raise(E.InvalidAssignment) 
          | _ -> raise(E.InvalidAssignment)
          in let _ = add_variable scope ty id
          in SStmtVDecl(ty', id, (ty', e'))
      | _ -> raise(E.InvalidAssignment)
  in

  let check_bool_expr scope e =
    let (t', e') = convert_expr scope e
    in if t' != Bool then raise (E.WrongType("bool")) else (t', e')
  in

  (* Convert a statement to an sstmt. *)
  let rec convert_statement scope expr fdecl = match expr with
    Expr e -> SExpr(convert_expr scope e)
  | StmtVDecl(ty, id, exp) -> convert_vardecl scope (ty, id, exp) (* Returns a SStmtVDecl *)
  | Append(e1, e2) ->
    let (t1, e1') = convert_expr scope e1
    and (t2, e2') = convert_expr scope e2 in
    let inner_ty = match t1 with
        List(ty) -> ty
      | _ -> raise(E.InvalidArgument)
    in let same_type = inner_ty = t2 in
    if same_type then SAppend((t1, e1'), (t2, e2'))
    else raise E.InvalidArgument
  | TableAppend(e1, e2) ->
    convert_statement scope (Block( (List.mapi (fun i _ -> Append(TableAccess(e1, i), List.nth e2 i)) e2) ) ) fdecl
  | Out(e) -> 
      let (t, e') = convert_expr scope e in
      let inner_tys = match t with 
          Table(tys) -> tys
        | _ -> raise(E.InvalidArgument) 
      in let column_lists = List.mapi (fun i x -> (List(x), STableAccess((t, e'), i))) inner_tys
      in SOut column_lists
  | Alter(e1, e2, e3) ->
    let (t1, e1') = convert_expr scope e1
    and (t2, e2') = convert_expr scope e2
    and (t3, e3') = convert_expr scope e3 in
    let inner_ty = match t1 with
        List(ty) -> ty
      | _ -> raise(E.InvalidArgument)
    in let is_valid = inner_ty = t3 && t2 = Num in
    if is_valid then SAlter((t1, e1'), (t2, e2'), (t3, e3'))
    else raise E.InvalidArgument
  | Block sl ->
      let new_scope = {
              variables = StringMap.empty;
              parent = Some(!scope);
      }
      in let new_scope_r = ref new_scope in
      let rec convert_statement_list = function
          [Return _ as s] -> [convert_statement new_scope_r s fdecl]
        | Return _ :: _   -> raise (E.NothingAfterReturn)
        | Block sl :: ss  -> convert_statement_list (sl @ ss) (* Flatten blocks *)
        | s :: ss         -> convert_statement new_scope_r s fdecl :: convert_statement_list ss
        | []              -> []
      in SBlock(List.rev (convert_statement_list (List.rev sl)))
  | If(p, b1, b2) -> SIf(check_bool_expr scope p, convert_statement scope b1 fdecl, convert_statement scope b2 fdecl)
  | For(e1, e2, e3, st) ->
    SFor(convert_expr scope e1, check_bool_expr scope e2, convert_expr scope e3, convert_statement scope st fdecl)
  | ForDecl(e1_ty, e1_id, e1_ex, e2, e3, st) ->
      let (e_ty, e') = convert_expr scope e1_ex in
      let same_type = e1_ty = e_ty in
      if same_type then
        SForDecl(e_ty, e1_id, (e_ty, e'), check_bool_expr scope e2, convert_expr scope e3, convert_statement scope st fdecl)
        (* TODO: fix to add better error message *)
        else raise(E.InvalidAssignment)
  | ForEach(t, var_name, e2, body) ->
    let _ = add_variable scope t var_name in
      SForEach(t, var_name, convert_expr scope e2, convert_statement scope body fdecl)
  | While(e, st) ->
    SWhile(convert_expr scope e, convert_statement scope st fdecl)


  | Return e -> match fdecl.fname with
             "_test" -> raise (E.ReturnsOutsideFunction)

            | _      -> let (t, e') = convert_expr scope e 
                        in
                        if t = fdecl.typ 
                        then SReturn (t, e') 
                        else raise (
                          Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                          string_of_typ fdecl.typ ^ " in " ^ string_of_expr e))
  in


  let convert_statements statement =
    convert_statement global_scope statement dummy
  in
  let statements' = 
    List.map convert_statements (List.rev statements)
  in 
  let convert_function_statements fxn func_scope statement = 
     convert_statement func_scope statement fxn
  in 

  let check_function func = 
    let formals' = 
      check_binds func.formals
    in
    let formals'm = 
      List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                  StringMap.empty formals'
    in
    let new_function_scope = 
      {
        variables = formals'm;
        parent = Some(!global_scope);
      }
    in
    let new_function_scope_r = 
      ref new_function_scope 
      in
        { styp = func.typ;
        sfname = func.fname;
        sformals = formals';
        sbody = List.map (convert_function_statements func new_function_scope_r) func.body
      }

  in (List.map check_function funcs, statements')

let rec convert_csv exp = match exp with
    StringLiteral s -> (String, SStringLiteral s)
  | IntegerLiteral s -> (Num, SIntegerLiteral s)
  | FloatLiteral s -> (Num, SFloatLiteral s)
  | TableLiteral rows ->
    if List.length rows < 1 then (Table([Void]), SNoexpr)
    else
      let rec transpose lst = match lst with
      | []             -> []
      | []   :: xss    -> transpose xss
      | (x::xs) :: xss ->
        (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)
      in
      let rows' = List.map (List.rev) rows in
      let transposed_table = transpose rows' in
      let check_row row = convert_csv (ListLiteral row) in
        let row_type item =
          let (ty, _) = convert_csv (List.hd item) in ty
        in
        (Table(List.map row_type transposed_table), STableLiteral(List.map check_row transposed_table))
  | ListLiteral row ->
    let (ty, _) =
      if List.length row > 0 then convert_csv (List.hd row)
      else (Void, SStringLiteral "")
    in
    let check_type e =
      let (ty2, e') = convert_csv e in
      if ty2 = ty then (ty2, e') else raise(E.MixedTypes)
    in
    (List(ty), SListLiteral(List.map check_type row))
  | _ -> raise(Failure "Invalid csv type")

