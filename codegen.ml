(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Data structure to represent the current scope and its parent. *)
type var_table = {
  names: L.llvalue StringMap.t; (* Names bound in current block *)
  parent: var_table option; (* Enclosing scope *)
}

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let trans (functions, statements) =

  let context    = L.global_context () in

  (* Add types to the context so we can use them in our LLVM code *)
  let num_t      = L.double_type  context (* num type *)
  and i32_t      = L.i32_type     context (* int type (for returns) *)
  and i8_t       = L.i8_type      context (* pointer type *)
  and bool_t       = L.i1_type    context (* boolean type *)
  and void_t     = L.void_type    context (* void type *)

  (* Create an LLVM module - a container for our code *)
  and the_module = L.create_module context "Joel" in

  (* Convert Joel types to LLVM types *)
  let ltype_of_typ = function
      A.Num   -> num_t
    | A.Bool  -> bool_t
    | A.Void  -> void_t
    | _ -> raise (Failure ("Error: Not Yet Implemented"))
  in

  (* If a variable is declared but not assigned a value, give it a placeholder value
    according to its type so we can store it in the symbol table. *)
  let get_init_noexpr = function
      A.Num -> L.const_float num_t 0.0
    | A.Bool -> L.const_int bool_t 0
    | _ -> raise (Failure ("Error: Not Yet Implemented"))
  in

  (* Declare the printf builtin function *)
  let printf_t = L.var_arg_function_type num_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Build a "main" function to enclose all statements in the program *)
  let main_ty = L.function_type i32_t [||] in
  let the_function = L.define_function "main" main_ty the_module in

  (* Add a terminator instruction f to wherever builder is located. *)
  let add_terminal builder f =
    match L.block_terminator (L.insertion_block builder) with
       Some _ -> ()
      | None -> ignore (f builder)
  in

  (* Add a "return 0" statement to the end of a function (used to terminate the main function) *)
  let make_return builder =
    let t = L.build_ret (L.const_int i32_t 0) in 
      add_terminal builder t
  in 

  (* Iterate through the list of semantically-checked statements, generating code for each one. *)
  let build_program_body statements =
    let builder = L.builder_at_end context (L.entry_block the_function) in
      let str_format_str  = L.build_global_stringptr "%s\n" "fmt" builder 
      and int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
      and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder
      in

      (* Define the global variable table. *)
      let variable_table = {
        names = StringMap.empty;
        parent = None;
      } 

      (* Get a reference to the global table we just created.
        We will pass this scope via reference through recursive calls
        and mutate it when we need to add a new variable. *)
      in let global_scope = ref variable_table 
      in

      (* Find a variable, beginning in a given scope and searching upwards. *)
      let rec find_variable (scope: var_table ref) name = 
      try StringMap.find name !scope.names
      with Not_found ->
        match !scope.parent with
            Some(parent) -> find_variable (ref parent) name
          | _ -> print_string ("Lookup error: " ^ name); raise Not_found
      in

      (* Generate LLVM code for an expression; return its value *)
      let rec expr builder scope (t, e) = match e with
        | SIntegerLiteral i -> L.const_float num_t (float_of_int i)
        | SFloatLiteral f -> L.const_float num_t (float_of_string f)
        | SBoolLiteral b -> L.const_int bool_t (if b then 1 else 0)
        | SId id -> L.build_load (find_variable scope id) id builder
        | SAssign (n, e) -> update_variable scope n e; expr builder scope (t, SId(n)) (* Update the variable; return its new value *)
        | SAssignOp (n, op, e) -> expr builder scope (t, SAssign(n, (t, SBinop((t, SId(n)), op, e)))) (* expand expression - i.e. a += 1 becomes a = a + 1 *)
        | SPop (n, pop) -> let prev = expr builder scope (t, SId(n)) in (* expand expression - i.e. a++ becomes a = a + 1, and we return a's prev. value *)
          ignore(expr builder scope (t, SAssign(n, (t, SBinop((t, SId(n)), (
            match pop with 
              A.Inc -> A.Add
            | A.Dec -> A.Sub
          ), (t, SIntegerLiteral(1))))))); prev

        | SBinop (e1, op, e2) ->
         let (t, _) = e1
         and e1' = expr builder scope e1
         and e2' = expr builder scope e2 in
         if t = A.Num then (match op with
          A.Add     -> L.build_fadd 
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul 
          | A.Div     -> L.build_fdiv  (* Todo: modulo *)
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | _ -> raise (Failure ("Error: Not Yet Implemented"))
           ) e1' e2' "tmp" builder
         else (match op with
            A.And -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure ("Error: Not Yet Implemented"))
           ) e1' e2' "tmp" builder

        | SCall ("printf", [e]) -> L.build_call printf_func [| float_format_str ; (expr builder scope e) |] "printf" builder
        | SCall("printb", [e]) -> L.build_call printf_func [| int_format_str ; (expr builder scope e) |] "printf" builder
        | _ -> raise (Failure ("Error: Not Yet Implemented"))
      
      (* Construct code for a variable assigned in the given scope. 
        Allocate on the stack, initialize its value, if appropriate, 
        and mutate the given map to remember its value. *)
      and add_variable (scope: var_table ref) t n e = 
        let e' = let (_, ex) = e in match ex with
            SNoexpr -> get_init_noexpr t
          | _ -> expr builder scope e
        in L.set_value_name n e';
        let l_var = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store e' l_var builder);
        scope := {
          names = StringMap.add n l_var !scope.names;
          parent = !scope.parent;
        }

      (* Update a variable, beginning in the given scope.
        Bind the nearest occurrence of the variable to the given
        new value, and mutate the given map to remember its value. *)
      and update_variable (scope: var_table ref) name e =
      try let e' = expr builder scope e in
        let l_var = find_variable scope name 
        in ignore (L.build_store e' l_var builder);
        scope := {
          names = StringMap.add name l_var !scope.names;
          parent = !scope.parent;
        }
      with Not_found -> (* If variable is not in this scope, check the parent scope *)
        match !scope.parent with
            Some(parent) -> ignore(find_variable (ref parent) name); ()
          | _ -> print_string ("Update error: " ^ name); raise Not_found
      in

      (* Build a single statement. Should return a builder. *)
      let rec build_statement scope stmt builder = match stmt with
            SExpr e -> let _ = expr builder scope e in builder
          | SBlock sl ->
            let new_scope = {
              names = StringMap.empty;
              parent = Some(!scope);
            }
            in let new_scope_r = ref new_scope in 
            let build builder stmt = build_statement new_scope_r stmt builder
            in List.fold_left build builder sl 
          | SStmtVDecl(t, n, e) -> let _ = add_variable scope t n e in builder          
          | SIf (predicate, then_stmt, else_stmt) ->
            let bool_val = expr builder scope predicate in
            (* create merge bb  *)
            let merge_bb = L.append_block context "merge" the_function in
              let branch_instr = L.build_br merge_bb in
            (* create then bb *)
            let then_bb = L.append_block context "then" the_function in
              let then_builder = build_statement scope then_stmt (L.builder_at_end context then_bb) in
              let () = add_terminal then_builder branch_instr in

            (* create else bb *)
            let else_bb = L.append_block context "else" the_function in
              let else_builder = build_statement scope else_stmt (L.builder_at_end context else_bb) in
              let () = add_terminal else_builder branch_instr in

            let _ = L.build_cond_br bool_val then_bb else_bb builder in
              (* Return a builder pointing at this new merge block.
                 It's now our main block. *)
              L.builder_at_end context merge_bb

          | _ as t ->
            let str = Sast.string_of_sstmt t in
              Printf.printf "type: %s." str; raise (Failure ("Error: Not Yet Implemented"))
      in

        let statement_reducer builder stmt = build_statement global_scope stmt builder in
          (* Builder gets updated with each call *)
          let final_builder = List.fold_left statement_reducer builder statements in
            make_return final_builder; ()
        (* List.iter (fun stmt -> let _ = build_statement global_scope stmt builder in ()) statements; make_return builder; () *)

  in
    build_program_body (statements);
    the_module

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
(*let translate (globals, functions) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context 
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "MicroC" in

  (* Convert MicroC types to LLVM types *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
  in

  (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Define each function (arguments and return type) so we can 
   * define it's body and call it later *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = 
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = 
        let () = L.set_value_name n p in
  let local = L.build_alloca (ltype_of_typ t) n builder in
        let _  = L.build_store p local builder in
  StringMap.add n local m 
      in

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      let add_local m (t, n) =
  let local_var = L.build_alloca (ltype_of_typ t) n builder
  in StringMap.add n local_var m 
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals 
    in

    (* Return the value for a variable or formal argument. First check
     * locals, then globals *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder (_, e) = match e with
  SLiteral i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFliteral l -> L.const_float_of_string float_t l
      | SNoexpr -> L.const_int i32_t 0
      | SId s -> L.build_load (lookup s) s builder
      | SBinop (e1, op, e2) ->
    let (t, _) = e1
    and e1' = expr builder e1
    and e2' = expr builder e2 in
    if t = A.Float then (match op with 
      A.Add     -> L.build_fadd
    | A.Sub     -> L.build_fsub
    | A.Mult    -> L.build_fmul
    | A.Div     -> L.build_fdiv 
    | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
    | A.Neq     -> L.build_fcmp L.Fcmp.One
    | A.Less    -> L.build_fcmp L.Fcmp.Olt
    | A.Leq     -> L.build_fcmp L.Fcmp.Ole
    | A.Greater -> L.build_fcmp L.Fcmp.Ogt
    | A.Geq     -> L.build_fcmp L.Fcmp.Oge
    | A.And | A.Or ->
        raise (Failure "internal error: semant should have rejected and/or on float")
    ) e1' e2' "tmp" builder 
    else (match op with
    | A.Add     -> L.build_add
    | A.Sub     -> L.build_sub
    | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
    | A.And     -> L.build_and
    | A.Or      -> L.build_or
    | A.Equal   -> L.build_icmp L.Icmp.Eq
    | A.Neq     -> L.build_icmp L.Icmp.Ne
    | A.Less    -> L.build_icmp L.Icmp.Slt
    | A.Leq     -> L.build_icmp L.Icmp.Sle
    | A.Greater -> L.build_icmp L.Icmp.Sgt
    | A.Geq     -> L.build_icmp L.Icmp.Sge
    ) e1' e2' "tmp" builder
      | SUnop(op, e) ->
    let (t, _) = e and e' = expr builder e in
    (match op with
      A.Neg when t = A.Float -> L.build_fneg 
    | A.Neg                  -> L.build_neg
          | A.Not                  -> L.build_not) e' "tmp" builder
      | SAssign (s, e) -> let e' = expr builder e in
                          let _  = L.build_store e' (lookup s) builder in e'
      | SCall ("print", [e]) | SCall ("printb", [e]) ->
    L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder
      | SCall ("printbig", [e]) ->
    L.build_call printbig_func [| (expr builder e) |] "printbig" builder
      | SCall ("printf", [e]) -> 
    L.build_call printf_func [| float_format_str ; (expr builder e) |]
      "printf" builder
      | SCall (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
   let actuals = List.rev (List.map (expr builder) (List.rev act)) in
   let result = (match fdecl.styp with 
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in
    
    (* Each basic block in a program ends with a "terminator" instruction i.e.
    one that ends the basic block. By definition, these instructions must
    indicate which basic block comes next -- they typically yield "void" value
    and produce control flow, not values *)
    (* Invoke "f builder" if the current block doesn't already
       have a terminator (e.g., a branch). *)
    let add_terminal builder f =
                           (* The current block where we're inserting instr *)
      match L.block_terminator (L.insertion_block builder) with
  Some _ -> ()
      | None -> ignore (f builder) in
  
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    (* Imperative nature of statement processing entails imperative OCaml *)
    let rec stmt builder = function
  SBlock sl -> List.fold_left stmt builder sl
        (* Generate code for this expression, return resulting builder *)
      | SExpr e -> let _ = expr builder e in builder 
      | SReturn e -> let _ = match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder 
                              (* Build return statement *)
                            | _ -> L.build_ret (expr builder e) builder 
                     in builder
      (* The order that we create and add the basic blocks for an If statement
      doesnt 'really' matter (seemingly). What hooks them up in the right order
      are the build_br functions used at the end of the then and else blocks (if
      they don't already have a terminator) and the build_cond_br function at
      the end, which adds jump instructions to the "then" and "else" basic blocks *)
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
         (* Add "merge" basic block to our function's list of blocks *)
   let merge_bb = L.append_block context "merge" the_function in
         (* Partial function used to generate branch to merge block *) 
         let branch_instr = L.build_br merge_bb in

         (* Same for "then" basic block *)
   let then_bb = L.append_block context "then" the_function in
         (* Position builder in "then" block and build the statement *)
         let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
         (* Add a branch to the "then" block (to the merge block) 
           if a terminator doesn't already exist for the "then" block *)
   let () = add_terminal then_builder branch_instr in

         (* Identical to stuff we did for "then" *)
   let else_bb = L.append_block context "else" the_function in
         let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
   let () = add_terminal else_builder branch_instr in

         (* Generate initial branch instruction perform the selection of "then"
         or "else". Note we're using the builder we had access to at the start
         of this alternative. *)
   let _ = L.build_cond_br bool_val then_bb else_bb builder in
         (* Move to the merge block for further instruction building *)
   L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
          (* First create basic block for condition instructions -- this will
          serve as destination in the case of a loop *)
    let pred_bb = L.append_block context "while" the_function in
          (* In current block, branch to predicate to execute the condition *)
    let _ = L.build_br pred_bb builder in

          (* Create the body's block, generate the code for it, and add a branch
          back to the predicate block (we always jump back at the end of a while
          loop's body, unless we returned or something) *)
    let body_bb = L.append_block context "while_body" the_function in
          let while_builder = stmt (L.builder_at_end context body_bb) body in
    let () = add_terminal while_builder (L.build_br pred_bb) in

          (* Generate the predicate code in the predicate block *)
    let pred_builder = L.builder_at_end context pred_bb in
    let bool_val = expr pred_builder predicate in

          (* Hook everything up *)
    let merge_bb = L.append_block context "merge" the_function in
    let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
    L.builder_at_end context merge_bb

      (* Implement for loops as while loops! *)
      | SFor (e1, e2, e3, body) -> stmt builder
      ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module *)
