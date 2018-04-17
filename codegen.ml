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
let trans (_, statements) =

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
        | SStringLiteral s -> L.build_global_stringptr s "string" builder
        | SId id -> L.build_load (find_variable scope id) id builder
        | SAssign (n, e) -> update_variable scope n e builder; expr builder scope (t, SId(n)) (* Update the variable; return its new value *)
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
        | SCall("print", [e]) -> L.build_call printf_func [| str_format_str ; (expr builder scope e) |] "printf" builder
        | _ -> raise (Failure ("Error: Not Yet Implemented"))
      
      (* Construct code for a variable assigned in the given scope. 
        Allocate on the stack, initialize its value, if appropriate, 
        and mutate the given map to remember its value. *)
      and add_variable (scope: var_table ref) t n e builder = 
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
      and update_variable (scope: var_table ref) name e builder =
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

          | SStmtVDecl(t, n, e) -> let _ = add_variable scope t n e builder in builder 

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

          | SWhile (predicate, body) ->
            (* predicate block -- checks the condition. *)
            let pred_bb = L.append_block context "while" the_function in
              let _ = L.build_br pred_bb builder in
                (* generate predicate code *)
                let pred_builder = L.builder_at_end context pred_bb in
                  let bool_val = expr pred_builder scope predicate in
                    let body_bb = L.append_block context "while_body" the_function in
                      let while_builder = build_statement scope body (L.builder_at_end context body_bb) in
                        let () = add_terminal while_builder (L.build_br pred_bb) in                      
                    let merge_bb = L.append_block context "merge" the_function in
                    let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
                      L.builder_at_end context merge_bb

          | SFor(e1, e2, e3, body) -> build_statement scope
              ( SBlock [SExpr e1 ;
                        SWhile(e2, SBlock[ body ; 
                                           SExpr e3] ) ] ) builder

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