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
module I64 = Int64
module A = Ast
open Sast

module StringMap = Map.Make(String)
module E = Exceptions

(* Data structure to represent the current scope and its parent. *)
type var_table = {
  names: L.llvalue StringMap.t; (* Names bound in current block *)
  parent: var_table ref option; (* Enclosing scope *)
}

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let trans (_, statements) =

  let context    = L.global_context () in

  (* Add types to the context so we can use them in our LLVM code *)
  let num_t      = L.double_type  context (* num type *)
  and i32_t      = L.i32_type     context (* int type (for returns) *)
  and i8_t       = L.i8_type      context (* pointer type *)
  and bool_t     = L.i1_type      context (* boolean type *)
  and str_t      = L.pointer_type (L.i8_type context) (* string type *)
  and void_t     = L.void_type    context (* void type *)


  (* Create an LLVM module - a container for our code *)
  and the_module = L.create_module context "Joel" in

  let num_list_item_struct = L.named_struct_type context "num_list_item" in
  let bool_list_item_struct = L.named_struct_type context "bool_list_item" in
  let string_list_item_struct = L.named_struct_type context "string_list_item" in

  let num_list_item_pointer =
    L.pointer_type num_list_item_struct
  in
  let bool_list_item_pointer =
    L.pointer_type bool_list_item_struct
  in
  let string_list_item_pointer =
    L.pointer_type string_list_item_struct
  in

  let pack_struct struct_type arg_list =
    L.struct_set_body struct_type (Array.of_list arg_list) true
  in

  let () =
    pack_struct num_list_item_struct [num_t; num_list_item_pointer; bool_t]
  in
  let () =
    pack_struct bool_list_item_struct [bool_t; bool_list_item_pointer; bool_t]
  in
  let () =
    pack_struct string_list_item_struct [str_t; string_list_item_pointer; bool_t]
  in


  let get_list_type ty = match ty with
      A.Num -> num_list_item_struct
    | A.Bool -> bool_list_item_struct
    | A.String -> string_list_item_struct
    | _ -> raise(E.InvalidListType)
  in

  let get_list_pointer_type ty = match ty with
      A.Num -> num_list_item_pointer
    | A.Bool -> bool_list_item_pointer
    | A.String -> string_list_item_pointer
    | _ -> raise(Failure("here: " ^ A.string_of_typ ty)) (* this one is where the error is happening. *)
  in
  let table_struct tys =
    L.struct_type context (Array.of_list (List.map get_list_pointer_type tys))
  in

  let table_struct_pointer tys =
    L.pointer_type (table_struct tys)
  in

  (* Convert Joel types to LLVM types *)
  let ltype_of_typ ty = match ty with
      A.Num   -> num_t
    | A.Bool  -> bool_t
    | A.Void  -> void_t
    | A.String -> str_t
    | A.List(t) -> get_list_pointer_type t
    | A.Table(t) -> table_struct_pointer t
    | _ -> raise (Failure ("Error: Not Yet Implemented " ^ (A.string_of_typ ty)))
  in


  let list_end_item ty e1 =
    let list_item_struct = get_list_type ty in
    L.const_named_struct list_item_struct
      (Array.of_list [e1; L.const_pointer_null (L.pointer_type list_item_struct); L.const_int bool_t 0])
  in

  let list_terminator ty =
    let list_item_struct = get_list_type ty in
    L.const_named_struct list_item_struct (Array.of_list [L.const_null (ltype_of_typ ty); L.const_pointer_null (L.pointer_type list_item_struct); L.const_int bool_t 1])
  in


  let null_table tys =
    let new_table_type = table_struct tys in
    L.const_null new_table_type
  in

  let empty_table tys builder =
    let new_table_type = table_struct tys in
    let store_list_terminator ty =
      let item = list_terminator ty in
      let item_pointer =
        L.build_alloca (get_list_type ty) "TEMP" builder
      in
      let () =
        ignore(L.build_store item item_pointer builder)
      in
      item_pointer
    in
    let list_heads = List.map store_list_terminator tys in
    let null_table = L.const_null new_table_type in
    let new_table =
      L.build_alloca (table_struct tys) "TEMP" builder
    in
    let () =
      ignore(L.build_store null_table new_table builder)
    in
    let rec initialize_list_head i =
      if i>=0 then
        let list_head = List.nth list_heads i in

        let pointer_to_item =
          L.build_struct_gep new_table i "TEMP" builder
        in
        let () =
          ignore(L.build_store list_head pointer_to_item builder)
        in
        initialize_list_head (i-1)
      else new_table
    in
    initialize_list_head ((List.length list_heads) - 1)
  in


  (* If a variable is declared but not assigned a value, give it a placeholder value
    according to its type so we can store it in the symbol table. *)
  let get_init_noexpr = function
      A.Num -> L.const_float num_t 0.0
    | A.Bool -> L.const_int bool_t 0
    | A.String -> L.const_pointer_null str_t
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

  (* BAKED IN UTILITY FUNCTIONS *)

  (* START OF LIST ACCESS FUNCTION DEFINITION *)
  let build_list_access_function ty =
    let list_item_pointer = get_list_pointer_type ty in
    let list_item_struct = get_list_type ty in
    let list_access_function =
      L.define_function
        ("_LIST_ACCESS_" ^ (A.string_of_typ ty)) (
          L.function_type
          list_item_pointer
          (Array.of_list [list_item_pointer; num_t])
        )
        the_module
    in
    let list_access_function_builder = L.builder_at_end context (L.entry_block list_access_function) in
    let copy_of_mem_addr =
      L.build_alloca list_item_struct "TEMP" list_access_function_builder
    in
    let orig_mem_addr =
      L.build_load (L.param list_access_function 0) "TEMP" list_access_function_builder
    in
    let () =
      ignore(L.build_store orig_mem_addr copy_of_mem_addr list_access_function_builder)
    in
    let pointer_to_head =
      L.build_alloca list_item_pointer "TEMP" list_access_function_builder
    in
    let () =
      ignore(L.build_store copy_of_mem_addr pointer_to_head list_access_function_builder)
    in
    let loaded_pointer_to_head =
      L.build_load pointer_to_head "TEMP" list_access_function_builder
    in
    let iterator_alloc = L.build_alloca num_t "TEMP" list_access_function_builder in
    let () =
      ignore(L.build_store (L.const_float num_t 0.0) iterator_alloc list_access_function_builder)
    in
    let pred_bb = L.append_block context "while" list_access_function in
    let _ = L.build_br pred_bb list_access_function_builder in
    let pred_builder = L.builder_at_end context pred_bb in
    let comparison =
      L.build_fcmp L.Fcmp.Olt (L.build_load iterator_alloc "TEMP" pred_builder) (L.param list_access_function 1) "TEMP" pred_builder
    in
    let body_bb = L.append_block context "while_body" list_access_function in
    let while_builder = L.builder_at_end context body_bb in
    let loaded_iterator = L.build_load iterator_alloc "TEMP" while_builder in
    let new_val =
      L.build_fadd loaded_iterator (L.const_float num_t 1.0) "TEMP" while_builder
    in
    let pointer_to_next =
      L.build_struct_gep loaded_pointer_to_head 1 "TEMP" while_builder
    in
    let loaded_pointer_to_next =
      L.build_load pointer_to_next "TEMP" while_builder
    in
    let dereferened_pointer_to_next =
      L.build_load loaded_pointer_to_next "TEMP" while_builder
    in
    let () =
      ignore(L.build_store dereferened_pointer_to_next loaded_pointer_to_head while_builder)
    in
    let () =
      ignore(L.build_store new_val iterator_alloc while_builder)
    in
    let () = add_terminal while_builder (L.build_br pred_bb) in
    let merge_bb = L.append_block context "merge" list_access_function in
    let _ = L.build_cond_br comparison body_bb merge_bb pred_builder in

    let t = L.build_ret loaded_pointer_to_head in
    let () = add_terminal (L.builder_at_end context merge_bb) t in
    list_access_function
  in
  (* END OF LIST ACCESS FUNCTION DEFINITION *)

  let list_access ty =
    match (L.lookup_function ("_LIST_ACCESS_" ^ (A.string_of_typ ty)) the_module) with
      Some(f) -> f
    | None -> build_list_access_function ty
  in

  (* START OF LIST LENGTH FUNCTION DEFINITION *)
  let build_list_length_function ty =
    let list_item_pointer = get_list_pointer_type ty in
    let list_item_struct = get_list_type ty in
    let list_length_function =
      L.define_function
        ("_LIST_LENGTH_" ^ (A.string_of_typ ty))
        (
          L.function_type
          num_t
          (Array.of_list [list_item_pointer])
        )
        the_module
    in
    let list_length_function_builder = L.builder_at_end context (L.entry_block list_length_function) in
    let copy_of_mem_addr =
      L.build_alloca list_item_struct "TEMP" list_length_function_builder
    in
    let orig_mem_addr =
      L.build_load (L.param list_length_function 0) "TEMP" list_length_function_builder
    in
    let () =
      ignore(L.build_store orig_mem_addr copy_of_mem_addr list_length_function_builder)
    in
    let pointer_to_head =
      L.build_alloca list_item_pointer "TEMP" list_length_function_builder
    in
    let () =
      ignore(L.build_store copy_of_mem_addr pointer_to_head list_length_function_builder)
    in
    let loaded_pointer_to_head =
      L.build_load pointer_to_head "TEMP" list_length_function_builder
    in
    let pointer_to_flag =
      L.build_struct_gep loaded_pointer_to_head 2 "TEMP" list_length_function_builder
    in
    let loaded_pointer_to_flag =
      L.build_load pointer_to_flag "TEMP" list_length_function_builder
    in
    let current_flag =
      L.build_alloca bool_t "TEMP" list_length_function_builder
    in
    let () =
      ignore(L.build_store loaded_pointer_to_flag current_flag list_length_function_builder)
    in
    let iterator_alloc = L.build_alloca num_t "TEMP" list_length_function_builder in
    let () =
      ignore(L.build_store (L.const_float num_t 0.0) iterator_alloc list_length_function_builder)
    in
    let pred_bb = L.append_block context "while" list_length_function in
    let _ = L.build_br pred_bb list_length_function_builder in
    let pred_builder = L.builder_at_end context pred_bb in
    let new_pointer_to_flag =
      L.build_struct_gep loaded_pointer_to_head 2 "TEMP" pred_builder
    in
    let loaded_new_pointer_to_flag =
      L.build_load new_pointer_to_flag "TEMP" pred_builder
    in
    let () =
      ignore(L.build_store loaded_new_pointer_to_flag current_flag pred_builder)
    in
    let comparison =
      L.build_icmp L.Icmp.Eq (L.build_load current_flag "TEMP" pred_builder) (L.const_int bool_t 0) "TEMP" pred_builder
    in
    let body_bb = L.append_block context "while_body" list_length_function in
    let while_builder = L.builder_at_end context body_bb in
    let loaded_iterator = L.build_load iterator_alloc "TEMP" while_builder in
    let new_val =
      L.build_fadd loaded_iterator (L.const_float num_t 1.0) "TEMP" while_builder
    in
    let pointer_to_next =
      L.build_struct_gep loaded_pointer_to_head 1 "TEMP" while_builder
    in
    let loaded_pointer_to_next =
      L.build_load pointer_to_next "TEMP" while_builder
    in
    let dereferened_pointer_to_next =
      L.build_load loaded_pointer_to_next "TEMP" while_builder
    in
    let () =
      ignore(L.build_store dereferened_pointer_to_next loaded_pointer_to_head while_builder)
    in
    let () =
      ignore(L.build_store new_val iterator_alloc while_builder)
    in
    let () = add_terminal while_builder (L.build_br pred_bb) in
    let merge_bb = L.append_block context "merge" list_length_function in
    let merge_builder = L.builder_at_end context merge_bb in
    let _ = L.build_cond_br comparison body_bb merge_bb pred_builder in
    let t = L.build_ret (L.build_load iterator_alloc "TEMP" merge_builder) in
    let () = add_terminal (L.builder_at_end context merge_bb) t in
    list_length_function
  in
  (* END OF LIST LENGTH FUNCTION DEFINITION *)
  let list_length ty =
    match (L.lookup_function ("_LIST_LENGTH_" ^ (A.string_of_typ ty)) the_module) with
      Some(f) -> f
    | None -> build_list_length_function ty
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
            Some(parent) -> find_variable (parent) name
          | _ -> print_string ("Lookup error: " ^ name); raise Not_found
      in

      let raw_list (_, e) = match e with
          SListLiteral s -> s
        | _ -> raise(Failure("invalide argument passed."))
      in

      let list_inner_type l = match l with
          A.List(ty) -> ty
        | _ -> raise(E.InvalidArgument)
      in

      (* Generate LLVM code for an expression; return its value *)
      let rec expr builder scope (t, e) = match e with
        | SIntegerLiteral i -> L.const_float num_t (float_of_int i)
        | SFloatLiteral f -> L.const_float num_t (float_of_string f)
        | SBoolLiteral b -> L.const_int bool_t (if b then 1 else 0)
        | SListLiteral _ -> build_list (list_inner_type t) (t, e) scope builder
        | STableLiteral lists ->
          let inner_ty_list = match t with
              A.Table(tys) -> tys
            | _ -> raise(E.InvalidArgument)
          in
          let make_empty_list ty = expr builder scope (A.List(ty), SListLiteral []) in
          let list_heads =
            if (List.length lists) = 0 then List.map make_empty_list inner_ty_list
            else List.map (expr builder scope) lists
          in
          let null_table = null_table inner_ty_list in
          let new_table =
            L.build_alloca (table_struct inner_ty_list) "TEMP" builder
          in
          let () =
            ignore(L.build_store null_table new_table builder)
          in
          let rec load_list_head i =
            if i>=0 then
              let list_head = List.nth list_heads i in

              let pointer_to_item =
                L.build_struct_gep new_table i "TEMP" builder
              in
              let () =
                ignore(L.build_store list_head pointer_to_item builder)
              in
              load_list_head (i-1)
            else new_table
          in
          load_list_head ((List.length list_heads) - 1)
        | SListAccess(e1, e2) ->
          let e1' = expr builder scope e1 in
          let e2' = expr builder scope e2 in
          let item =
            L.build_call (list_access t) (Array.of_list [e1'; e2']) "_FUNC_VAL" builder
          in
          let value =
            L.build_struct_gep item 0 "TEMP" builder
          in
          L.build_load value "TEMP" builder
        | STableAccess(e1, i) ->
          let e1' = expr builder scope e1 in
          let (ty, _) = e1 in
          let inner_ty_list = match ty with
              A.Table(tys) -> tys
            | _ -> raise(E.InvalidArgument)
          in

          let new_table =
            L.build_alloca (table_struct inner_ty_list) "TEMP" builder
          in
          let loaded_table =
            L.build_load e1' "TEMP" builder
          in
          let () =
            ignore(L.build_store loaded_table new_table builder)
          in

          let value =
            L.build_struct_gep new_table i "TEMP" builder
          in
          let loaded_value =
            L.build_load value "TEMP" builder
          in
          loaded_value
        | SLength(e) ->
          let (t1, _) = e in
          let e' = expr builder scope e in
          L.build_call (list_length (t1)) (Array.of_list [e']) "_FUNC_VAL" builder
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
          | _ -> raise (Failure ("Internal Error: bad numeric operation"))
           ) e1' e2' "tmp" builder
         else if t = A.Bool then (match op with
            A.And -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
          | _ -> raise (Failure ("Internal Error: bad boolean operation"))
           ) e1' e2' "tmp" builder
         (* else if t = A.String then
          in match op with
            A.Add     -> L.build_global_stringptr (str1 ^ str2) "string" builder
          | _ -> raise (Failure ("Internal Error: bad string operation")) *)
         else raise (Failure ("Internal Error: bad binop"))

        | SCall ("printf", [e]) -> L.build_call printf_func [| float_format_str ; (expr builder scope e) |] "printf" builder
        | SCall("printb", [e]) -> L.build_call printf_func [| int_format_str ; (expr builder scope e) |] "printf" builder
        | SCall("print", [e]) -> L.build_call printf_func [| str_format_str ; (expr builder scope e) |] "printf" builder
        | _ -> raise (Failure ("Error: Not Yet Implemented"))

      and build_list t e (scope: var_table ref) builder =
        let list_item_struct = get_list_type t in
        let build_link temp_var b =
          let front_item = list_end_item t (expr builder scope b) in
          let head_var = L.build_alloca list_item_struct "LIST_ITEM" builder in
          let () =
            ignore(L.build_store front_item head_var builder)
          in
          let head_pointer =
            L.build_struct_gep head_var 1 "TEMP" builder
          in
          let () =
            ignore(L.build_store temp_var head_pointer builder )
          in
          head_var
        in
        let stripped_list = raw_list e
        in
        (* build an empty item to terminate the list *)
        let empty_expr =
          list_terminator t
        in
        let empty_var = L.build_alloca list_item_struct "TEMP" builder in
        let () =
          ignore(L.build_store empty_expr empty_var builder)
        in
        List.fold_left build_link empty_var (List.rev stripped_list)

      (* Construct code for a variable assigned in the given scope.
        Allocate on the stack, initialize its value, if appropriate,
        and mutate the given map to remember its value. *)
      and add_variable (scope: var_table ref) t n e builder =
        let e' = let (_, ex) = e in match ex with
            SNoexpr -> get_init_noexpr t
          | _ -> expr builder scope e
        in L.set_value_name n e';
        let ltype = ltype_of_typ t
        in
        let l_var = L.build_alloca ltype n builder in
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
            Some(parent) -> ignore(find_variable (parent) name); ()
        | _ -> print_string ("Update error: " ^ name); raise Not_found
      in


      (* Build a single statement. Should return a builder. *)
      let rec build_statement scope stmt builder = match stmt with
            SExpr e -> let _ = expr builder scope e in builder

          | SBlock sl ->
            let new_scope = {
              names = StringMap.empty;
              parent = Some(scope);
            }
            in let new_scope_r = ref new_scope in
            let build builder stmt = build_statement new_scope_r stmt builder
            in List.fold_left build builder sl
          (* | SAppend(e1, e2, e3) ->
            let  *)

          | SStmtVDecl(t, n, e) -> let _ = add_variable scope t n e builder in builder
          | SAppend(e1, e2) ->
            let e1' = expr builder scope e1 in
            let e2' = expr builder scope e2 in
            let (t, _) = e1 in

            let list_item_pointer = get_list_pointer_type (list_inner_type t) in
            let list_item_struct = get_list_type (list_inner_type t) in
            let copy_of_mem_addr =
              L.build_alloca list_item_struct "TEMP" builder
            in
            let orig_mem_addr =
              L.build_load e1' "TEMP" builder
            in
            let () =
              ignore(L.build_store orig_mem_addr copy_of_mem_addr builder)
            in
            let pointer_to_head =
              L.build_alloca list_item_pointer "TEMP" builder
            in
            let () =
              ignore(L.build_store copy_of_mem_addr pointer_to_head builder)
            in
            let loaded_pointer_to_head =
              L.build_load pointer_to_head "TEMP" builder
            in
            let pointer_to_flag =
              L.build_struct_gep loaded_pointer_to_head 2 "TEMP" builder
            in
            let loaded_pointer_to_flag =
              L.build_load pointer_to_flag "TEMP" builder
            in
            let current_flag =
              L.build_alloca bool_t "TEMP" builder
            in
            let () =
              ignore(L.build_store loaded_pointer_to_flag current_flag builder)
            in
            let iterator_alloc = L.build_alloca num_t "TEMP" builder in
            let () =
              ignore(L.build_store (L.const_float num_t 0.0) iterator_alloc builder)
            in
            let pred_bb = L.append_block context "while" the_function in
            let _ = L.build_br pred_bb builder in
            let pred_builder = L.builder_at_end context pred_bb in
            let new_pointer_to_flag =
              L.build_struct_gep loaded_pointer_to_head 2 "TEMP" pred_builder
            in
            let loaded_new_pointer_to_flag =
              L.build_load new_pointer_to_flag "TEMP" pred_builder
            in
            let () =
              ignore(L.build_store loaded_new_pointer_to_flag current_flag pred_builder)
            in
            let comparison =
              L.build_icmp L.Icmp.Eq (L.build_load current_flag "TEMP" pred_builder) (L.const_int bool_t 0) "TEMP" pred_builder
            in
            let body_bb = L.append_block context "while_body" the_function in
            let while_builder = L.builder_at_end context body_bb in
            let loaded_iterator = L.build_load iterator_alloc "TEMP" while_builder in
            let new_val =
              L.build_fadd loaded_iterator (L.const_float num_t 1.0) "TEMP" while_builder
            in
            let pointer_to_next =
              L.build_struct_gep loaded_pointer_to_head 1 "TEMP" while_builder
            in
            let loaded_pointer_to_next =
              L.build_load pointer_to_next "TEMP" while_builder
            in
            let dereferened_pointer_to_next =
              L.build_load loaded_pointer_to_next "TEMP" while_builder
            in
            let () =
              ignore(L.build_store dereferened_pointer_to_next loaded_pointer_to_head while_builder)
            in
            let () =
              ignore(L.build_store loaded_pointer_to_next pointer_to_head while_builder)
            in
            let () =
              ignore(L.build_store new_val iterator_alloc while_builder)
            in
            let () = add_terminal while_builder (L.build_br pred_bb) in

            let iterator_check_bb = L.append_block context "check_iterator" the_function in
            let iterator_check_builder = L.builder_at_end context iterator_check_bb in

            let iterator_check =
              L.build_fcmp L.Fcmp.Oeq (L.build_load iterator_alloc "TEMP" iterator_check_builder) (L.const_float num_t 0.0) "TEMP" iterator_check_builder
            in
            let handle_zero_bb = L.append_block context "handle_zero_case" the_function in
            let handle_zero_builder = L.builder_at_end context handle_zero_bb in
            let () =
              ignore(L.build_store e1' pointer_to_head handle_zero_builder)
            in
            let merge_bb = L.append_block context "merge" the_function in
            let merge_builder = L.builder_at_end context merge_bb in
            let () = add_terminal handle_zero_builder (L.build_br merge_bb) in

            let newly_loaded_pointer_to_head =
              L.build_load pointer_to_head "TEMP" merge_builder
            in
            let pointer_to_current_val =
              L.build_struct_gep newly_loaded_pointer_to_head 0 "TEMP" merge_builder
            in
            let () =
              ignore(L.build_store e2' pointer_to_current_val merge_builder)
            in
            let pointer_to_current_flag =
              L.build_struct_gep newly_loaded_pointer_to_head 2 "TEMP" merge_builder
            in
            let () =
              ignore(L.build_store (L.const_int bool_t 0) pointer_to_current_flag merge_builder)
            in
            let empty_expr =
              list_terminator (list_inner_type t)
            in
            let empty_var = L.build_alloca list_item_struct "TEMP" merge_builder in
            let () =
              ignore(L.build_store empty_expr empty_var merge_builder)
            in
            let pointer_to_next =
              L.build_struct_gep newly_loaded_pointer_to_head 1 "TEMP" merge_builder
            in
            let () =
              ignore(L.build_store empty_var pointer_to_next merge_builder)
            in

            let _ = L.build_cond_br iterator_check handle_zero_bb merge_bb iterator_check_builder in
            let _ = L.build_cond_br comparison body_bb iterator_check_bb pred_builder in
            merge_builder

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

            let _ =
              L.build_br pred_bb builder
            in
                (* generate predicate code *)
            let pred_builder =
              L.builder_at_end context pred_bb
            in
            let bool_val =
              expr pred_builder scope predicate
            in
            let body_bb =
              L.append_block context "while_body" the_function
            in
            let while_builder =
              build_statement scope body (L.builder_at_end context body_bb)
            in
            let () =
              add_terminal while_builder (L.build_br pred_bb)
            in
            let merge_bb =
              L.append_block context "merge" the_function
            in
            let _ =
              L.build_cond_br bool_val body_bb merge_bb pred_builder
            in
            L.builder_at_end context merge_bb

          | SFor(e1, e2, e3, body) -> build_statement scope
              ( SBlock [SExpr e1 ;
                        SWhile(e2, SBlock[ body ;
                                           SExpr e3] ) ] ) builder

          | SForEach(t, id, e2, body) ->
            let index_var_name = "foreach_index" in
              (* inital value *)
              let expr_a = SStmtVDecl(Ast.Num, index_var_name, (Ast.Num, SIntegerLiteral(0))) in
                let expr_x = SStmtVDecl(t, id, (Ast.Void, SNoexpr)) in

                (* get the actual type of the list to pass to length call *)
                let (ty, e2') = e2 in
                  let list_length = match ty with
                    List(ty) -> (Ast.Num, SLength (ty, e2') )
                    | _ -> raise(Failure("fail"))
                  in 

                let expr_b = (Ast.Bool, SBinop((Ast.Num, (SId(index_var_name))), Less, list_length)) in
                  let expr_c = (Ast.Num, SPop(index_var_name, Inc)) in 
                    let list_lookup = (t, SListAccess(e2, (Ast.Num, SId(index_var_name))))
                      in                      
                    let list_lookup_assign = SExpr(t, SAssign(id, list_lookup)) in

              build_statement scope

              ( SBlock [ expr_a;
                         expr_x;
                        SWhile(expr_b, SBlock[ list_lookup_assign ;
                                            body ;
                                           SExpr expr_c] ) ] ) 
              builder

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
