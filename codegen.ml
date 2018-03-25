

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

let do_nothing (_, statements) =
  let context    = L.global_context () in

  let i32_t  = L.i32_type    context
  and i8_t   = L.i8_type     context
  in
  
  (*  Map JOEL types to LLVM types *)
  let ltype_of_typ = function
    (* assume tables are only of numbers... and now i'm realizing why matrix is what u do. *)
      A.Table -> L.pointer_type(i32_t)
    | A.Num -> i32_t
  in

  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  let the_module = L.create_module context "Joel" in

  (* get our printing *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Build a "main" function to enclose all statements in the program *)
  let main_ty = L.function_type i32_t [||] in
  let the_function = L.define_function "main" main_ty the_module in

  let add_terminal builder f =
    (* The current block where we're inserting instr *)
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
      | None -> ignore (f builder)
  in

  let string_repr (t, e) = match e with
    SIntegerLiteral i -> string_of_int i

  in


  (* placeholder right now to sanity check that the builder is building. *)

  let make_return builder =
    let t = L.build_ret (L.const_int i32_t 0) in 
      add_terminal builder t
  (* in *)

(*   let build_array builder =
    let type = i32_t in
 *)      
  in 

  let build_program_body statements =
    let builder = L.builder_at_end context (L.entry_block the_function) in
      let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
      and str_format_str  = L.build_global_stringptr "%s\n" "fmt" builder 
      in

      let add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
          in StringMap.add n local_var m

      in

      (* expression builder *)
      let rec expr builder (_, e) = match e with
          STableLiteral t -> L.const_int i32_t 0   (* temporary    *)
        | SIntegerLiteral i -> L.const_int i32_t i
        | SStringLiteral s -> L.build_global_stringptr s "string" builder
        | SCall ("printf", [e]) ->
          let (_, sx) = e in
            match sx with
              STableLiteral rows ->
                let pp rows =
                  let string_of_row row = 
                    "\t" ^ String.concat ", " (List.map string_repr (List.rev row)) ^ ";\n"
                  in
                    "(\n" ^ String.concat "\n" (List.map string_of_row rows) ^ ")"
                in 
                  let string_repr = pp rows in 
                    L.build_call printf_func [| str_format_str ; (L.build_global_stringptr string_repr "string" builder) |] "printf" builder

      in

      (* statement builder *)
      let rec build_statement stmt =
        let hack = match stmt with
          SExpr e -> (ignore(expr builder e))
        in 
          ()

      in
        List.iter build_statement statements; make_return builder; ()

  in
    build_program_body statements;
    the_module