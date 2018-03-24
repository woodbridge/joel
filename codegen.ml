(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(* We'll refer to Llvm and Ast constructs with module names 
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (statements) =
  let context    = L.global_context () in
  (* Add types to the context so we can use them in our LLVM code *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context 
  and float_t    = L.float_type  context
  and void_t     = L.void_type   context 
  (* Create an LLVM module -- this is a "container" into which we'll 
     generate actual code *)
  and the_module = L.create_module context "Joel" in

  (* Convert Joel types to LLVM types *)
  let ltype_of_typ = function
      A.Num   -> float_t
    | t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet"))
  in

  (* Declare a "printf" function to implement Joel's "print". *)
  let printf_t : L.lltype = 
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
     L.declare_function "printf" printf_t the_module in 

  let to_imp str = raise (Failure ("Not yet implemented: " ^ str)) in

  (* Generate the instructions for a "main" function, in which all program statements will be run. *)
  let build_program statements = 
    (* Make the LLVM module "aware" of our main function *)
      let main_ty = L.function_type void_t [||] in
      let the_function = L.define_function "main" main_ty the_module in
    (* Create an Instruction Builder, which points into a basic block
      and determines where the next instruction should be placed *)
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (* Create a pointer to a format string for printf *)
    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in
    (* Generate LLVM code for a call to MicroC's "print" *)
    let rec expr builder ((_, e) : sexpr) = match e with
  SIntegerLiteral i -> L.const_int i32_t i (* Generate a constant integer *)
      | SCall ("print", [e]) -> (* Generate a call instruction *)
    L.build_call printf_func [| int_format_str ; (expr builder e) |]
      "printf" builder 
      (* Throw an error for any other expressions *)
      | _ -> to_imp ("Error")  
    (* in
     Deal with a block of expression statements, terminated by a return 
    let rec stmt builder = function
  SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> let _ = expr builder e in builder 
      | SReturn e -> let _ = match fdecl.styp with
                              A.Int -> L.build_ret (expr builder e) builder 
                            | _ -> to_imp (A.string_of_typ fdecl.styp)
                     in builder
      | s -> to_imp (string_of_sstmt s)
    (* Generate the instructions for the function's body, 
       which mutates the_module *)
    in ignore(stmt builder (SBlock fdecl.sbody))*)
  (* Build each function (there should only be one for Hello World), 
     and return the final module *)
  in List.iter build_program statements; the_module *)