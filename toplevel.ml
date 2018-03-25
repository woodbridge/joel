(* Top-level code to drive the scanner and parser for Joel on some input.
   Scan and parse the input. Does not print the AST - only prints "Success"
   upon successfully parsing the input, or prints the parser error if it fails. *)

(* let () =
	let usage_msg = "usage: ./toplevel.native [file.joel]" in
	let channel = ref stdin in

	(* Take the file to compile as a command-line arg *)
	Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
	
	let lexbuf = Lexing.from_channel !channel in
	let _ = Parser.program Scanner.token lexbuf in
	print_string "Success" *)

type action = Ast | Sast | LLVM_IR | Compile

let () =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./toplevel.native [-a|-s|-l|-c] [file.joel]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in  
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.convertToSAST ast in
    match !action with
      Ast     -> ()
    | Sast    -> print_string (Sast.string_of_sprogram sast)
    | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.hello sast)) (* print_string (Llvm.string_of_llmodule (Codegen.translate sast)) *)
    | Compile -> () (* let m = Codegen.translate sast in
	Llvm_analysis.assert_valid_module m;
	print_string (Llvm.string_of_llmodule m) *)
