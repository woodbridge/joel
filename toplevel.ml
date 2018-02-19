(* Top-level code to drive the scanner and parser for Joel on some input.
   Scan and parse the input. *)

let () =
	let usage_msg = "usage: ./toplevel.native [file.mc]" in
	let channel = ref stdin in

	(* Take the file to compile as a command-line arg *)
	Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
	
	let lexbuf = Lexing.from_channel !channel in
	let ast = Parser.program Scanner.token lexbuf in
	print_string (Ast.string_of_program ast) (* Print out the AST *)