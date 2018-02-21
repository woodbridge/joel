(* Top-level code to drive the scanner and parser for Joel on some input.
   Scan and parse the input. Does not print the AST - only prints "Success"
   upon successfully parsing the input, or prints the parser error if it fails. *)

let () =
	let usage_msg = "usage: ./toplevel.native [file.joel]" in
	let channel = ref stdin in

	(* Take the file to compile as a command-line arg *)
	Arg.parse [] (fun filename -> channel := open_in filename) usage_msg;
	
	let lexbuf = Lexing.from_channel !channel in
	let ast = Parser.program Scanner.token lexbuf in
	print_string "Success"