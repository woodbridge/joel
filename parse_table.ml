
let parse_file file =

	let ic = open_in file in
	let lexbuf = Lexing.from_channel ic in
	let table = Table_parser.table Table_scanner.token lexbuf in  
	Semant.convert_csv table 

let test file =
	print_string (Sast.string_of_sexpr (parse_file file))

(* let () = test "censuspops.csv" *)