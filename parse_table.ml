
let parse_file file =

	let ic = open_in file in
	let lexbuf = Lexing.from_channel ic in
	let table = Table_parser.table Table_scanner.token lexbuf in  
	let stable = Semant.convert_csv table in
	print_string (Sast.string_of_sexpr stable)

let () =
	let file = "test.csv" in
	parse_file file