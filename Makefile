TARFILES = Makefile scanner.mll parser.mly ast.ml

.PHONY : default
default : scanner.ml parser.ml

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

.PHONY : clean
clean :
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi parser.output

.PHONY : debug
debug :
	ocamlyacc -v parser.mly
