TARFILES = Makefile scanner.mll parser.mly ast.ml

.PHONY : default
default : scanner.ml parser.ml

scanner.ml : scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli : parser.mly
	ocamlyacc parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

.PHONY : clean
clean :
	rm -f parser.ml parser.mli scanner.ml *.cmo *.cmi
