build: 
	ocamlc -c algebra.mli
	ocamlc -c algebra.ml
doc: 
	ocamldoc algebra.mli -html -d documentation/

