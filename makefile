############
DIFF=/usr/bin/diff -sb
OCAMLC=/usr/bin/env ocamlc
OCAMLLEX=/usr/bin/env ocamllex
OCAMLYACC=/usr/bin/env ocamlyacc
############

all: clean jlite_main

jlite_structs.cmo: jlite_structs.ml
	$(OCAMLC) -c $<

jlite_annotatedtyping.cmo: jlite_annotatedtyping.ml
	$(OCAMLC) -c $<

jlite_toir3.cmo: jlite_toir3.ml
	$(OCAMLC) -c $<

ir3_structs.cmo: ir3_structs.ml
	$(OCAMLC) -c $<

arm_structs.cmo: arm_structs.ml
	$(OCAMLC) -c $<

jlite_parser.ml: jlite_parser.mli
	$(OCAMLC) -c $<

jlite_parser.mli: jlite_parser.mly
	$(OCAMLYACC) -v $<

jlite_lexer.ml: jlite_lexer.mll
	$(OCAMLLEX) $<

compile: jlite_structs.cmo ir3_structs.cmo jlite_toir3.cmo jlite_annotatedtyping.cmo arm_structs.cmo jlite_parser.ml jlite_lexer.ml
	$(OCAMLC) -c jlite_lexer.ml
	$(OCAMLC) -c jlite_parser.ml
	$(OCAMLC) -c jlite_main.ml

jlite_main: compile
	$(OCAMLC) -o jlite_main jlite_structs.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_toir3.cmo jlite_annotatedtyping.cmo arm_structs.cmo jlite_main.cmo

############
test: jlite_main
	./$< < armTests/test_booleans.j > armTests/test_booleans.out
	$(DIFF) armTests/class_main.out armTests/class_main.s
	./$< < armTests/test_fields.j > armTests/test_fields.out
	$(DIFF) armTests/test_fields.out armTests/test_fields.s
	./$< < armTests/test_functions.j > armTests/test_functions.out
	$(DIFF) armTests/test_functions.out armTests/test_functions.s
	./$< < armTests/test_ops.j > armTests/test_ops.out
	$(DIFF) armTests/test_ops.out armTests/test_ops.s

############
clean:
	rm -f *.mli *.cmo *.cmi armTests/*.out
	rm -f jlite_main jlite_parser.ml jlite_lexer.ml jlite_parser.output
