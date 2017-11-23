############
DIFF=/usr/bin/diff -sb
OCAMLF=/usr/bin/env ocamlfind
OCAMLC=/usr/bin/env ocamlc -g
OCAMLLEX=/usr/bin/env ocamllex
OCAMLYACC=/usr/bin/env ocamlyacc
ARM=arm-linux-gnueabi-g++-4.7
GEM=/usr/local/src/gem5/build/ARM/gem5.opt \
	/usr/local/src/gem5/configs/example/se.py -c
############

all: jlite_main

jlite_structs.cmo: jlite_structs.ml
	$(OCAMLC) -c $<

jlite_annotatedtyping.cmo: jlite_annotatedtyping.ml
	$(OCAMLC) -c $<

basic_blocks.cmo: basic_blocks.ml
	$(OCAMLC) -c $<

register_allocation.cmo: register_allocation.ml
	$(OCAMLC) -c $<

jlite_toir3.cmo: jlite_toir3.ml
	$(OCAMLC) -c $<

ir3_structs.cmo: ir3_structs.ml
	$(OCAMLC) -c $<

arm_structs.cmo: arm_structs.ml
	$(OCAMLC) -c $<

ir3_to_arm.cmo: ir3_to_arm.ml
	$(OCAMLC) -c $<

optimize_ir3.cmo: optimize_ir3.ml
	$(OCAMLC) -c $<

graph.cmo: graph.ml
	$(OCAMLC) -c $<

jlite_report.cmo: jlite_report.ml
	$(OCAMLC) -c $<

jlite_duplicate_check.cmo: jlite_duplicate_check.ml
	$(OCAMLC) -c $<

jlite_parser.ml: jlite_parser.mli
	$(OCAMLC) -c $<

jlite_parser.mli: jlite_parser.mly
	$(OCAMLYACC) -v $<

jlite_lexer.ml: jlite_lexer.mll
	$(OCAMLLEX) $<

compile: jlite_report.cmo graph.cmo jlite_structs.cmo jlite_duplicate_check.cmo ir3_structs.cmo jlite_annotatedtyping.cmo jlite_toir3.cmo arm_structs.cmo basic_blocks.cmo optimize_ir3.cmo register_allocation.cmo jlite_parser.ml jlite_lexer.ml ir3_to_arm.cmo
	$(OCAMLC) -c jlite_lexer.ml
	$(OCAMLC) -c jlite_parser.ml
	$(OCAMLC) -c jlite_main.ml

jlite_main: compile
	$(OCAMLC) -o jlite_main jlite_report.cmo graph.cmo jlite_structs.cmo jlite_duplicate_check.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_annotatedtyping.cmo jlite_toir3.cmo arm_structs.cmo basic_blocks.cmo register_allocation.cmo optimize_ir3.cmo ir3_to_arm.cmo jlite_main.cmo

############
min_vector:
	- $(ARM) armTests/min_vector.s -o armTests/test.bin --static
	- $(GEM) armTests/test.bin --output armTests/min_vector.out
	$(DIFF) armTests/min_vector.test armTests/vector.out

run:
	- $(ARM) armTests/test_ops.s -o armTests/test.bin --static
	- $(GEM) armTests/test.bin --output armTests/test_ops.out
	$(DIFF) armTests/test_ops.test armTests/test_ops.out

	- $(ARM) armTests/test_print.s -o armTests/test.bin --static
	- $(GEM) armTests/test.bin --output armTests/test_print.out
	$(DIFF) armTests/test_print.test armTests/test_print.out

	- $(ARM) armTests/telegram.s -o armTests/test.bin --static
	- $(GEM) armTests/test.bin --output armTests/telegram.out
	$(DIFF) armTests/telegram.test armTests/telegram.out

############
test: jlite_main
	OCAMLRUNPARAM=b ./$< armTests/fib.j > armTests/fib.s
	OCAMLRUNPARAM=b ./$< armTests/fact.j > armTests/fact.s
	OCAMLRUNPARAM=b ./$< armTests/test_functions.j > armTests/test_functions.s
	OCAMLRUNPARAM=b ./$< armTests/test_strength.j > armTests/test_strength.s
	OCAMLRUNPARAM=b ./$< armTests/test_fields.j > armTests/test_fields.s
	OCAMLRUNPARAM=b ./$< armTests/test_ops.j > armTests/test_ops.s
	OCAMLRUNPARAM=b ./$< armTests/simple.j > armTests/simple.s
	OCAMLRUNPARAM=b ./$< armTests/telegram.j > armTests/telegram.s
	OCAMLRUNPARAM=b ./$< armTests/test_print.j > armTests/test_print.s
	OCAMLRUNPARAM=b ./$< armTests/test_spill.j > armTests/test_spill.s
	OCAMLRUNPARAM=b ./$< armTests/test_5params.j > armTests/test_5params.s
	OCAMLRUNPARAM=b ./$< armTests/test_assign.j > armTests/test_assign.s
	OCAMLRUNPARAM=b ./$< armTests/vector.j > armTests/vector.s

unit_basic_block: tests/basic_blocks_test.ml
	$(OCAMLF) ocamlc -o tests/basic_block_test -package oUnit -linkpkg -g jlite_structs.ml ir3_structs.ml basic_blocks.ml $<
	./tests/basic_block_test
	rm ./tests/basic_block_test

unit_optimize_ir3: tests/optimize_ir3_test.ml
	$(OCAMLF) ocamlc -o tests/optimize_ir3 -package oUnit -linkpkg -g jlite_structs.ml ir3_structs.ml optimize_ir3.ml $<
	./tests/optimize_ir3
	rm ./tests/optimize_ir3

unit: unit_basic_block unit_optimize_ir3

############
clean:
	rm -f *.mli *.cmo *.cmi armTests/*.s armTests/*.out
	rm -f jlite_main jlite_parser.ml jlite_lexer.ml jlite_parser.output
