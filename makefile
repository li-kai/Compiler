############
DIFF=/usr/bin/diff -sb
OCAMLF=/usr/bin/env ocamlfind
OCAMLC=/usr/bin/env ocamlc
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

jlite_toir3.cmo: jlite_toir3.ml
	$(OCAMLC) -c $<

ir3_structs.cmo: ir3_structs.ml
	$(OCAMLC) -c $<

arm_structs.cmo: arm_structs.ml
	$(OCAMLC) -c $<

ir3_to_arm.cmo: ir3_to_arm.ml
	$(OCAMLC) -c $<

jlite_parser.ml: jlite_parser.mli
	$(OCAMLC) -c $<

jlite_parser.mli: jlite_parser.mly
	$(OCAMLYACC) -v $<

jlite_lexer.ml: jlite_lexer.mll
	$(OCAMLLEX) $<

compile: jlite_structs.cmo ir3_structs.cmo jlite_toir3.cmo jlite_annotatedtyping.cmo arm_structs.cmo basic_blocks.cmo jlite_parser.ml jlite_lexer.ml ir3_to_arm.cmo
	$(OCAMLC) -c jlite_lexer.ml
	$(OCAMLC) -c jlite_parser.ml
	$(OCAMLC) -c jlite_main.ml

jlite_main: compile
	$(OCAMLC) -o jlite_main jlite_structs.cmo jlite_lexer.cmo jlite_parser.cmo ir3_structs.cmo jlite_toir3.cmo jlite_annotatedtyping.cmo arm_structs.cmo ir3_to_arm.cmo jlite_main.cmo

############
run:
	$(ARM) armTests/test_ops.s -o armTests/test.in --static
	$(GEM) armTests/test.in --output armTests/test.out

############
test: jlite_main
	./$< < armTests/test_booleans.j > armTests/test_booleans.arm
	$(DIFF) armTests/class_main.arm armTests/class_main.s
	./$< < armTests/test_fields.j > armTests/test_fields.arm
	$(DIFF) armTests/test_fields.arm armTests/test_fields.s
	./$< < armTests/test_functions.j > armTests/test_functions.arm
	$(DIFF) armTests/test_functions.arm armTests/test_functions.s
	./$< < armTests/test_ops.j > armTests/test_ops.arm
	$(DIFF) armTests/test_ops.arm armTests/test_ops.s

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
	rm -f *.mli *.cmo *.cmi armTests/*.arm armTests/*.out
	rm -f jlite_main jlite_parser.ml jlite_lexer.ml jlite_parser.output
