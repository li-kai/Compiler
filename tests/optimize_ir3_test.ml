open OUnit2
open Basic_blocks
open Ir3_structs
open Jlite_structs
open Optimize_ir3

let ph_empty = (fun _ ->
    assert_equal
      (peephole_optimize_stmts []) []
  )

let ph_arith = (fun _ ->
    let stmts =
      (Label3 2) :: [] in
    assert_equal
      (peephole_optimize_stmts stmts) stmts
  )

let peep_hole_tests = "peep hole suite" >::: [
    "empty"  >:: ph_empty;
    "arith" >:: ph_arith;
  ]

let _ = run_test_tt_main peep_hole_tests
