open OUnit2
open Basic_blocks
open Ir3_structs
open Jlite_structs

let ir3prog = [
  (AssignStmt3 ("i", (Idc3Expr (IntLiteral3 1))));
  (Label3 2);
  (AssignStmt3 ("j", (Idc3Expr (IntLiteral3 1))));
  (Label3 3);
  (AssignStmt3 ("t1",
    (BinaryExp3 ((AritmeticOp "*"), (IntLiteral3 10), (Var3 "i")))
  ));
  (AssignStmt3 ("t2",
    (BinaryExp3 ((AritmeticOp "+"), (Var3 "t1"), (Var3 "j")))
  ));
  (AssignStmt3 ("t3",
    (BinaryExp3 ((AritmeticOp "*"), (IntLiteral3 8), (Var3 "t2")))
  ));
  (AssignStmt3 ("t4",
    (BinaryExp3 ((AritmeticOp "-"), (Var3 "t3"), (IntLiteral3 88)))
  ));
  (AssignStmt3 ("j",
    (BinaryExp3 ((AritmeticOp "+"), (IntLiteral3 1), (Var3 "j")))
  ));
  (IfStmt3 (BinaryExp3 ((RelationalOp "<=", (IntLiteral3 1), (Var3 "j"))), 3));
  (AssignStmt3 ("i",
    (BinaryExp3 ((AritmeticOp "+"), (IntLiteral3 10), (Var3 "i")))
  ));
  (IfStmt3 (BinaryExp3 ((RelationalOp "<=", (IntLiteral3 10), (Var3 "i"))), 2));
  (Label3 13);
  (AssignStmt3 ("i", (Idc3Expr (IntLiteral3 1))));
  (IfStmt3 (BinaryExp3 ((RelationalOp "<=", (IntLiteral3 10), (Var3 "i"))), 13));
]

let ir3prog_arr = Array.of_list ir3prog

module Block =
struct
  type t = block
  let compare = fun a b ->
    let b = a.id = b.id && a.stmts = b.stmts in
    if b then 0 else 1
  let pp_printer = fun ppf blk ->
    Format.pp_print_string ppf(string_of_basic_block blk)
let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListInt = OUnitDiff.ListSimpleMake(Block);;

let tests = "test suite for fn_to_basic_blocks" >::: [
  "empty"  >:: (fun _ ->
    ListInt.assert_equal
    (* ~printer: string_of_basic_blocks *)
    (fn_to_basic_blocks []) []
  );
  "converts to basic blocks" >:: (fun _ ->
    let res = fn_to_basic_blocks ir3prog in
    ListInt.assert_equal
    (* ~printer: string_of_basic_blocks *)
    res [
      { id = 4; stmts = [ ir3prog_arr.(0) ]};
      { id = 3; stmts = [
        ir3prog_arr.(1);
        ir3prog_arr.(2);
      ]};
      { id = 2; stmts = [
        ir3prog_arr.(3);
        ir3prog_arr.(4);
        ir3prog_arr.(5);
        ir3prog_arr.(6);
        ir3prog_arr.(7);
        ir3prog_arr.(8);
        ir3prog_arr.(9);
      ]};
      { id = 1; stmts = [
        ir3prog_arr.(10);
        ir3prog_arr.(11);
        ]};
      { id = 0; stmts = [
        ir3prog_arr.(12);
        ir3prog_arr.(13);
        ir3prog_arr.(14);
      ]};
    ]
  )
]

let _ = run_test_tt_main tests
