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

let tests = "test suite for sum" >::: [
  "empty"  >:: (fun _ ->
(print_string (string_of_list ir3prog string_of_ir3_stmt "\n"));
    assert_equal 0 (0)
  );
]

let _ = run_test_tt_main tests
