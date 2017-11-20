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

let rec compare_lines v1 v2 = match v1, v2 with
| [], []       -> true
| [], _ | _, []        -> false
| x::x_tail, y::y_tail -> x.stmt = y.stmt &&
  Id3Set.equal x.live y.live &&
  Id3Set.equal x.next_use y.next_use &&
  compare_lines x_tail y_tail

module Block =
struct
  type t = block
  let compare = fun a b ->
  let b = a.id = b.id && compare_lines a.lines b.lines in
    if b then 0 else 1
  let pp_printer = fun ppf blk ->
    Format.pp_print_string ppf(string_of_basic_block blk)
let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListInt = OUnitDiff.ListSimpleMake(Block);;

let default_stmt = {
  stmt = ir3prog_arr.(0);
  next_use = Id3Set.empty;
  live = Id3Set.empty;
}

let make_lines (stmts) =
  List.map (fun stmt -> { default_stmt with stmt; }) stmts

let make_block (id: int) lines = {
  id;
  lines;
  live_in = Id3Set.empty;
  live_out = Id3Set.empty;
}

let tests = "test suite basic_blocks" >::: [
"fn_to_basic_blocks empty"  >:: (fun _ ->
    ListInt.assert_equal
    (* ~printer: string_of_basic_blocks *)
    (fn_to_basic_blocks []) []
  );
"fn_to_basic_blocks converts to basic blocks" >:: (fun _ ->
    let res = fn_to_basic_blocks ir3prog in
    ListInt.assert_equal
    (* ~printer: string_of_basic_blocks *)
    res [
      make_block 4 (make_lines [ ir3prog_arr.(0) ]);
      make_block 3 (make_lines [
        ir3prog_arr.(1);
        ir3prog_arr.(2);
      ]);
      make_block 2 (make_lines [
        ir3prog_arr.(3);
        ir3prog_arr.(4);
        ir3prog_arr.(5);
        ir3prog_arr.(6);
        ir3prog_arr.(7);
        ir3prog_arr.(8);
        ir3prog_arr.(9);
      ]);
      make_block 1 (make_lines [
        ir3prog_arr.(10);
        ir3prog_arr.(11);
      ]);
      make_block 0 (make_lines [
        ir3prog_arr.(12);
        ir3prog_arr.(13);
        ir3prog_arr.(14);
      ]);
    ]
  );
"fn_to_basic_blocks converts to basic blocks" >:: (fun _ ->
let _ = reset_blk() in
let res = get_liveness_of_basic_blocks (fn_to_basic_blocks ir3prog) in
  ListInt.assert_equal
    (* ~printer: string_of_basic_blocks *)
res [
      make_block 4 [
        { default_stmt with
          stmt = ir3prog_arr.(0);
        }
      ];
      make_block 3 [
        { default_stmt with
          stmt = ir3prog_arr.(1);
        };
        { default_stmt with
          stmt = ir3prog_arr.(2);
        };
      ];
      make_block 2 [
        { default_stmt with
          stmt = ir3prog_arr.(3);
          live = Id3Set.of_list ["i"; "j"];
        };
        {
          stmt = ir3prog_arr.(4);
          next_use = Id3Set.of_list ["i"];
          live = Id3Set.of_list ["i"; "j"];
        };
        {
          stmt = ir3prog_arr.(5);
          next_use = Id3Set.of_list ["j"; "t1"];
          live = Id3Set.of_list ["j"; "t1"];
        };
        {
          stmt = ir3prog_arr.(6);
          next_use = Id3Set.of_list ["t2"];
          live = Id3Set.of_list ["j"; "t2"];
        };
        {
          stmt = ir3prog_arr.(7);
          next_use = Id3Set.of_list ["t3"];
          live = Id3Set.of_list ["j"; "t3"];
        };
        {
          stmt = ir3prog_arr.(8);
          next_use = Id3Set.of_list ["j"];
          live = Id3Set.of_list ["j"];
        };
        {
          stmt = ir3prog_arr.(9);
          next_use = Id3Set.of_list ["j"];
          live = Id3Set.of_list ["j"];
        };
      ];
      make_block 1 [
        {
          stmt = ir3prog_arr.(10);
          next_use = Id3Set.of_list ["i"];
          live = Id3Set.of_list ["i"];
        };
        {
          stmt = ir3prog_arr.(11);
          next_use = Id3Set.of_list ["i"];
          live = Id3Set.of_list ["i"];
        };
      ];
      make_block 0 [
        { default_stmt with
          stmt = ir3prog_arr.(12);
        };
        { default_stmt with
          stmt = ir3prog_arr.(13);
        };
        {
          stmt = ir3prog_arr.(14);
          next_use = Id3Set.of_list ["i"];
          live = Id3Set.of_list ["i"];
        };
      ];
    ]
  );
]

let _ = run_test_tt_main tests
