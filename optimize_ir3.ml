open Jlite_structs
open Ir3_structs

let constant_folding_exp
    (e: ir3_exp) : ir3_exp =
  match e with
  | BinaryExp3 (AritmeticOp "+", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (IntLiteral3 (i1 + i2))
  | BinaryExp3 (AritmeticOp "-", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (IntLiteral3 (i1 - i2))
  | BinaryExp3 (AritmeticOp "*", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (IntLiteral3 (i1 * i2))
  | BinaryExp3 (AritmeticOp "/", IntLiteral3 i1, IntLiteral3 i2) -> failwith "Cannot contain division operations!"
  | BinaryExp3 (RelationalOp "==", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 == i2))
  | BinaryExp3 (RelationalOp "!=", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 != i2))
  | BinaryExp3 (RelationalOp ">", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 > i2))
  | BinaryExp3 (RelationalOp ">=", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 >= i2))
  | BinaryExp3 (RelationalOp "<", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 < i2))
  | BinaryExp3 (RelationalOp "<=", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 <= i2))
  | BinaryExp3 (BooleanOp "&&", BoolLiteral3 b1, BoolLiteral3 b2) -> Idc3Expr (BoolLiteral3 (b1 && b2))
  | BinaryExp3 (BooleanOp "||", BoolLiteral3 b1, BoolLiteral3 b2) -> Idc3Expr (BoolLiteral3 (b1 || b2))
  | UnaryExp3 (UnaryOp "!" , BoolLiteral3 b) -> Idc3Expr (BoolLiteral3 (not b))
  | UnaryExp3 (UnaryOp "-" , IntLiteral3 i) -> Idc3Expr (IntLiteral3 (-i))
  | _ -> e

let strength_reduction_exp
    (e: ir3_exp) : ir3_exp =
  match e with
  | BinaryExp3 (AritmeticOp "+", IntLiteral3 0,  x) -> Idc3Expr x
  | BinaryExp3 (AritmeticOp "+", x, IntLiteral3 0) -> Idc3Expr x
  | BinaryExp3 (AritmeticOp "-", IntLiteral3 0,  x) -> Idc3Expr x
  | BinaryExp3 (AritmeticOp "-", x, IntLiteral3 0) -> Idc3Expr x
  | BinaryExp3 (AritmeticOp "*", IntLiteral3 0,  x) -> Idc3Expr (IntLiteral3 0)
  | BinaryExp3 (AritmeticOp "*", x, IntLiteral3 0) -> Idc3Expr (IntLiteral3 0)
  | BinaryExp3 (AritmeticOp "*", IntLiteral3 1,  x) -> Idc3Expr x
  | BinaryExp3 (AritmeticOp "*", x, IntLiteral3 1) -> Idc3Expr x
  | BinaryExp3 (AritmeticOp "*", IntLiteral3 2,  x) -> BinaryExp3 (AritmeticOp "+", x, x)
  | BinaryExp3 (AritmeticOp "*", x, IntLiteral3 2) -> BinaryExp3 (AritmeticOp "+", x, x)
  | _ -> e

let optimize_exp
    (e: ir3_exp) : ir3_exp =
  e |> strength_reduction_exp |> constant_folding_exp

let optimize_stmt
    (stmt: ir3_stmt) : ir3_stmt =
  match stmt with
  | IfStmt3 (e, l) -> IfStmt3 (optimize_exp e, l)
  | AssignStmt3 (id, e) -> AssignStmt3 (id, optimize_exp e)
  | AssignFieldStmt3 (e1, e2) -> AssignFieldStmt3 (optimize_exp e1, optimize_exp e2)
  | MdCallStmt3 e -> MdCallStmt3 (optimize_exp e)
  | _ -> stmt

let optimize_stmts
    (stmts: ir3_stmt list) : ir3_stmt list =
  List.map optimize_stmt stmts

let optimize_md
    (md: md_decl3) : md_decl3 =
  { md with ir3stmts = optimize_stmts md.ir3stmts }

let optimize_prog
    ((cdata, main_md, class_mds): ir3_program) : ir3_program =
  let optimized_main_md = optimize_md main_md in
  let optimized_class_mds = List.map optimize_md class_mds in
  (cdata, optimized_main_md, optimized_class_mds)
