open Jlite_structs
open Ir3_structs


(* ====================================================================== *)

let temp_var (id: id3) : bool =
  String.get id 0 == '_'

let get_val ht idc3_id =
  match idc3_id with
  | Var3 id ->
    if temp_var id
    then Hashtbl.find ht id
    else Var3 id
  | _ -> idc3_id

let rec repeat_fn n fn input =
  if n < 1
  then input
  else fn @@ repeat_fn (n - 1) fn input

let set_of_list l =
  let res = Hashtbl.create (List.length l)
  in let () = List.iter (fun x -> Hashtbl.add res x ()) l
  in res;;

let rec get_assign_lhs_ids stmts : id3 list =
  match stmts with
  | [] -> []
  | AssignStmt3 (id, e) :: tail -> id :: get_assign_lhs_ids tail
  | _ :: tail -> get_assign_lhs_ids tail

let get_temp_var_ids
    (stmts: ir3_stmt list) =
  let ids = get_assign_lhs_ids stmts in
  let temp_ids = List.filter temp_var ids in
  set_of_list temp_ids

let remove_unused_temp_vars
    (lvars: var_decl3 list) (temp_var_ids) : var_decl3 list =
  let is_used_var ((_, id): var_decl3) =
    if temp_var id
    then Hashtbl.mem temp_var_ids id
    else true
  in

  List.filter is_used_var lvars

let replace_temp_in_exp
    (e: ir3_exp) ht : ir3_exp =
  match e with
  | BinaryExp3 (op, v1, v2) ->
    begin
      let nv1 = get_val ht v1 in
      let nv2 = get_val ht v2 in
      BinaryExp3 (op, nv1, nv2)
    end
  | UnaryExp3 (op, id) -> UnaryExp3 (op, get_val ht id)
  | Idc3Expr id -> Idc3Expr (get_val ht id)
  | MdCall3 (m, args) ->
    begin
      let new_args = List.map (fun arg -> get_val ht arg) args in
      MdCall3 (m, new_args)
    end
  | _ -> e

let replace_temp_with_constants
    (stmts: ir3_stmt list) =
  let rec aux_replace_stmts
      (stmts: ir3_stmt list) ht =
    match stmts with
    | [] -> []
    | AssignStmt3 (id, expr) :: tail ->
      if temp_var id
      then
        match expr with
        | Idc3Expr e ->
          Hashtbl.add ht id e;
          aux_replace_stmts tail ht
        | _ ->
          let _ = Hashtbl.add ht id (Var3 id) in
          let new_expr = replace_temp_in_exp expr ht in
          AssignStmt3 (id, new_expr) :: aux_replace_stmts tail ht
      else
        let new_expr = replace_temp_in_exp expr ht in
        AssignStmt3 (id, new_expr) :: aux_replace_stmts tail ht
    | head :: tail ->
      begin
        let new_head =
          begin
            match head with
            | IfStmt3 (e, l) -> IfStmt3 (replace_temp_in_exp e ht, l) :: []
	        | AssignFieldStmt3 (e1, e2) ->
           begin
             let ne1 = replace_temp_in_exp e1 ht in
             let ne2 = replace_temp_in_exp e2 ht in
             AssignFieldStmt3 (ne1, ne2) :: []
           end
	     | MdCallStmt3 e -> MdCallStmt3 (replace_temp_in_exp e ht) :: []
      | PrintStmt3 (Var3 id) ->
        if temp_var id
        then
          let v = Hashtbl.find ht id in
          PrintStmt3 v :: []
        else head :: []
      | ReturnStmt3 id ->
        if temp_var id
        then
          let v = Hashtbl.find ht id in
          let _ = Hashtbl.remove ht id in
          AssignStmt3 (id, Idc3Expr v) ::
          head :: []
        else head :: []
      | _ -> head :: []
          end
        in
        let new_tail = aux_replace_stmts tail ht in
        new_head @ new_tail
      end
  in

  aux_replace_stmts stmts (Hashtbl.create 2)
(* stmts *)

(* ====================================================================== *)

let constant_folding_exp
    (e: ir3_exp) : ir3_exp =
  match e with
  | BinaryExp3 (AritmeticOp "+", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (IntLiteral3 (i1 + i2))
  | BinaryExp3 (AritmeticOp "-", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (IntLiteral3 (i1 - i2))
  | BinaryExp3 (AritmeticOp "*", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (IntLiteral3 (i1 * i2))
  | BinaryExp3 (AritmeticOp "/", IntLiteral3 i1, IntLiteral3 i2) -> failwith "Cannot contain division operations!"
  | BinaryExp3 (RelationalOp "==", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 == i2))
  | BinaryExp3 (RelationalOp "!=", IntLiteral3 i1, IntLiteral3 i2) -> Idc3Expr (BoolLiteral3 (i1 != i2))
  | BinaryExp3 (RelationalOp "==", BoolLiteral3 b1, BoolLiteral3 b2) -> Idc3Expr (BoolLiteral3 (b1 == b2))
  | BinaryExp3 (RelationalOp "!=", BoolLiteral3 b1, BoolLiteral3 b2) -> Idc3Expr (BoolLiteral3 (b1 != b2))
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
  constant_folding_exp @@ strength_reduction_exp e

let optimize_stmt
    (stmt: ir3_stmt) : ir3_stmt list =
  match stmt with
  | IfStmt3 (e, l) ->
    begin
      match e with
      | Idc3Expr (BoolLiteral3 true) -> [GoTo3 l]
      | Idc3Expr (BoolLiteral3 false) -> []
      | _ -> [IfStmt3 (optimize_exp e, l)]
    end
  | AssignStmt3 (id, e) ->
    begin
      match e with
      | Idc3Expr (Var3 vid) ->
        if vid = id then []
        else [AssignStmt3 (id, e)]
      | _ -> [AssignStmt3 (id, optimize_exp e)]
    end

  | AssignFieldStmt3 (e1, e2) -> [AssignFieldStmt3 (optimize_exp e1, optimize_exp e2)]
  | MdCallStmt3 e -> [MdCallStmt3 (optimize_exp e)]
  | _ -> [stmt]

let optimize_stmts
    (stmts: ir3_stmt list) : ir3_stmt list =
  List.flatten @@ List.map optimize_stmt (replace_temp_with_constants stmts)

let optimize_md
    (md: md_decl3) : md_decl3 =
  let optimized_stmts = optimize_stmts md.ir3stmts in
  let temp_vars_ids = get_temp_var_ids optimized_stmts in
  let optimized_localvars = remove_unused_temp_vars md.localvars3 temp_vars_ids in
  { md with
    ir3stmts = optimized_stmts;
    localvars3 = optimized_localvars;
  }

let optimize_prog
    ((cdata, main_md, class_mds): ir3_program) : ir3_program =
  let repeated_optimize_md = repeat_fn 10 optimize_md in
  let optimized_main_md = repeated_optimize_md main_md in
  let optimized_class_mds = List.map repeated_optimize_md class_mds in
  (cdata, optimized_main_md, optimized_class_mds)
