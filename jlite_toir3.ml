module S = Jlite_structs
module T = Jlite_annotatedtyping
module S3 = Ir3_structs

open S
open T

let fresh_var_counter = ref 0
let gen_fresh_var () =
  let id = !fresh_var_counter in
  fresh_var_counter := !fresh_var_counter + 1;
  "_t" ^ (string_of_int id)

let fresh_label_counter = ref 0
let gen_label () =
  let id = !fresh_label_counter in
  fresh_label_counter := !fresh_label_counter + 1;
  id

let method_table = Hashtbl.create 100

let jlite_to_IR3_var_decls vars =
  List.map (fun (typ, varid) -> (typ, Jlite_structs.string_of_var_id varid)) vars

let get_method_signature (class_name: string) (md_name: string) (params_types: S.jlite_type list) =
  class_name ^ "::" ^ md_name ^ " (" ^ (String.concat ", " @@ List.map Jlite_structs.string_of_jlite_type params_types) ^ ")"


let jlite_to_IR3_class_decl (class_name, fds, mds) =
  let var_table = jlite_to_IR3_var_decls fds in
  let md_table = List.map (fun v -> (get_method_signature class_name (Jlite_structs.string_of_var_id v.jliteid) (List.map (fun (typ, varid) -> typ) v.params), S.string_of_var_id v.ir3id)) mds in
  List.iter
    (fun ((md_sig_str, mapped_name): (string * string)) : unit ->
      Hashtbl.add method_table md_sig_str mapped_name)
  md_table;
  (class_name, var_table)

let iR3_expr_to_id3 (e: S3.ir3_exp) (t: S3.ir3_type) (vars: S3.var_decl3 list) (stmts: S3.ir3_stmt list) (toId3: bool) =
  if toId3 then
    match e with
    | S3.Idc3Expr (S3.Var3 _) -> (e, vars, stmts)
    | _ ->
       let new_variable = gen_fresh_var () in
       (S3.Idc3Expr (S3.Var3 (new_variable)), vars @ [t, new_variable], stmts @ [S3.AssignStmt3 (new_variable, e)])
  else
    (e, vars, stmts)

let iR3_expr_get_idc3 (e: S3.ir3_exp) =
  match e with
  | S3.Idc3Expr c -> c
  | _ -> failwith @@ "Cannot find idc3 in " ^ (S3.string_of_ir3_exp e)

let iR3_expr_get_id3 (e: S3.ir3_exp) =
  match e with
  | S3.Idc3Expr c ->
     begin
       match c with
       | S3.Var3 i3 -> i3
       | _ -> failwith @@ "Cannot find id3 in " ^ (S3.string_of_ir3_exp e)
     end
  | _ -> failwith @@ "Cannot find id3 in " ^ (S3.string_of_ir3_exp e)

let jlite_to_IR3_varid (class_name: string) (e: S.var_id) (toIdc3: bool) =
  match e with
  | TypedVarId (id1, t, 1) ->
     let newExpr = S3.FieldAccess3 ("this", id1) in
     iR3_expr_to_id3 newExpr t [] [] toIdc3
  | TypedVarId (id1, _, 2) | SimpleVarId (id1) -> (S3.Idc3Expr (S3.Var3 (id1)), [], [])
  | _ -> failwith "Bug in type annotation"

let rec jlite_to_IR3_expr (class_name: string) (e: S.jlite_exp) (toIdc3: bool) (toId3: bool) : (S3.ir3_exp * S3.var_decl3 list * S3.ir3_stmt list) =
  let rec helper (e: S.jlite_exp) (toIdc3: bool) (toId3: bool) =
    match e with
    | S.TypedExp (te, t) ->
       begin
         match te with
         | UnaryExp (op, e) ->
            let argIR3, vars, stmts = helper e true false in
            let argIdc3 = iR3_expr_get_idc3 argIR3 in
            let newExpr = S3.UnaryExp3 (op, argIdc3) in
            (iR3_expr_to_id3 newExpr t vars stmts toIdc3)
         | BinaryExp (op, e1, e2) ->
            let (arg1IR3, vars1, stmts1) = helper e1 true false in
            let (arg2IR3, vars2, stmts2) = helper e2 true false in
            let arg1Idc3 = iR3_expr_get_idc3 arg1IR3 in
            let arg2Idc3 = iR3_expr_get_idc3 arg2IR3 in
            let newExpr = S3.BinaryExp3 (op, arg1Idc3, arg2Idc3) in
            (iR3_expr_to_id3 newExpr t (vars1 @ vars2) (stmts1 @ stmts2) toIdc3)
         | FieldAccess (e, varid) ->
            let argIR3, vars, stmts = helper e true true in
            let argId3 = iR3_expr_get_id3 argIR3 in
            let newExpr = S3.FieldAccess3 (argId3, S.string_of_var_id varid) in
            iR3_expr_to_id3 newExpr t vars stmts toIdc3
         | ObjectCreate name ->
            let newExpr = S3.ObjectCreate3 name in
            iR3_expr_to_id3 newExpr t [] [] toIdc3
         | MdCall (e, params) ->
            let get_type_from_param = function
              | S.TypedExp (te, t) -> t
              | _ -> failwith "Bug in type annotation. Params should have type TypedExp"
            in
            let params_types = List.map get_type_from_param params in
            let processed_params = List.map (fun param -> helper param true false) params in
            let params_idc3 = List.map (fun (a, _, _) -> iR3_expr_get_idc3 a) processed_params in
            let params_vars = List.flatten @@ List.map (fun (_, b, _) -> b) processed_params in
            let params_stmts = List.flatten @@ List.map (fun (_, _, c) -> c) processed_params in
            let get_new_method_name class_name md_name params_types =
              let method_signature = get_method_signature class_name md_name params_types in
              assert (Hashtbl.mem method_table method_signature);
              Hashtbl.find method_table method_signature
            in
            begin
              match e with
              | FieldAccess (inner_e, varid) ->
                 begin
                   match inner_e with
                   | TypedExp (te, ObjectT name) ->
                      let argIR3, vars, stmts = helper inner_e true false in
                      let argIdc3 = iR3_expr_get_idc3 argIR3 in
                      let new_method_name = get_new_method_name name (S.string_of_var_id varid) params_types in
                      let newExpr = S3.MdCall3 (new_method_name, argIdc3 :: params_idc3) in
                      (iR3_expr_to_id3 newExpr t (params_vars @ vars) (params_stmts @ stmts) toIdc3)
                   | _ -> failwith @@ "Error during type annotation. Got " ^ (S.string_of_jlite_expr inner_e)
                 end
              | Var (varid) ->
                 let new_method_name = get_new_method_name class_name (S.string_of_var_id varid) params_types in
                 let newExpr = S3.MdCall3 (new_method_name, S3.Var3 ("this") :: params_idc3) in
                 (iR3_expr_to_id3 newExpr t params_vars params_stmts toIdc3)
              | _ -> failwith @@ "Error during type annotation. Got " ^ (S.string_of_jlite_expr e)
            end
         | BoolLiteral b ->
            let newExpr = S3.Idc3Expr (S3.BoolLiteral3 b) in
            iR3_expr_to_id3 newExpr t [] [] toId3
         | IntLiteral i ->
            let newExpr = S3.Idc3Expr (S3.IntLiteral3 i) in
            iR3_expr_to_id3 newExpr t [] [] toId3
         | StringLiteral s ->
            let newExpr = S3.Idc3Expr (S3.StringLiteral3 s) in
            iR3_expr_to_id3 newExpr t [] [] toId3
         | ThisWord ->
            let newExpr = S3.Idc3Expr (S3.Var3 "this") in
            iR3_expr_to_id3 newExpr t [] [] toIdc3
         | NullWord ->
            let newExpr = S3.Idc3Expr (S3.Var3 "NULL") in
            iR3_expr_to_id3 newExpr t [] [] toIdc3
         | Var (varid) -> jlite_to_IR3_varid class_name varid toIdc3
         | TypedExp (_, _) -> failwith "TypedExp cannot contain another TypedExp"
       end
    | _ -> failwith @@ "Every jLite_exp should be wrapped by TypedExp. Got " ^ (S.string_of_jlite_expr e)
  in
  helper e toIdc3 toId3

let rec jlite_to_IR3_stmts (class_name: S.class_name) (md: S.md_decl) (stmts: S.jlite_stmt list) : (S3.var_decl3 list * S3.ir3_stmt list) =
  match stmts with
  | [] -> ([], [])
  | stmt :: tail ->
     let rec helper s =
       match s with
       | IfStmt (e, true_stmts, false_stmts) ->
          let (expr3, exprvars, exprstmts) = jlite_to_IR3_expr class_name e false false in
          let (truevars, truestmts) = jlite_to_IR3_stmts class_name md true_stmts in
          let (falsevars, falsestmts) = jlite_to_IR3_stmts class_name md false_stmts in
          let true_label = gen_label () in
          let end_label = gen_label () in
          let goto_end = S3.GoTo3 end_label in
          let ifIR3 = S3.IfStmt3 (expr3, true_label) in
          (exprvars @ truevars @ falsevars,
           [ifIR3] @
             falsestmts @ [goto_end] @
               [S3.Label3 true_label] @ truestmts @
                 [S3.Label3 end_label])
       | WhileStmt (e, bstmts) ->
          let (expr3, exprvars, exprstmts) = jlite_to_IR3_expr class_name e false false in
          let (stmtsvars, stmtsstmts) = jlite_to_IR3_stmts class_name md bstmts in
          let begin_cond_label = gen_label () in
          let begin_code_label = gen_label () in
          let end_label = gen_label () in
          let goto_cond = S3.GoTo3 begin_cond_label in
          let goto_end = S3.GoTo3 end_label in
          let ifIR3 = S3.IfStmt3 (expr3, begin_code_label) in
          (exprvars @ stmtsvars,
           [S3.Label3 begin_cond_label] @
             exprstmts @ [ifIR3] @ [goto_end] @
               [S3.Label3 begin_code_label] @ stmtsstmts @ [goto_cond] @
                 [S3.Label3 end_label])
       | ReadStmt (varid) ->
          let readIR3 = match varid with
            | TypedVarId (id1, t, 1) -> S3.ReadStmt3 (id1)
            | TypedVarId (id1, _, 2) | SimpleVarId (id1) -> S3.ReadStmt3 (id1)
            | _ -> failwith "Bug in type annotation"
          in
          ([], [readIR3])
       | PrintStmt e ->
          let (expr3, exprvars, exprstmts) = jlite_to_IR3_expr class_name e true false in
          let argIdc3 = iR3_expr_get_idc3 expr3 in
          let printIR3 = S3.PrintStmt3 argIdc3 in
          (exprvars, exprstmts @ [printIR3])
       | AssignStmt (varid, e) ->
          let (expr3, exprvars, exprstmts) = jlite_to_IR3_expr class_name e false false in
          begin
            let assignIR3 = match varid with
              (* Class scope *)
              | TypedVarId (id1, t, 1) ->
                 S3.AssignFieldStmt3 (S3.FieldAccess3 ("this", id1), expr3)
              | TypedVarId (id1, _, 2) | SimpleVarId id1 ->
                 S3.AssignStmt3 (id1, expr3)
              | _ -> failwith "Bug in type annotation"
            in
            (exprvars, exprstmts @ [assignIR3])
          end
       | AssignFieldStmt (e1, e2) ->
          let (exprl3, exprlvars, exprlstmts) = jlite_to_IR3_expr class_name e1 false false in
          let (exprr3, exprrvars, exprrstmts) = jlite_to_IR3_expr class_name e2 false false in
          let assignIR3 = S3.AssignFieldStmt3 (exprl3, exprr3) in
          (exprlvars @ exprrvars, exprlstmts @ exprrstmts @ [assignIR3])
       | MdCallStmt e ->
          let (expr3, exprvars, exprstmts) = jlite_to_IR3_expr class_name e false false in
          let mdCallIR3 = S3.MdCallStmt3 expr3 in
          (exprvars, exprstmts @ [mdCallIR3])
       | ReturnStmt e ->
          let (expr3, exprvars, exprstmts) = jlite_to_IR3_expr class_name e true true in
          let retIR3 = S3.ReturnStmt3 (iR3_expr_get_id3 expr3) in
          (exprvars, exprstmts @ [retIR3])
       | ReturnVoidStmt -> ([], [S3.ReturnVoidStmt3])
     in
     let (vars, stmts) = (helper stmt) in
     let (tailvars, tailstmts) = jlite_to_IR3_stmts class_name md tail in
     (vars @ tailvars, stmts @ tailstmts)

let jlite_to_IR3_method_decl (class_name: S.class_name) (md_decl: S.md_decl) : S3.md_decl3 =
  (* We can reset the variable counter *)
  fresh_var_counter := 0;
  let (nvars, nstmts) = jlite_to_IR3_stmts class_name md_decl md_decl.stmts in
  {
    S3.id3 = Jlite_structs.string_of_var_id md_decl.ir3id;
    S3.rettype3 = md_decl.rettype;
    S3.params3 = (ObjectT class_name, "this") :: (jlite_to_IR3_var_decls md_decl.params);
    S3.localvars3 = (jlite_to_IR3_var_decls md_decl.localvars) @ nvars;
    S3.ir3stmts = nstmts
  }

let jlite_to_IR3_aux_classes (aux_classes: (S.class_name * (S.var_decl list) * (S.md_decl list)) list) =
  let process_class_methods (class_name, var_decls, md_decls) =
    List.map (jlite_to_IR3_method_decl class_name) md_decls
  in
  List.flatten @@ List.map process_class_methods aux_classes

let jlite_program_to_IR3 prog =
  let main_class, aux_classes = prog in
  let (main_class_name, main_class_method) = main_class in
  (* These two have to be done first as they write to the global method table *)
  let main_class_decl_ir3 = jlite_to_IR3_class_decl (main_class_name, [], [main_class_method]) in
  let aux_classes_decls_ir3 = List.map jlite_to_IR3_class_decl aux_classes in
  (* Now process the methods *)
  let main_class_md_ir3 = jlite_to_IR3_method_decl main_class_name main_class_method in
  let main_class_md_ir3 = { main_class_md_ir3 with S3.id3 = "main" } in
  let aux_classes_mds_ir3 = jlite_to_IR3_aux_classes aux_classes in
  main_class_decl_ir3 :: aux_classes_decls_ir3, main_class_md_ir3, aux_classes_mds_ir3
