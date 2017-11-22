module R = Jlite_report
module S = Jlite_structs
module Dup_check = Jlite_duplicate_check

open S
   
type local_environment = (string * S.jlite_type) list

type field_declaration = string * S.jlite_type

type method_signature = string * (S.jlite_type * (S.jlite_type list))

type class_descriptor = (string * ((field_declaration list) * (method_signature list))) list

type type_environment = class_descriptor * local_environment

let string_of_method_signature (name, (rettype, params_type)) =
  let string_of_params_type = String.concat ", " @@ List.map S.string_of_jlite_type params_type in
  (S.string_of_jlite_type rettype) ^ " " ^ name ^ " (" ^ string_of_params_type ^ ")"
                          
let local_env_lookup local_env id =
  let rec aux lst =
    match lst with
    | (v, t, scope) :: tl -> if v = id then Some t, scope
                             else aux tl
    | [] -> None, -1
  in
  aux local_env

let local_env_aug local_env id t scope = (id, t, scope) :: local_env

let find_opt (pred: 'a -> bool) (lst: 'a list) : 'a option =
  let rec aux lst =
    match lst with
    | hd :: tl -> if pred hd then Some hd else aux tl
    | [] -> None
  in
  aux lst

let find_all (pred: 'a -> bool) (lst: 'a list) : 'a list =
  let rec aux lst acc =
    match lst with
    | hd :: tl -> if pred hd then aux tl (hd :: acc)
                  else aux tl acc
    | [] -> List.rev acc
  in
  aux lst []

let assoc_opt (key: 'a) (lst: ('a * 'b) list) : 'b option =
  let rec aux lst =
    match lst with
    | (k, v) :: tl -> if key = k then Some v else aux tl
    | [] -> None
  in
  aux lst

let force_TypedExp e =
  match e with
  | S.TypedExp (te, t) -> (te, t)
  | _ -> failwith @@ "Expected TypedExp. Got " ^ (S.string_of_jlite_expr e)

let initialize (main_class, aux_classes) =
  let process_class (class_name, var_decls, md_decls) =
    let fds = List.map (fun (typ, id) -> (S.string_of_var_id id, typ)) var_decls in
    let msigs = List.map (fun md_dec -> (S.string_of_var_id md_dec.S.jliteid, (md_dec.S.rettype, List.map (fun (typ, id) -> typ) md_dec.S.params))) md_decls in
    (class_name, (fds, msigs))
  in
  let (main_class_name, main_class_md_decl):(S.class_name * S.md_decl) = main_class in
  (process_class (main_class_name, [], [main_class_md_decl])) :: (List.map process_class aux_classes)

let update_method_names (class_name, var_decls, md_decls) =
  List.iteri (fun idx a -> a.S.ir3id <- S.SimpleVarId ("$" ^ class_name ^ "_" ^ (string_of_int idx))) md_decls

let is_assignable_from t1 t2 =
  match t1, t2 with
  | ObjectT _, Unknown -> true
  | StringT, Unknown -> true
  | _, _ -> t1 = t2

let is_method_compatible_with_call (md_sig: method_signature) (call: string * (S.jlite_type list)) =
  let (md_name, (md_rettype, md_params)) = md_sig in
  let (call_name, call_params_types) = call in
  call_name = md_name &&
    List.length md_params = List.length call_params_types &&
    List.for_all2 (fun typ1 typ2 -> is_assignable_from typ1 typ2) md_params call_params_types


let rec type_check_stmts (class_desc: class_descriptor) env stmts =
  let mapped_stmts = List.map (type_check_stmt class_desc env) stmts in
  let rec get_last_type lst =
    match lst with
    | [] -> failwith "Empty stmts?"
    | [(type_checked_stmt, typ)] -> typ
    | hd::tl -> get_last_type tl
  in
  (List.map (fun (s, t) -> s) mapped_stmts, get_last_type mapped_stmts)

and type_check_stmt (class_desc: class_descriptor) env (stmt: S.jlite_stmt) =
  match stmt with
  | S.IfStmt (e, s_true, s_false) ->
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     let type_checked_s_true, s_true_type = type_check_stmts class_desc env s_true in
     let type_checked_s_false, s_false_type = type_check_stmts class_desc env s_false in
     if e_type <> S.BoolT then
       let () = R.report_error @@ "Expected boolean expression in an IfStmt. Got " ^ (S.string_of_jlite_type e_type) ^ " instead. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
       (stmt, S.Unknown)
     else if s_true_type <> s_false_type then
       let () = R.report_error @@ "Expected the same return type from the true block and the false block in an IfStmt. Got " ^ (S.string_of_jlite_type s_true_type) ^ " for the true block and " ^ (S.string_of_jlite_type s_false_type) ^ " for the false block. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
       (stmt, S.Unknown)
     else
       (IfStmt (TypedExp (type_checked_e, e_type), type_checked_s_true, type_checked_s_false), s_true_type)
  | S.WhileStmt (e, s_true) ->
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     let type_checked_s_true, s_true_type = type_check_stmts class_desc env s_true in
     if e_type <> S.BoolT then
       let () = R.report_error @@ "Expected boolean expression in a WhileStmt. Got " ^ (S.string_of_jlite_type e_type) ^ " instead. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
       (stmt, S.Unknown)
     else
       (WhileStmt (TypedExp (type_checked_e, e_type), type_checked_s_true), s_true_type)
  | S.ReadStmt varid ->
     let varid_str = S.string_of_var_id varid in
     let (mapped_type, scope) = local_env_lookup env varid_str in
     begin
       match mapped_type with
       | Some S.IntT -> (ReadStmt (TypedVarId (varid_str, S.IntT, scope)), VoidT) (* TODO SCOPE *)
       | Some S.BoolT -> (ReadStmt (TypedVarId (varid_str, S.BoolT, scope)), VoidT) (* TODO SCOPE *)
       | Some S.StringT -> (ReadStmt (TypedVarId (varid_str, S.StringT, scope)), VoidT) (* TODO SCOPE *)
       | Some t ->
          let () = R.report_error @@ "Expected Int, Bool, or String in ReadStmt, got " ^ (S.string_of_jlite_type t) ^ " instead. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
          (stmt, S.Unknown)
       | None -> 
          let () = R.report_error @@ "Unbound variable " ^ varid_str ^ " in a ReadStmt. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
          (stmt, S.Unknown)
     end
  | S.PrintStmt e ->
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     begin
       match e_type with
       | S.IntT
         | S.BoolT
         | S.StringT -> (PrintStmt (TypedExp (type_checked_e, e_type)), VoidT)
       | _ ->
          let () = R.report_error @@ "Expected expression with type Int, Bool, or String in PrintStmt, got " ^ (S.string_of_jlite_type e_type) ^ " instead. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
          (stmt, S.Unknown)
     end
  | S.AssignStmt (varid, e) ->
     let varid_str = S.string_of_var_id varid in
     let (mapped_type, scope) = local_env_lookup env varid_str in
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     begin
       match mapped_type with
       | Some typ ->
          if is_assignable_from typ e_type then 
            (AssignStmt (TypedVarId (varid_str, typ, scope), TypedExp (type_checked_e, e_type)), VoidT)
          else
            let () = R.report_error @@ "Cannot assign an expression of type " ^ (if e_type = S.Unknown then "NULL" else S.string_of_jlite_type e_type) ^ " to a variable with type " ^ (S.string_of_jlite_type typ) ^ ". Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
            (stmt, S.Unknown)
       | None ->
          let () = R.report_error @@ "Unbound variable " ^ varid_str ^ " in an AssignStmt. Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
          (stmt, S.Unknown)
     end
  | S.AssignFieldStmt (e1, e2) ->
     let (type_checked_e1, e1_type) = force_TypedExp @@ type_check_expr class_desc env e1 in
     let (type_checked_e2, e2_type) = force_TypedExp @@ type_check_expr class_desc env e2 in
     begin
       match type_checked_e1 with
       | FieldAccess (e11, varid) ->
          if e1_type <> e2_type then
            begin
              match e1_type, e2_type with
              | ObjectT _, S.Unknown -> (AssignFieldStmt (TypedExp (type_checked_e1, e1_type), TypedExp (type_checked_e2, e2_type)), e1_type)
              | _, _ ->
                 let () = R.report_error @@ "Cannot assign an expression of type " ^ (if e2_type = S.Unknown then "NULL" else S.string_of_jlite_type e2_type) ^ " to a variable with type " ^ (S.string_of_jlite_type e1_type) ^ ". Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
                 (stmt, S.Unknown)
            end
          else
            (AssignFieldStmt (TypedExp (type_checked_e1, e1_type), TypedExp (type_checked_e2, e2_type)), e1_type)
       | _ ->
          let () = R.report_error @@ "Trying to do field assignment when the left hand side is not a field access, but a " ^ (S.string_of_jlite_type e1_type) ^ ". Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
          (stmt, S.Unknown)
     end
  | S.MdCallStmt e ->
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     begin
       match type_checked_e with
       | MdCall (e1, params) -> (MdCallStmt (TypedExp (type_checked_e, e_type)), e_type) (* Defer this to type_check_expr *)
       | _ -> failwith "There must be an MdCall inside MdCallStmt"
     end
  | S.ReturnStmt e ->
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     let (stored_ret, scope) = local_env_lookup env "$RET" in
     begin
       match stored_ret with
       | Some typ ->
          if typ = e_type then (ReturnStmt (TypedExp (type_checked_e, e_type)), e_type)
          else
            let () = R.report_error @@ "The expected return value is " ^ (S.string_of_jlite_type typ) ^ " while the return expression has type " ^ (S.string_of_jlite_type e_type) ^ ". Program fragment: \n" ^ (S.string_of_jlite_stmt stmt) in
            (stmt, S.Unknown)
       | None -> failwith "Where are we?"
     end
  | S.ReturnVoidStmt -> (stmt, VoidT)

and type_check_expr (class_desc: class_descriptor) env (expr: S.jlite_exp) =
  match expr with
  | S.UnaryExp (op, e) ->
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     begin
       match op, e_type with
       | UnaryOp "-", IntT -> TypedExp (UnaryExp (op, TypedExp (type_checked_e, e_type)), IntT)
       | UnaryOp "!", BoolT -> TypedExp (UnaryExp (op, TypedExp (type_checked_e, e_type)), BoolT)
       | _, _ ->
          let () = R.report_error @@ "Invalid UnaryExp: Trying to apply " ^ (S.string_of_jlite_op op) ^ " to an expression of type " ^ (S.string_of_jlite_type e_type) ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
          TypedExp (expr, S.Unknown)
     end
  | S.BinaryExp (op, e1, e2) ->
     let (type_checked_e1, e1_type) = force_TypedExp @@ type_check_expr class_desc env e1 in
     let (type_checked_e2, e2_type) = force_TypedExp @@ type_check_expr class_desc env e2 in
     begin
       match e1_type, e2_type, op with
       | IntT, IntT, AritmeticOp "+"
         | IntT, IntT, AritmeticOp "-"
         | IntT, IntT, AritmeticOp "*"
         | IntT, IntT, AritmeticOp "/" -> TypedExp (BinaryExp (op, TypedExp (type_checked_e1, e1_type), TypedExp (type_checked_e2, e2_type)), IntT)
       | IntT, IntT, RelationalOp ">"
         | IntT, IntT, RelationalOp "<"
         | IntT, IntT, RelationalOp "<="
         | IntT, IntT, RelationalOp ">="
         | IntT, IntT, RelationalOp "=="
         | IntT, IntT, RelationalOp "!="
         | BoolT, BoolT, RelationalOp "=="
         | BoolT, BoolT, RelationalOp "!="
         | BoolT, BoolT, BooleanOp "&&"
         | BoolT, BoolT, BooleanOp "||" -> TypedExp (BinaryExp (op, TypedExp (type_checked_e1, e1_type), TypedExp (type_checked_e2, e2_type)), BoolT)
       | _, _, _ ->
          let () = R.report_error @@ "Invalid BinaryExp: Trying to apply " ^ (S.string_of_jlite_op op) ^ " with LHS of type " ^ (S.string_of_jlite_type e1_type) ^ " and RHS of type " ^ (S.string_of_jlite_type e2_type) ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
          TypedExp (expr, S.Unknown)
     end
  | S.FieldAccess (e, varid) ->
     let varid_str = S.string_of_var_id varid in
     let (type_checked_e, e_type) = force_TypedExp @@ type_check_expr class_desc env e in
     begin
       match e_type with
       | ObjectT class_name ->
          let class_desc_entry = find_opt (fun (name, (_, _)) -> name = class_name) class_desc in
          begin
            match class_desc_entry with
            | Some (class_name, (fds, mds)) ->
               let fd_entry = assoc_opt varid_str fds in
               begin
                 match fd_entry with
                 | Some typ -> TypedExp (FieldAccess (TypedExp (type_checked_e, e_type), TypedVarId (varid_str, typ, 0)), typ)
                 | None ->
                    let () = R.report_error @@ "Attempting to access nonexistent field " ^ varid_str ^ " in object " ^ class_name ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
                    TypedExp (expr, S.Unknown)
               end
            | None ->
               let () = R.report_error @@ "Attempting to access field of a nonexistent class " ^ class_name ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
               TypedExp (expr, S.Unknown)
          end
       | _ ->
          let () = R.report_error @@ "Attempting to do field access on something other than object with type " ^ (S.string_of_jlite_type e_type) ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
          TypedExp (expr, S.Unknown)
     end
  | S.ObjectCreate class_name ->
     let class_desc_entry = find_opt (fun (name, (_, _)) -> name = class_name) class_desc in
     begin
       match class_desc_entry with
       | Some (class_name, (fds, mds)) -> TypedExp (ObjectCreate class_name, ObjectT class_name)
       | None ->
          let () = R.report_error @@ "Attempting to create an object of a nonexistent class " ^ (S.string_of_jlite_expr expr) ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
          TypedExp (expr, S.Unknown)
     end
  | S.MdCall (e, params) ->
     let typed_exp_params = List.map (type_check_expr class_desc env) params in
     let nparams = List.map force_TypedExp typed_exp_params in
     let nparams_no_names = List.map (fun (_, typ) -> typ) nparams in
     let find_matching_md class_name md_name =
       let class_desc_entry_m = assoc_opt class_name class_desc in
       begin
         match class_desc_entry_m with
         | Some (fds, mds) ->
            let md_matches (md_sig: method_signature) : bool =
              is_method_compatible_with_call md_sig (md_name, nparams_no_names)
            in
            let md_to_call_m = find_all md_matches mds in
            begin
              match md_to_call_m with
              | [(md_name, (md_rettype, md_params))] -> (md_name, (md_rettype, md_params))
              | [] ->
                 let () = R.report_error @@ "Attempting to do a method call on class " ^ class_name ^ " with method name = " ^ md_name ^ " without any matching method signature. Program fragment = " ^ (S.string_of_jlite_expr expr) in
                 ("", (S.Unknown, []))
              | _ ->
                 let () = R.report_error @@ "Unable to determine the method to call. Program fragment: " ^ (S.string_of_jlite_expr expr) ^ ". Candidates: " ^ (String.concat "\n" @@ List.map string_of_method_signature md_to_call_m) ^ "\n" in
                 ("", (S.Unknown, []))
            end
         | _ ->
            let () = R.report_error @@ "Attempting to do a method call on a nonexistent class " ^ class_name ^ ". Program fragment = " ^ (S.string_of_jlite_expr expr) in
            ("", (S.Unknown, []))
       end
     in
     (* Assign types to nulls in call params *)
     let assign_types_to_nulls (md_params: S.jlite_type list) (call_params: S.jlite_exp list) =
       assert (List.length md_params = List.length call_params);
       let rec aux md_params call_params acc =
         match md_params, call_params with
         | hd1::tl1, TypedExp (te, Unknown)::tl2 -> aux tl1 tl2 (TypedExp (te, hd1) :: acc)
         | hd1::tl1, hd2::tl2 -> aux tl1 tl2 (hd2 :: acc)
         | [], [] -> List.rev acc
         | _, _ -> failwith "Length of the two lists must be equal"
       in
       aux md_params call_params []
     in
     begin
       match e with
       (* Global call *)
       | FieldAccess (inner_e, varid) ->
          let varid_str = S.string_of_var_id varid in
          let tce, te = force_TypedExp (type_check_expr class_desc env inner_e) in
          begin
            match te with
            | ObjectT class_name ->
               let (md_name, (md_rettype, md_params)) = find_matching_md class_name varid_str in
               let typed_exp_params_without_null = assign_types_to_nulls md_params typed_exp_params in
               TypedExp (MdCall (FieldAccess (TypedExp (tce, te), varid), typed_exp_params_without_null), md_rettype)
            | _ ->
               let () = R.report_error @@ "Attempting to do a method call on " ^ (S.string_of_jlite_type te) ^ " (expected ObjectT). Program fragment = " ^ (S.string_of_jlite_expr expr) in
               TypedExp (expr, S.Unknown)
          end
       (* Local Call *)
       | Var (varid) ->
          let varid_str = S.string_of_var_id varid in
          let this_typ, scope = local_env_lookup env "this" in
          begin
            match this_typ with
            | Some (ObjectT class_name) ->
               let (md_name, (md_rettype, md_params)) = find_matching_md class_name varid_str in
               let typed_exp_params_without_null = assign_types_to_nulls md_params typed_exp_params in
               TypedExp (MdCall (Var (varid), typed_exp_params_without_null), md_rettype)
            | Some _ -> failwith "`this` must be bound to an ObjectT"
            | None -> failwith "Where are we?"
          end
       | _ ->
          let () = R.report_error @@ "Attempting to do a method call on " ^ (S.string_of_jlite_expr e) ^ ". Program fragment = " ^ (S.string_of_jlite_expr expr) in
          TypedExp (expr, S.Unknown)
     end
  | S.BoolLiteral _ -> TypedExp (expr, S.BoolT)
  | S.IntLiteral _ -> TypedExp (expr, S.IntT)
  | S.StringLiteral _ -> TypedExp (expr, S.StringT)
  | S.ThisWord ->
     let this_typ, scope = local_env_lookup env "this" in
     begin
       match this_typ with
       | Some typ -> TypedExp (expr, typ)
       | None -> failwith "Where are we?"
     end
  | S.NullWord -> TypedExp (expr, S.Unknown)
  | S.Var varid ->
     let varid_str = S.string_of_var_id varid in
     let (varid_typ, scope) = local_env_lookup env varid_str in
     begin
       match varid_typ with
       | Some typ -> TypedExp (Var (TypedVarId (varid_str, typ, scope)), typ)
       | None ->
          let () = R.report_error @@ "Attempting to access an unbound variable " ^ varid_str ^ ". Program fragment: \n" ^ (S.string_of_jlite_expr expr) in
          TypedExp (expr, S.Unknown)
     end
  | _ -> failwith "Unhandled jlite_exp"
     

let type_check_block (class_desc: class_descriptor) env localvars stmts =
  let localvar_mapping = List.map (fun (typ, varid) -> (S.string_of_var_id varid, typ, 2)) localvars in
  let env2 = localvar_mapping @ env in
  let (type_checked_stmts, block_type) = type_check_stmts class_desc env2 stmts in
  (localvars, type_checked_stmts)

let find_invalid_type (class_desc: class_descriptor) vars =
  find_opt
    (fun (typ, id) ->
      match typ with
      | ObjectT obj -> not (List.exists (fun (name, (_, _)) -> name = obj) class_desc)
      | IntT | BoolT | StringT | VoidT -> false
      | _ -> failwith "Should not exist"
    )
  vars

let type_check_md_decl class_name (class_desc: class_descriptor) env (md_decl: S.md_decl) =
  let invalid_type_in_params = find_invalid_type class_desc md_decl.params in
  let invalid_type_in_localvars = find_invalid_type class_desc md_decl.localvars in
  match invalid_type_in_params, invalid_type_in_localvars with
  | Some (t1, _), _ ->
     let () = R.report_error @@ "Invalid type " ^ (S.string_of_jlite_type t1) ^ " in the parameters of method " ^ (S.string_of_var_id md_decl.jliteid) ^ " in class " ^ class_name ^ " !" in
     md_decl
  | None, Some (t1, _) ->
     let () = R.report_error @@ "Invalid type " ^ (S.string_of_jlite_type t1) ^ " in the local variables of method " ^ (S.string_of_var_id md_decl.jliteid) ^ " in class " ^ class_name ^ " !" in
     md_decl
  | None, None ->
    let param_mapping = List.map (fun (typ, varid) -> (S.string_of_var_id varid, typ, 2)) md_decl.params in
    let ret_mapping = ("$RET", md_decl.rettype, -1) in
    let env2 = param_mapping @ [ret_mapping] @ env in
    let localvars2, stmts2 = type_check_block class_desc env2 md_decl.localvars md_decl.stmts in
    {md_decl with localvars = localvars2; stmts = stmts2}

let type_check_main_class (class_desc: class_descriptor) (main_class:S.class_name * S.md_decl) =
  let class_name, md_decl = main_class in
  let env = [("this", ObjectT class_name, 1)]  in 
  let type_checked_class = type_check_md_decl class_name class_desc env md_decl in
  (class_name, type_checked_class)

let type_check_aux_class class_desc aux_class =
  let class_name, var_decls, md_decls = aux_class in
  (* Class scope = 1 *)
  let invalid_type_in_class_vars = find_invalid_type class_desc var_decls in
  match invalid_type_in_class_vars with
  | Some (t1, _) ->
     let () = R.report_error @@ "Invalid type " ^ (S.string_of_jlite_type t1) ^ " in the class variables of class " ^ class_name ^ "!" in
     (class_name, var_decls, md_decls)
  | None ->
    let var_decls = List.map (fun (typ, id) -> (typ, TypedVarId (S.string_of_var_id id, typ, 1))) var_decls in
    let var_mapping = List.map (fun (typ, varid) -> (S.string_of_var_id varid, typ, 1)) var_decls in
    let env = ("this", ObjectT class_name, 1) :: (var_mapping) in
    let type_checked_md_decls = List.map (type_check_md_decl class_name class_desc env) md_decls in
    (class_name, var_decls, type_checked_md_decls)

let type_check_jlite_program (prog: S.jlite_program) =
  let (main_class, aux_classes) = prog in
  let (main_class_name, main_class_md_decl) = main_class in
  let () = Dup_check.find_duplicate_classes prog in
  let () = Dup_check.find_duplicate_methods (main_class_name, [], [main_class_md_decl]) in
  let () = List.iter Dup_check.find_duplicate_methods aux_classes in
  let () = Dup_check.find_duplicate_class_vars (main_class_name, [], [main_class_md_decl]) in
  let () = List.iter Dup_check.find_duplicate_class_vars aux_classes in
  let () = Dup_check.find_duplicate_method_args_ids (main_class_name, [], [main_class_md_decl]) in
  let () = List.iter Dup_check.find_duplicate_method_args_ids aux_classes in
  let () = Dup_check.ensure_unique_defs_in_class (main_class_name, [], [main_class_md_decl]) in
  let () = List.iter Dup_check.ensure_unique_defs_in_class aux_classes in
  let () = Dup_check.ensure_unique_defs_in_methods (main_class_name, [], [main_class_md_decl]) in
  let () = List.iter Dup_check.ensure_unique_defs_in_methods aux_classes in
  let () = update_method_names (main_class_name, [], [main_class_md_decl]) in
  let () = List.iter update_method_names aux_classes in
  let class_descriptor = initialize prog in
  let type_checked_main_class = type_check_main_class class_descriptor main_class in
  let type_checked_aux_classes = List.map (type_check_aux_class class_descriptor) aux_classes in
  (type_checked_main_class, type_checked_aux_classes)
