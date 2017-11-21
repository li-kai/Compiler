open Ir3_structs
open Arm_structs

exception Fatal;;

let labelcount = ref 0 
let fresh_label () = 
  (labelcount:= !labelcount + 1; "L" ^ (string_of_int !labelcount))

let rec range ?(start=0) len =
  if start >= len
  then []
  else start :: (range len ~start:(start+1))


let list_find_with_index lst x ~key =
  let rec aux lst idx =
    match lst with
    | hd::tl ->
      if key hd = x then Some idx
      else aux tl (idx+1)
    | [] -> None
  in
  aux lst 0

(* Offset of a field from the class itself *)
let offset_of_field (var_decl_list: Ir3_structs.var_decl3 list) (field_name: string) =
  match list_find_with_index var_decl_list field_name (fun (typ, id) -> id) with
  | Some idx -> idx * 4
  | None -> failwith "Invalid field access"

(* Offset of a class field from the class itself *)
let offset_of_class_field (ir3_prog: Ir3_structs.ir3_program) (class_name: Ir3_structs.cname3) (field_name: string) : int =
  let rec aux lst =
    match lst with
    | (name, var_decl_list)::tl ->
      if name = class_name then offset_of_field var_decl_list field_name
      else aux tl
    | [] -> failwith "Invalid class name"
  in
  let (cdata3, _, _) = ir3_prog in
  aux cdata3

(* Offset of a local variable from fp *)
let offset_of_var (md_decl3: Ir3_structs.md_decl3) (var_name: string) : int =
  let var_idx = list_find_with_index (md_decl3.Ir3_structs.localvars3 @ md_decl3.Ir3_structs.params3) var_name (fun (typ, id) -> id) in
  match var_idx with
  | Some idx -> 24 + 4 + 4 * idx
  | _ -> failwith "Invalid local variable access"

let immediate_int
    (i: int): operand2_type =
  ImmedOp ("#" ^ (string_of_int i))

let enter_label
    (md: md_decl3): string =
  "." ^ md.id3 ^ "_enter"

let exit_label
    (md: md_decl3): string =
  "." ^ md.id3 ^ "_exit"

let get_class_size (cname: Ir3_structs.cname3) (ir3_program: Ir3_structs.ir3_program) : int =
  let (cdata3_lst, _, _) = ir3_program in
  let rec aux lst =
    match lst with
    | (name, varlist)::tl ->
        if name = cname then 4 * (List.length varlist)
        else aux tl
    | [] -> failwith "Invalid class name"
  in
  aux cdata3_lst

let get_var_type (vname: Ir3_structs.id3) (md3: Ir3_structs.md_decl3) : Ir3_structs.ir3_type =
  let rec aux lst =
    match lst with
    | (typ, name) :: tl ->
       if name = vname then typ
       else aux tl
    | [] -> failwith "Invalid variable type lookup"
  in
  aux (md3.Ir3_structs.localvars3 @ md3.Ir3_structs.params3)

let convert_idc3 (idc3: Ir3_structs.idc3) (reg: string) (md3: Ir3_structs.md_decl3) : arm_program * (arm_program * arm_program) =
  match idc3 with
  | IntLiteral3 i -> [], ([MOV ("", false, reg, immediate_int i)], [])
  | BoolLiteral3 b ->
     begin
       match b with
       | true -> [], ([MOV ("", false, reg, immediate_int 1)], [])
       | false -> [], ([MOV ("", false, reg, immediate_int 0)], [])
     end
  | StringLiteral3 s ->
     begin
       let label_str = fresh_label () in
       let lbl_instr = [PseudoInstr (label_str^":"); PseudoInstr (".asciz \"" ^ s ^ "\\n\"")] in
       let load_instr = LDR ("", "", reg, LabelAddr ("="^label_str)) in
       lbl_instr, ([load_instr], [])
     end
  | Var3 v ->
     let offset = offset_of_var md3 v in
     [], ([LDR ("", "", reg, RegPreIndexed ("fp", -offset, false))], [STR ("", "", reg, RegPreIndexed ("fp", -offset, false))])

let prepare_md_call (args: Ir3_structs.idc3 list) (md3: Ir3_structs.md_decl3) : arm_program * (arm_program * arm_program) =
  let rev_args = List.rev args in
  let adjust_sp, cleanup_sp =
    if List.length args > 4 then
      [SUB ("", false, "sp", "sp", immediate_int (4 * (List.length args - 4)))],
      [ADD ("", false, "sp", "sp", immediate_int (4 * (List.length args - 4)))]
    else [], []
  in
  let rec aux idx rev_args =
    match rev_args with
    | hd::tl ->
       let init, current_instr =
         if idx < 4 then
           let init, (bef, aft) = convert_idc3 hd ("a"^(string_of_int (idx+1))) md3 in
           init, bef
         else
           let init, (bef, aft) = convert_idc3 hd "a1" md3 in
           init, bef @ [STR ("", "", "v1", RegPreIndexed ("sp", 4 * (idx - 4), false))] @ aft
       in
       let rest_init, rest_instr = (aux (idx-1) tl) in
       (init @ rest_init, current_instr @ rest_instr)
    | [] -> [], []
  in
  let init, result = aux (List.length rev_args - 1) rev_args in
  init, (adjust_sp @ result, cleanup_sp)

let expr_to_arm (expr: Ir3_structs.ir3_exp) (md3: Ir3_structs.md_decl3) (ir3_program: Ir3_structs.ir3_program) : arm_program * arm_program =
  match expr with
  | BinaryExp3 (op, lhs, rhs) ->
     begin
       let init1, (bef1, aft1) = convert_idc3 lhs "a1" md3 in
       let init2, (bef2, aft2) = convert_idc3 rhs "a2" md3 in
       match op, lhs, rhs with
       | Jlite_structs.BooleanOp op, x, y ->
          begin
            match op with
            | "||" -> init1 @ init2, bef1 @ bef2 @ [ORR ("", false, "a1", "a1", RegOp ("a2"))]
            | "&&" -> init1 @ init2, bef1 @ bef2 @ [AND ("", false, "a1", "A1", RegOp ("a2"))]
            | _ -> failwith "Unknown BooleanOp"
          end
       | Jlite_structs.AritmeticOp op, x, y ->
          begin
            match op with
            | "+" -> init1 @ init2, bef1 @ bef2 @ [ADD ("", false, "a1", "a1", RegOp ("a2"))]
            | "-" -> init1 @ init2, bef1 @ bef2 @ [SUB ("", false, "a1", "a1", RegOp ("a2"))]
            | "*" -> init1 @ init2, bef1 @ bef2 @ [MUL ("", false, "a3", "a1", "a2"); MOV ("", false, "a1", RegOp ("a3"))]
            | _ -> failwith "Unknown AritmeticOp"
          end
       | Jlite_structs.RelationalOp op, x, y ->
          begin
            match op with
            | "==" ->
               let prog = CMP ("", "a1", RegOp "a2") ::
                            MOV ("EQ", false, "a1", immediate_int 1) ::
                              MOV ("NE", false, "a1", immediate_int 0) :: [] in
               init1 @ init2 , bef1 @ bef2 @ prog
            | "!=" ->
               let prog = CMP ("", "a1", RegOp "a2") ::
                            MOV ("NE", false, "a1", immediate_int 1) ::
                              MOV ("EQ", false, "a1", immediate_int 0) :: [] in
               init1 @ init2 , bef1 @ bef2 @ prog
            | ">" ->
               let prog = CMP ("", "a1", RegOp "a2") ::
                            MOV ("GT", false, "a1", immediate_int 1) ::
                              MOV ("LE", false, "a1", immediate_int 0) :: [] in
               init1 @ init2 , bef1 @ bef2 @ prog
            | "<" ->
               let prog = CMP ("", "a1", RegOp "a2") ::
                            MOV ("LT", false, "a1", immediate_int 1) ::
                              MOV ("GE", false, "a1", immediate_int 0) :: [] in
               init1 @ init2 , bef1 @ bef2 @ prog
            | ">=" ->
               let prog = CMP ("", "a1", RegOp "a2") ::
                            MOV ("GE", false, "a1", immediate_int 1) ::
                              MOV ("LT", false, "a1", immediate_int 0) :: [] in
               init1 @ init2 , bef1 @ bef2 @ prog
            | "<=" ->
               let prog = CMP ("", "a1", RegOp "a2") ::
                            MOV ("LE", false, "a1", immediate_int 1) ::
                              MOV ("GT", false, "a1", immediate_int 0) :: [] in
               init1 @ init2 , bef1 @ bef2 @ prog
            | _ -> failwith "Unknown RelationalOp"
          end
       | _, _, _ -> failwith "Invalid BinaryExpr3"
     end
  | UnaryExp3 (op, operand) ->
     begin
       match op, operand with
       | Jlite_structs.UnaryOp "-", x ->
          let init, (bef, aft) = convert_idc3 x "a1" md3 in
          init, bef @ [RSB ("", false, "a1", "a1", immediate_int 0)]
       | Jlite_structs.UnaryOp "!", x ->
          let init, (bef, aft) = convert_idc3 x "a1" md3 in
          init, bef @ [EOR ("", false, "a1", "a1", immediate_int 1)]
       | _, _ -> failwith "Invalid UnaryExp3"
     end
  | FieldAccess3 (vname, fname) ->
     let init, (bef, aft) = convert_idc3 (Var3 vname) "a2" md3 in
     let var_type = get_var_type vname md3 in
     let class_name =
       match var_type with
       | Jlite_structs.ObjectT obj -> obj
       | _ -> failwith "Calling FieldAccess invalid object"
     in
     let field_offset = offset_of_class_field ir3_program class_name fname in
     init, bef @ [LDR ("", "", "a1", RegPreIndexed ("a2", -field_offset, false))]
  | Idc3Expr idc3 ->
     let init, (bef, aft) = convert_idc3 idc3 "a1" md3 in
     init, bef
  | MdCall3 (mname, args) ->
     let init, (prep, cleanup) = prepare_md_call args md3 in
     init, prep @ [BL ("", mname)] @ cleanup
  | ObjectCreate3 cname ->
     let class_size = get_class_size cname ir3_program in
     [], [MOV ("", false, "a1", immediate_int class_size); BL ("", "_Znwj(PLT)")]

let stmt_to_arm
    (stmt: ir3_stmt) (md: md_decl3) (ir3_prog: ir3_program) : arm_program * arm_program =
  match stmt with
  | Label3 label ->
    begin
      [], [PseudoInstr ("\n." ^ (string_of_int label) ^ ":")]
    end
  | ReturnVoidStmt3 ->
    begin
      let data = [] in
      let mov_instr = MOV ("", false, "a1", immediate_int 0) in
      let branch_instr = B ("", exit_label md) in
      let instr = mov_instr :: branch_instr :: [] in
      data, instr
    end
  | GoTo3 label ->
    begin
      [], [B ("", "." ^ (string_of_int label))]
    end
  | ReadStmt3 id3 ->
    begin      
      failwith "Unhandled ir3 statement: ReadStmt3"
    end
  | PrintStmt3 idc3 ->
    begin
      let label_str = fresh_label() in
      match idc3 with 
      | StringLiteral3 str ->
        [PseudoInstr (label_str^":"); PseudoInstr (".asciz \"" ^ str ^ "\\n\"")],
        LDR ("", "", "a1", (LabelAddr ("=" ^ label_str))) :: 
        [BL ("", "printf(PLT)")]

      | IntLiteral3 i -> 
        [PseudoInstr (label_str^":"); PseudoInstr (".asciz \"%i\\n\"")],
        LDR ("", "", "a1", (LabelAddr ("=" ^ label_str))) :: 
        MOV ("", false, "a2", (immediate_int i)) :: 
        [BL ("", "printf(PLT)")]

      | Var3 var_id3 -> 
        let var_type = get_var_type var_id3 md in
        let format_string =
          match var_type with
          | Jlite_structs.IntT | Jlite_structs.BoolT -> "\"%i\""
          | Jlite_structs.StringT -> "\"%s\""
          | Jlite_structs.ObjectT _ | Jlite_structs.VoidT | Jlite_structs.Unknown -> failwith "Unknown type!"
        in
        [PseudoInstr (label_str^":"); PseudoInstr (".asciz "^format_string)],
        LDR ("", "", "a1", (LabelAddr ("=" ^ label_str))) ::
        LDR ("", "", "a2", (RegPreIndexed ("fp", - offset_of_var md var_id3, false))) ::
        [BL ("", "printf(PLT)")]
      | _ -> failwith "Unhandled type"
    end
  | ReturnStmt3 id3 ->
     let offset = offset_of_var md id3 in
     let ldr = LDR ("", "", "a1", RegPreIndexed ("fp", -offset, false)) in
     [], [ldr]
  | MdCallStmt3 expr -> expr_to_arm expr md ir3_prog
  | IfStmt3 (expr, label3) ->
     let expr_init, expr_instrs = expr_to_arm expr md ir3_prog in
     let prog = CMP ("", "a1", immediate_int 1) :: B ("EQ", "."^(string_of_int label3)) :: [] in
     (* TODO: this can be further optimized *)
     expr_init, expr_instrs @ prog
  | AssignStmt3 (vname, expr) ->
     let expr_init, expr_instrs = expr_to_arm expr md ir3_prog in
     let var_offset = offset_of_var md vname in
     let prog = [STR ("", "", "a1", RegPreIndexed ("fp", -var_offset, false))] in
     expr_init, expr_instrs @ prog
  | AssignFieldStmt3 (expr1, expr2) ->
     let voffset, foffset =
       begin
        match expr1 with
        | FieldAccess3 (vname, fname) ->
            let voffset = offset_of_var md vname in
            let var_type = get_var_type vname md in
            let cname =
              match var_type with
              | Jlite_structs.ObjectT cname -> cname
              | _ -> "Invalid FieldAccess"
            in
            let foffset = offset_of_class_field ir3_prog cname fname in
            voffset, foffset
        | _ -> failwith "Attempting AssignFieldStmt3 to something other than FieldAccess"
       end
     in
     let expr2_init, expr2_instrs = expr_to_arm expr1 md ir3_prog in
     let mov_base_instr = LDR ("", "", "a2", RegPreIndexed ("fp", -voffset, false)) in
     let assign_instr = STR ("", "", "a1", RegPreIndexed ("a2", -foffset, false)) in
     expr2_init, expr2_instrs @ [mov_base_instr] @ [assign_instr]

let rec stmts_to_arm
    (stmts: ir3_stmt list) (md: md_decl3) (ir3_prog: ir3_program) : arm_program * arm_program =
  match stmts with
  | [] -> [], []
  | head::tail ->
    begin
      let stmt_data, stmt_instr = stmt_to_arm head md ir3_prog in
      let rest_data, rest_instr = stmts_to_arm tail md ir3_prog in
      stmt_data @ rest_data, stmt_instr @ rest_instr
    end

(* loads first four params from a1-a4, and the rest from caller's stack *)
let load_params_onto_stack
    (num_params: int) (num_localvars: int): arm_program =
  let stack_size_until_params = 24 + 4 + 4 * num_localvars in
  let param_num_to_instr (param_num: int): arm_program =
    if param_num < 4 then
      STR ("", "", "a" ^ (string_of_int (param_num + 1)), (RegPreIndexed ("fp", -(stack_size_until_params + 4 * param_num), false))) ::
      []
    else
      LDR ("", "", "v1", (RegPreIndexed ("fp", (4 * (param_num - 3)), false))) ::
      STR ("", "", "v1", (RegPreIndexed ("fp", -(stack_size_until_params + 4 * param_num), false))) ::
      [] in
  List.flatten (List.map param_num_to_instr (range num_params))

let md_to_arm
    (md: md_decl3) (prog: ir3_program) : arm_program * arm_program =
  let start_instr =
    PseudoInstr("\n" ^ md.id3 ^ ":") ::
    STMFD ("fp" :: "lr" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
    ADD ("", false, "fp", "sp", immediate_int 24) ::
    [] in

  let save_local_data_instr =
    SUB ("", false, "sp", "fp", immediate_int (24 + 4 * List.length (md.localvars3 @ md.params3))) ::
    [] in

  let params_store_instr = load_params_onto_stack (List.length md.params3) (List.length md.localvars3) in

  let end_instr =
    PseudoInstr ("\n" ^ exit_label md ^ ":") ::
    SUB ("", false, "sp", "fp", immediate_int 24) ::
    LDMFD ("fp" :: "pc" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
    [] in

  let md_data, md_instr = stmts_to_arm md.ir3stmts md prog in
  (md_data, start_instr @ save_local_data_instr @ params_store_instr @ md_instr @ end_instr)

let rec mds_to_arm
    (md_list: md_decl3 list) (prog: ir3_program) : arm_program * arm_program =

  match md_list with
  | [] -> [], []
  | head::tail ->
    begin
      let md_data, md_instr = md_to_arm head prog in
      let rest_data, rest_instr = mds_to_arm tail prog in
      md_data @ rest_data, md_instr @ rest_instr
    end

let prog_to_arm
    (prog: ir3_program) : arm_program =

  let cdata_list, main_md, md_list = prog in
  let main_data, main_instr = md_to_arm main_md prog in
  let class_data, class_instr = mds_to_arm md_list prog in
  let prog_data = main_data @ class_data in
  let prog_instr = main_instr @ class_instr in
  let data =
    begin
      PseudoInstr (".data") ::
      PseudoInstr ("") ::
      prog_data
    end in
  let text =
    begin
      PseudoInstr ("\n.text") ::
      PseudoInstr ("\n.global main") ::
      prog_instr @
      [PseudoInstr ("\n")]
    end in
  data @ text
