open Ir3_structs
open Arm_structs

exception Fatal;;

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
  | Some idx -> 24 + 4 * idx
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

let expr_to_arm (expr: Ir3_structs.ir3_exp) =
  match expr with
  | BinaryExpr3 (op, lhs, rhs) ->
     begin
       match op, lhs, rhs with
       | (BooleanOp op, BoolLiteral3 x, BoolLiteral3 y) -> PseudoInstr "TODO"
       | (ArithmeticOp op, IntLiteral3 x, IntLiteral3 y) -> PseudoInstr "TODO"
       | (RelationalOp op, IntLiteral3 x, IntLiteral3 y) -> PseudoInstr "TODO"
       | (RelationalOp "==", BoolLiteral3 x, BoolLiteral3 y)
         | (RelationalOp "!=", BoolLiteral3 x, BoolLiteral3 y)
         | _, _, _ -> failwith "Invalid BinaryExpr3"
     end
  | UnaryExp3 (op, operand) ->
     begin
       match op, operand with
       | UnaryOp "-", IntLiteral3 x -> PseudoInstr "TODO"
       | UnaryOp "!", BoolLiteral3 x -> PseudoInstr "TODO"
       | _, _ -> failwith "Invalid UnaryExp3"
     end
  | FieldAccess3 (cname, fname) -> PseudoInstr "TODO"
  | Idc3Expr idc3 -> PseudoInstr "TODO"
  | MdCall3 (mname, params) -> PseudoInstr "TODO"
  | ObjectCreate3 cname -> PseudoInstr "TODO"

let stmt_to_arm
    (stmt: ir3_stmt) (md: md_decl3) : arm_program * arm_program =

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
  | _ -> raise Fatal


let rec stmts_to_arm
    (stmts: ir3_stmt list) (md: md_decl3) : arm_program * arm_program =
  match stmts with
  | [] -> [], []
  | head::tail ->
    begin
      let stmt_data, stmt_instr = stmt_to_arm head md in
      let rest_data, rest_instr = stmts_to_arm tail md in
      stmt_data @ rest_data, stmt_instr @ rest_instr
    end

let md_to_arm
    (md: md_decl3) : arm_program * arm_program =
  let start_instr =
    PseudoInstr("\n" ^ md.id3 ^ ":") ::
    STMFD ("fp" :: "lr" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
    ADD ("", false, "sp", "fp", immediate_int 24) ::
    [] in
  let end_instr =
    PseudoInstr ("\n" ^ exit_label md ^ ":") ::
    SUB ("", false, "sp", "fp", immediate_int 24) ::
    LDMFD ("fp" :: "pc" :: "v1" :: "v2" :: "v3" :: "v4" :: "v5" :: []) ::
    [] in
  let md_data, md_instr = stmts_to_arm md.ir3stmts md in
  (md_data, start_instr @ md_instr @ end_instr)

let rec mds_to_arm
    (md_list: md_decl3 list) : arm_program * arm_program =

  match md_list with
  | [] -> [], []
  | head::tail ->
    begin
      let md_data, md_instr = md_to_arm head in
      let rest_data, rest_instr = mds_to_arm tail in
      md_data @ rest_data, md_instr @ rest_instr
    end

let prog_to_arm
    (prog: ir3_program) : arm_program =

  let cdata_list, main_md, md_list = prog in
  let main_data, main_instr = md_to_arm main_md in
  let class_data, class_instr = mds_to_arm md_list in
  let prog_data = main_data @ class_data in
  let prog_instr = main_instr @ class_instr in
  prog_data @ prog_instr
