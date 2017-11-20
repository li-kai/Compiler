open Ir3_structs
open Arm_structs

exception Fatal;;

let immediate_int
    (i: int): operand2_type =
  ImmedOp ("#" ^ (string_of_int i))

let enter_label
    (md: md_decl3): string =
  "." ^ md.id3 ^ "_enter"

let exit_label
    (md: md_decl3): string =
  "." ^ md.id3 ^ "_exit"

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
