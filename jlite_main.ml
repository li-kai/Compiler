
(* ===================================================== *)
(* ============== CS41212 Compiler Design ============== *)
(* ===================================================== *)

open Jlite_annotatedtyping
open Ir3_structs
open Jlite_toir3
open Optimize_ir3

let source_files = ref []
let optimized = ref false

let usage_msg = Sys.argv.(1) ^ " <source files>"

let set_source_file arg = source_files := arg :: !source_files

let parse_file file_name =
  let org_in_chnl = open_in file_name in
  let lexbuf = Lexing.from_channel org_in_chnl in
  try
  	print_string "Parsing...\n" ;
	print_string file_name ;
	print_string "\n" ;
	let prog =  Jlite_parser.input (Jlite_lexer.token file_name) lexbuf in
	close_in org_in_chnl;
    prog
  with
	End_of_file -> exit 0


let speclist = [("-O", Arg.Set optimized, "Enables all optimizations");]

let process prog =
  begin
    (* print_string (Jlite_structs.string_of_jlite_program prog); *)
    let typedprog= (Jlite_annotatedtyping.type_check_jlite_program prog) in
    (* print_string (Jlite_structs.string_of_jlite_program typedprog); *)
    let ir3prog = Jlite_toir3.jlite_program_to_IR3 typedprog in

    let ir3prog =  if !optimized then 
      Optimize_ir3.optimize_prog ir3prog 
    else
      ir3prog in

    print_string (Ir3_structs.string_of_ir3_program ir3prog);

    let asmprog = Ir3_to_arm.prog_to_arm ir3prog in
    print_string (Arm_structs.string_of_arm_prog asmprog);

    let prog_blocks = Basic_blocks.prog_to_blocks ir3prog in
    ()
    (* let new_basic_blocks = Global_optimization.Liveness_analysis.obtain_new_basic_blocks prog_blocks in
     * print_endline @@ Global_optimization.Liveness_analysis.string_of_new_basic_blocks new_basic_blocks *)
  end
let _ =
  begin
	Arg.parse speclist (fun x -> source_files:=!source_files@[x]) usage_msg;
    match !source_files with
    | [] -> print_string "no file provided \n"
    | x::_->
      let prog = parse_file x in
      process prog
  end


