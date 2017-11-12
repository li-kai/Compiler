open Ir3_structs

(*
  Algorithm 8.5: Partition three-address instructions into basic blocks

  Input: A sequence of three-address instructions

  Output: A list of basic blocks for that sequence in which
  each instruction is assigned to exactly one basic block.

  Method:
  1. Determine instructions that are leaders
    - First three-address instruction is a leader
    - Any instruction that is the target of a conditional
      or unconditional jump is a leader
    - Any instruction that immediately follows a condition
      or unconditional jump is a leader
  2. For each leader, its basic block consists of itself
     and all instructions up to but not including the
     next leader or the end of the intermediate program.
*)

type block =
  {
    id: int;
    mutable stmts: ir3_stmt list;
  }

let rec identify_leaders (fn: (ir3_stmt list)) =
    match fn with
    | [] -> []
    | stmt::tail -> (false, stmt) :: identify_leaders tail

let fn_to_basic_blocks (fn: (ir3_stmt list)) =
  let stmts_with_leaders = identify_leaders fn in
  match stmts_with_leaders with
  | [] -> []
  | stmt::stmts -> []::stmts
