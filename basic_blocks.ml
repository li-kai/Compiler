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

let identify_leaders (fn: (ir3_stmt list)) =
  let rec helper (fn: (ir3_stmt list)) =
    match fn with
      | [] -> []
      (* Target of a jump // assume all labels are used *)
      | (Label3 label)::tail -> (true, Label3 label)::helper(tail)
      (* Following a jump *)
      | (IfStmt3 (x, y))::tail -> (
        match helper(tail) with
          | [] -> (false, IfStmt3 (x, y))::[]
          | (_, new_stmt)::new_tail -> (false, IfStmt3 (x, y))::(true, new_stmt)::new_tail
      )
      | (GoTo3 label)::tail -> (
        match helper(tail) with
          | [] -> (false, GoTo3 label)::[]
          | (_, new_stmt)::new_tail -> (false, GoTo3 label)::(true, new_stmt)::new_tail
      )
      | head::tail -> (false, head)::helper(tail)
  in
  (* First instruction is false because new head block is always created in split_by_leader *)
  match fn with
    | [] -> []
    | first_stmt::tail -> (false, first_stmt)::helper(tail)

let blkcount = ref (-1)
let fresh_blk () =
  blkcount:= !blkcount + 1;
  { id = !blkcount; stmts = [] }

let rec split_by_leader (fn: (bool * ir3_stmt) list): block list =
  match fn with
    | [] -> []
    | (boolean, head)::tail -> (
      match split_by_leader(tail) with
        | [] -> let blk = fresh_blk() in
          [{ blk with stmts = [head] }]
        | arr_head::tail ->
          if boolean == true then
            let finished_block = { arr_head with stmts = head::arr_head.stmts } in
            [fresh_blk()] @ [finished_block] @tail
          else
            let joined_block = { arr_head with stmts = head::arr_head.stmts } in
            [joined_block] @tail
    )

let fn_to_basic_blocks (fn: (ir3_stmt list)) =
  let stmts_with_leaders = identify_leaders fn in
  split_by_leader stmts_with_leaders

let string_of_basic_block (blk: block) =
"\nId " ^ string_of_int blk.id ^ " stmts: " ^ (string_of_indented_stmt_list "\n"  string_of_ir3_stmt blk.stmts)
