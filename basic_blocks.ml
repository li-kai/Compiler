open Ir3_structs

(* Defining type constructs to be used below *)

module StringSet = Set.Make(
  struct
    let compare = String.compare
    type t = id3
  end
  );;
type id3_set = StringSet.t

type line = {
  no: int;
  stmt: ir3_stmt;
  (* next_use: id3_set; *)
  (* live: id3_set; *)
}

type block_id = string
type block =
  {
    id: block_id;
    lines: line list;
    (* live_in: id3_set; *)
    (* live_out: id3_set; *)
  }

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

let identify_leaders (fn: (ir3_stmt list)) =
  let rec helper (fn: (ir3_stmt list)) =
    let make_leader pre tail =
      match helper(tail) with
        | [] -> (false, pre)::[]
        | (_, new_stmt)::new_tail -> (false, pre)::(true, new_stmt)::new_tail
    in
    match fn with
      | [] -> []
      (* Target of a jump // assume all labels are used *)
      | (Label3 label)::tail -> (true, Label3 label)::helper(tail)
      (* Following a jump *)
      | (ReturnStmt3 x)::tail -> make_leader (ReturnStmt3 x) tail
      | (ReturnVoidStmt3)::tail -> make_leader (ReturnVoidStmt3) tail
      | (MdCallStmt3 x)::tail -> make_leader (MdCallStmt3 x) tail
      | (GoTo3 x)::tail -> make_leader (GoTo3 x) tail
      | (IfStmt3 (x, y))::tail -> make_leader (IfStmt3 (x, y)) tail
      | head::tail -> (false, head)::helper(tail)
  in
  (* First instruction is false because new head block is always created in split_by_leader *)
  match fn with
    | [] -> []
    | first_stmt::tail -> (false, first_stmt)::helper(tail)

let blkcount = ref (-1)
let fresh_blk () =
  blkcount:= !blkcount + 1;
  { id = string_of_int !blkcount;
    lines = [];
    (* live_in = Id3Set.empty; *)
    (* live_out = Id3Set.empty; *)
  }

  (* For testing *)
let reset_blk () =
  blkcount:= -1

let linecount = ref (-1)

let fresh_line (stmt) =
  {
    no = 0;
    stmt;
    (* next_use = Id3Set.empty; *)
    (* live = Id3Set.empty; *)
  }

let rec split_by_leader (fn: (bool * ir3_stmt) list): block list =
  match fn with
  | [] -> []
  | (boolean, head)::tail ->
    begin
      match split_by_leader(tail) with
      | [] -> let blk = fresh_blk() in
        [{ blk with lines = [fresh_line head] }]
      | arr_head::tail ->
        if boolean == true then
          let finished_block = {
            arr_head with lines = (fresh_line head)::arr_head.lines
          } in [fresh_blk()] @ [finished_block] @tail
        else
          let joined_block = {
            arr_head with lines = (fresh_line head)::arr_head.lines
          } in [joined_block] @tail
    end


let fn_to_basic_blocks (fn: (ir3_stmt list)) =
  let stmts_with_leaders = identify_leaders fn in
  split_by_leader stmts_with_leaders

let string_of_id3_set (set: id3_set) =
  String.concat ", " (StringSet.elements set)

let string_of_line (line: line) =
  let stmt_str = string_of_ir3_stmt line.stmt in
  (* let next_use_str = string_of_id3_set line.next_use in *)
  (* let live_str = string_of_id3_set line.live in *)
  "\n\tStmt: "^ stmt_str
  (* "\n\tStmt: " ^ stmt_str ^ "\n\tNext use: " ^ next_use_str *)
  (* ^ "\n\tLive: " ^ live_str *)

let string_of_basic_block (blk: block) =
  "\nId " ^ blk.id ^ " lines: " ^ (string_of_indented_stmt_list "\n"  string_of_line blk.lines)

(*
  Algorithm 8.4.3 Flow Graph

  Input: A list of basic blocks

  Output: A flow graph with def and use computed for each block

  Method:
    - There is an edge from block B to block C if and only if
      it is possible for the first instruction in block C to
      immediately follow the last instruction in block B.
    - There are two ways that such an edge could be justified:
      1. There is a conditional or unconditional jump from the end of B
         to the beginning of C.
      2. C immediately follows B in the original order of
         the three-address instructions, and B does not end
         in an unconditional jump.
 *)
(* We use an adjacency list to determine the edges *)
let empty_block = {
  id = "";
  lines = [];
  (* live_in = Id3Set.empty; *)
  (* live_out = Id3Set.empty; *)
}

let rec find_jump (fn: (line list)): (ir3_stmt option) =
  match fn with
  | [] -> None
  | line::[] -> (
      match line.stmt with
      | GoTo3 _ | MdCallStmt3 _ | IfStmt3 _ -> Some line.stmt
      | _ -> None
    )
  | head::tail -> find_jump(tail)

let find_dest_of_jump (blks: (block list)) (jump) =
  let target = match jump with
    | GoTo3 lb -> string_of_int lb
    | MdCallStmt3 x -> (
        match x with
        | MdCall3 (md, _) -> md
        | _ -> failwith "What is the method calling?"
      )
    | IfStmt3 (_, lb) -> string_of_int lb
    | _ -> failwith "Unknown jump type"
  in
  let identify_dest blk =
    match blk.lines with
    | [] -> false
    | hd::tail -> (
        match hd.stmt with
        | (Label3 label) -> (string_of_int label) = target
        | _ -> blk.id = target
      )
  in
  List.find identify_dest blks

let get_flow_graph (blks: (block list)) =
  let start_block = { empty_block with id = "start" } in
  let exit_block = { empty_block with id = "exit" } in
  let all_blocks = [start_block]@blks@[exit_block] in
  let tbl_out = Hashtbl.create (List.length all_blocks) in
  let _ =
    List.iter (fun blk -> Hashtbl.add tbl_out blk.id []) all_blocks
  in
  let _ = Hashtbl.add tbl_out "start" [(List.hd blks).id] in
  (* let _ = Hashtbl.add tbl_out (List.hd (List.rev blks)).id ["exit"] in *)
  let rec join_all_blocks blks: unit =
    match blks with
    | [] -> ();
    | exit::[] -> ();
    | head::next::tail ->
      let jump = find_jump head.lines in
      (* 1. There is a conditional or unconditional jump from the end of B
         to the beginning of C. *)
        let _ = (
          match jump with
          | Some inst ->
            let entry = Hashtbl.find tbl_out head.id in
            let dest_id = (find_dest_of_jump blks inst).id in
            if not(List.mem dest_id entry) then
              Hashtbl.add tbl_out head.id (dest_id::entry);
          | None -> ()
        ) in
        (* 2. C immediately follows B in the original order of
         the three-address instructions, and B does not end
         in an unconditional jump. *)
        let _ = (
          match jump with
            | Some (GoTo3 _) | Some (MdCallStmt3 _) -> ()
            (* Any other type are true *)
            | _ ->
              let entry = Hashtbl.find tbl_out head.id in
              if not(List.mem next.id entry) then
                Hashtbl.add tbl_out head.id (next.id::entry);
        ) in
        join_all_blocks (next::tail);
  in
  let _ = join_all_blocks all_blocks in
  tbl_out

type block_collection = {
  blocks: block list;
  edges_out: (block_id, block_id list) Hashtbl.t;
}

type 'a adj_list = ('a, 'a list) Hashtbl.t

(* Returns postorder traversal of the graph, which is a valid toposort if the graph g has one *)
let get_reverse_postorder (g: 'a adj_list) ~entry : 'a list =
  let n = Hashtbl.length g in
  let visited = Hashtbl.create n in
  let rev_postorder = ref [] in
  let rec dfs_rec u =
    List.iter (fun v -> if (not (Hashtbl.mem visited v)) then dfs_rec v) (Hashtbl.find g u);
    rev_postorder := u :: !rev_postorder;
    Hashtbl.replace visited u true;
  in
  dfs_rec entry;
  Hashtbl.iter (fun k _ -> if not(Hashtbl.mem visited k) then dfs_rec k) g;
  !rev_postorder

let line_count = ref (-1)
let number_lines_of_blk (blk_cl: block_collection): block_collection =
  let number_blk blk: block =
    let number_line line =
      line_count := !line_count + 1;
      { line with no = !line_count }
    in
    let numbered_lines = List.map number_line blk.lines in
    { blk with lines = numbered_lines }
  in
  let order_blks_id = get_reverse_postorder blk_cl.edges_out "start" in
  let find_blk id: block =
    List.find (fun blk -> id = blk.id) blk_cl.blocks
  in
  let order_blks = List.map find_blk order_blks_id in
  let numbered_order_blks = List.map number_blk order_blks in
  { blk_cl with blocks = numbered_order_blks }

let prog_to_blocks (prog: ir3_program): block_collection =
  let (c_list, c_mthd, mthd_list) = prog in
  let all_methods = c_mthd::mthd_list in
  let make_block mthd: block list =
    let mthd_blks = fn_to_basic_blocks mthd.ir3stmts in
    match mthd_blks with
    | [] -> []
    | hd::tail -> { hd with id = mthd.id3 }::tail
  in
  let all_blocks = List.flatten (List.map make_block all_methods) in
  let blk_cl = {
    blocks = all_blocks;
    edges_out = get_flow_graph all_blocks;
  }
  in number_lines_of_blk blk_cl

