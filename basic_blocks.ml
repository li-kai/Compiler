open Ir3_structs

(* Defining type constructs to be used below *)

module Id3Set = Set.Make(
	struct
	    let compare = String.compare
	    type t = id3
	  end
  );;
type id3_set = Id3Set.t

type line = {
  stmt: ir3_stmt;
  next_use: id3_set;
  live: id3_set;
}

type block_id = int
type block =
  {
    id: block_id;
    lines: line list;
    live_in: id3_set;
    live_out: id3_set;
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
  { id = !blkcount;
    lines = [];
    live_in = Id3Set.empty;
    live_out = Id3Set.empty;
  }

  (* For testing *)
let reset_blk () =
  blkcount:= -1

let fresh_line (stmt) =
  {
    stmt;
    next_use = Id3Set.empty;
    live = Id3Set.empty;
  }

let rec split_by_leader (fn: (bool * ir3_stmt) list): block list =
  match fn with
    | [] -> []
    | (boolean, head)::tail -> (
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
    )

let fn_to_basic_blocks (fn: (ir3_stmt list)) =
  let stmts_with_leaders = identify_leaders fn in
  split_by_leader stmts_with_leaders

let string_of_id3_set (set: id3_set) =
  String.concat ", " (Id3Set.elements set)

let string_of_line (line: line) =
  let stmt_str = string_of_ir3_stmt line.stmt in
  let next_use_str = string_of_id3_set line.next_use in
  let live_str = string_of_id3_set line.live in
  "\n\tStmt: " ^ stmt_str ^ "\n\tNext use: " ^ next_use_str
  ^ "\n\tLive: " ^ live_str

let string_of_basic_block (blk: block) =
  "\nId " ^ string_of_int blk.id ^ " lines: " ^ (string_of_indented_stmt_list "\n"  string_of_line blk.lines)

(*
  Algorithm 8.7: Determining the liveness and next-use
  information for each statement in a basic block

  Input: A basic block B of three-address statements.
  We assume that the symbol table initially shows all
  nontemporary variables in B as being live on exit.

  Output: At each statement i: x = y + z in B, we attach to
  i the liveness and next-use information of x, y and z

  Method:
  1. We start at the last statement in B and scan backwards to the beginning of B.
     At each statement i: x = y + z in B, we do the following:
    - Attach to statement i the information currently found
      in the symbol table regarding the next use and liveness of x, y and z
    - In the symbol table, set x to "not live" and "no next use"
    - In the symbol table, set y and z to "live" and the next uses of y and z to i
 *)

let find_used_in_idc3 (var_id: idc3): id3_set =
	match var_id with
	| Var3 v -> Id3Set.singleton v
  | IntLiteral3 _ | BoolLiteral3 _ | StringLiteral3 _ -> Id3Set.empty

let rec find_used_in_idc3_list (var_id_list: idc3 list): id3_set =
	match var_id_list with
	| head::tail ->
		Id3Set.union (find_used_in_idc3 head) (find_used_in_idc3_list tail)
	| [] ->
		Id3Set.empty

(* returns the variables used in an ir3 expression *)
let rec find_used_in_expr (expr:ir3_exp): id3_set =
	match expr with
	| BinaryExp3 (_, a, b) ->
		Id3Set.union (find_used_in_idc3 a) (find_used_in_idc3 b)
	| UnaryExp3 (_, a) ->
		(find_used_in_idc3 a)
	| FieldAccess3 (obj, _) ->
		Id3Set.singleton obj
	| Idc3Expr a ->
		(find_used_in_idc3 a)
	| MdCall3 (_, var_list) ->
		(find_used_in_idc3_list var_list)
	| ObjectCreate3 _ ->
		Id3Set.empty

(* returns the list of the variables used in a statement *)
let find_used_vars (stmt:ir3_stmt): id3_set =
	match stmt with
	| IfStmt3 (expr, _) ->
		find_used_in_expr expr
	| PrintStmt3 var_id ->
		find_used_in_idc3 var_id
	| AssignStmt3 (_, expr) ->
    find_used_in_expr expr
	| AssignFieldStmt3 (expr1, expr2) ->
		Id3Set.union (find_used_in_expr expr1) (find_used_in_expr expr2)
	| MdCallStmt3 expr ->
		find_used_in_expr expr
	| ReturnStmt3 var ->
		Id3Set.singleton var
  | ReturnVoidStmt3 | Label3 _ |
    GoTo3 _ | ReadStmt3 _  ->
		Id3Set.empty

let find_not_used (stmt:ir3_stmt): id3_set =
  match stmt with
  | AssignStmt3 (id, _) -> Id3Set.singleton id
  | AssignFieldStmt3 (expr, _) -> (find_used_in_expr expr)
  | _ -> Id3Set.empty

let rec get_liveness_of_basic_block (blk: block) =
  let rec helper (lines: line list) =
    match lines with
      | [] -> []
      | line :: [] ->  (* Final stmt live out is empty *)
      let next_use = (find_used_vars line.stmt) in
        { line with
          next_use;
          live = next_use;
        }::[]
      | line :: tail ->
        let computed = helper tail in
        let head = List.hd computed in
        let next_use = (find_used_vars line.stmt) in
        (* e.g. x = x + 1, we remove x from not_live *)
        let not_used = (find_not_used line.stmt) in
        let diffed = (Id3Set.diff head.live not_used) in
        let live = Id3Set.union diffed next_use in
        { line with
          next_use;
          live;
        }::computed
  in
  { blk with lines = helper blk.lines }

let get_liveness_of_basic_blocks (blks: (block list)) =
  List.map get_liveness_of_basic_block blks
