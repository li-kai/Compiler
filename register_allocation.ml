open Ir3_structs
open Basic_blocks
open Graph

type register = string
type interval = (int * int)

(* Registers available *)
let no_registers = 5
let registers = ["v1"; "v2"; "v3"; "v4"; "v5"]

type result = {
  id: id3;
  reg: register;
  intv: interval;
}

type live_interval = {
  id: id3;
  intv: interval;
}

(* Active map are sorted in order of increasing end time *)
module Intervals =
struct
  type t = interval
  let compare (x0, y0) (x1, y1) =
    match Pervasives.compare y0 y1 with
    0 -> Pervasives.compare x0 x1
    | c -> c
  end
module Intv_to_id = Map.Make(Intervals)

type lsra_t = {
  mutable active: id3 Intv_to_id.t;
  mutable free_reg: register list;
  result_tbl: (id3, register) Hashtbl.t;
  intervals: (id3 * interval) list;
}

(*
  Algorithm Linear Scan: Global register allocation

  Input: A mapping of id3 to liveness interval,
  sorted by increasing starting time.

  Output: A mapping of id3 to a register, and whether
  spilling is needed in order to use the register.

  Method:
  1. We start at the earlier interval and repeat.
     At each point we retire any previous intervals that
     have ended, and indicated that register as free.
     - If spilling has occurred, then, we pick the interval
       currently used register that ends last
     - If not, we simply indicate the variable is using
       the next available free register
 *)
let linear_scan (intvs: live_interval list) =
  let sort_interval a b =
    let (x, _) = a.intv in
    let (y, _) = b.intv in
    Pervasives.compare x y
  in
  let sorted_intvs = List.sort sort_interval intvs in
  let lsra: lsra_t = {
    active = Intv_to_id.empty;
    result_tbl = Hashtbl.create no_registers;
    free_reg = registers;
    intervals = [];
  } in
  (*
    ExpireOldIntervals(i)
      foreach interval j in active, in order of increasing end point
      if endpoint[j] ≥ startpoint[i] then
      return
      remove j from active
      add register[j] to pool of free registers
  *)
  let expire_old_intervals (intv: live_interval) =
    let (start_i, end_i) = intv.intv in
    let helper (key: interval) (id: id3) =
      let (start_j, end_j) = key in
      if end_j < start_i then
        let id = Intv_to_id.find key lsra.active in
        let free_reg = Hashtbl.find lsra.result_tbl id in
        lsra.active <- Intv_to_id.remove key lsra.active;
        lsra.free_reg <- free_reg::lsra.free_reg;
    in Intv_to_id.iter helper lsra.active
  in
  let spill_at_interval (intv: live_interval) =
    (* Find the last register that is going to be freed  *)
    let find_max z =
      let id = Intv_to_id.find z lsra.active in
      id != "spill"
    in
    let (spill_intv, spill_id) = Intv_to_id.find_first find_max lsra.active in
    let (start_spill, end_spill) = spill_intv in
    let (start_replace, end_replace) = intv.intv in
    if end_spill > end_replace then
    let reg = Hashtbl.find lsra.result_tbl spill_id in
      let _ = List.find (fun r -> r = reg) registers in
      Hashtbl.add lsra.result_tbl intv.id reg;
      Hashtbl.replace lsra.result_tbl spill_id "spill";
    else
      (* Keep on the stack *)
      Hashtbl.add lsra.result_tbl intv.id "spill"
  in
  let def_reg (intv: live_interval) =
    let _ = expire_old_intervals intv in
    if Intv_to_id.cardinal lsra.active = no_registers then
      spill_at_interval intv
    else
      let to_be_used = List.hd lsra.free_reg in
      (* Indicate that interval is being served by a register *)
      lsra.active <- Intv_to_id.add intv.intv intv.id lsra.active;
      lsra.free_reg <- List.tl lsra.free_reg;
      let _ = List.find (fun r -> r = to_be_used) registers in
      Hashtbl.add lsra.result_tbl intv.id to_be_used;
  in let _ = List.iter def_reg sorted_intvs in
  (* let accum_results (k: id3) (v: register) (acc: result list) =
    let (intv_id, intv) = List.find (fun (id, i) -> id = k) lsra.intervals in
    let res = { id = k; reg = v; intv; } in
    res::acc
  in *)
  lsra.result_tbl

module type Data_flow_analysis = sig
  type new_line
  type new_basic_block
  val string_of_new_basic_blocks : new_basic_block list -> string
  val string_of_new_basic_block : new_basic_block -> string
  val string_of_new_line : new_line -> string
  val obtain_new_basic_blocks : Basic_blocks.block_collection -> new_basic_block list
end

module type Data_flow_computation = sig
  type t
  val string_of_t : t -> string
  val boundary : t
  val transfer : t -> Basic_blocks.block -> t list
  val meet : t list -> t
  val init : t
end

module MkForwardDataFlowAnalysis (Dfc: Data_flow_computation) = struct
  type t = Dfc.t

  type new_line =
    {
      no: int;
      stmt: Ir3_structs.ir3_stmt;
      payload: t
    }

  type new_basic_block =
    {
      id: Basic_blocks.block_id;
      lines: new_line list;
      payload_in: t;
      payload_out: t
    }

  let string_of_new_line (line: new_line) =
    let stmt_str = Ir3_structs.string_of_ir3_stmt line.stmt in
    let live_str = Dfc.string_of_t line.payload in
    "\n\tStmt: " ^ stmt_str ^ "\n\tLive: " ^ live_str

  let string_of_new_basic_block (blk: new_basic_block) =
    "\nId " ^ blk.id ^ " lines: " ^ (Jlite_structs.string_of_indented_stmt_list "\n" string_of_new_line blk.lines)

  let string_of_new_basic_blocks (blk: new_basic_block list) =
    String.concat "\n\n" (List.map string_of_new_basic_block blk)

  let new_default_lines old_lines =
    List.map (fun line -> {no = line.Basic_blocks.no; stmt = line.Basic_blocks.stmt; payload = Dfc.boundary}) old_lines

  let generate_new_block old_block new_payloads new_in =
    let generate_new_lines old_lines payloads =
      let rec aux lst lst2 =
        match lst, lst2 with
        | hd1::tl1, hd2::tl2 -> { no = hd1.no; stmt = hd1.stmt; payload = hd2 } :: aux tl1 tl2
        | [], [] -> []
        | _, _ -> failwith "They should have the same length"
      in
      aux old_lines payloads
    in
    if List.length new_payloads = 0 then old_block
    else { old_block with payload_in = new_in; payload_out = List.hd @@ List.rev new_payloads; lines = generate_new_lines old_block.lines new_payloads }

  let new_default_block old_block =
    {
      id = old_block.Basic_blocks.id;
      lines = new_default_lines old_block.Basic_blocks.lines;
      payload_in = Dfc.boundary;
      payload_out = Dfc.boundary;
    }

  let old_lines_from_new_lines (new_lines: new_line list) : Basic_blocks.line list =
    List.map (fun v -> { Basic_blocks.stmt = v.stmt; Basic_blocks.no = v.no }) new_lines

  let old_basic_block_from_new_basic_block (new_block: new_basic_block) : Basic_blocks.block =
    {
      Basic_blocks.id = new_block.id;
      Basic_blocks.lines = old_lines_from_new_lines new_block.lines
    }

  let initialize_graph (edges: (string, Basic_blocks.block_id list) Hashtbl.t) (block_map: (string, int) Hashtbl.t) : int Graph.adj_list * int Graph.adj_list =
    let g : int Graph.adj_list = Hashtbl.create @@ Hashtbl.length block_map in
    let grev : int Graph.adj_list = Hashtbl.create @@ Hashtbl.length block_map in
    (* Create empty adjacency list *)
    let () = Hashtbl.iter (fun k _ -> Hashtbl.add g (Hashtbl.find block_map k) []) block_map in
    let () = Hashtbl.iter (fun k _ -> Hashtbl.add grev (Hashtbl.find block_map k) []) block_map in
    let process_edge_entry k v =
      let from_vertex = Hashtbl.find block_map k in
      List.iter
        (fun to_block_id ->
           let to_vertex = Hashtbl.find block_map to_block_id in
           Hashtbl.replace g from_vertex (to_vertex :: (Hashtbl.find g from_vertex));
           Hashtbl.replace grev to_vertex (from_vertex :: (Hashtbl.find grev to_vertex)))
        v
    in
    let () = Hashtbl.iter process_edge_entry edges in
    g, grev

  let initialize new_blocks_arr block_id_mapping =
    let start_id = Hashtbl.find block_id_mapping "start" in
    new_blocks_arr.(start_id) <- { new_blocks_arr.(start_id) with payload_out = Dfc.init }

  (* Inefficient ..... *)
  let fixed_point_iteration (old_blocks_arr: Basic_blocks.block array) (new_blocks_arr: new_basic_block array) block_id_mapping g grev =
    let start_id = Hashtbl.find block_id_mapping "start" in
    let preorder = Graph.get_preorder g start_id in
    let change = ref true in
    while !change do
      change := false;
      let process_block new_t block_idx =
        if List.length old_blocks_arr.(block_idx).Basic_blocks.lines > 0 then
          let new_payloads = Dfc.transfer new_t old_blocks_arr.(block_idx) in
          let new_basic_block = generate_new_block new_blocks_arr.(block_idx) new_payloads new_t in
          (* Check whether OUT[block_idx] changes *)
          if new_basic_block <> new_blocks_arr.(block_idx) then
            begin
              change := true;
              new_blocks_arr.(block_idx) <- new_basic_block
            end;
      in
      List.iter
        (fun block_idx ->
           let new_in = Dfc.meet (List.map (fun i -> (new_blocks_arr.(i)).payload_out) (Hashtbl.find grev block_idx)) in
           process_block new_in block_idx)
        preorder;
    done;
    ()

  let obtain_new_basic_blocks block_collection : new_basic_block list =
    let block_lst = block_collection.Basic_blocks.blocks in
    let edges = block_collection.Basic_blocks.edges_out in
    let old_blocks_arr = Array.of_list block_lst in
    let new_blocks_arr = Array.of_list @@ List.map (fun block -> new_default_block block) block_lst in
    let block_id_mapping = Hashtbl.create @@ List.length block_lst in
    let () = Array.iteri (fun idx block -> Hashtbl.add block_id_mapping block.id idx) new_blocks_arr in
    let g, grev = initialize_graph edges block_id_mapping in
    initialize new_blocks_arr block_id_mapping;
    fixed_point_iteration old_blocks_arr new_blocks_arr block_id_mapping g grev;
    Array.to_list new_blocks_arr
end

module MkBackwardDataFlowAnalysis (Dfc: Data_flow_computation) = struct

  type new_line =
    {
      no: int;
      stmt: Ir3_structs.ir3_stmt;
      payload: Dfc.t
    }

  type new_basic_block =
    {
      id: Basic_blocks.block_id;
      lines: new_line list;
      payload_in: Dfc.t;
      payload_out: Dfc.t
    }

  let string_of_new_line (line: new_line) =
    let stmt_str = Ir3_structs.string_of_ir3_stmt line.stmt in
    let live_str = Dfc.string_of_t line.payload in
    "\n\tStmt: " ^ stmt_str ^ "\n\tLive: " ^ live_str

  let string_of_new_basic_block (blk: new_basic_block) =
    "\nId " ^ blk.id ^ " lines: " ^ (Jlite_structs.string_of_indented_stmt_list "\n" string_of_new_line blk.lines)

  let string_of_new_basic_blocks (blk: new_basic_block list) =
    String.concat "\n\n" (List.map string_of_new_basic_block blk)

  let new_default_lines old_lines =
    List.map (fun line -> {no = line.Basic_blocks.no; stmt = line.Basic_blocks.stmt; payload = Dfc.boundary}) old_lines

  (* DIFF! *)
  let generate_new_block old_block new_payloads new_out =
    let generate_new_lines old_lines payloads =
      let rec aux lst lst2 =
        match lst, lst2 with
        | hd1::tl1, hd2::tl2 -> { no = hd1.no; stmt = hd1.stmt; payload = hd2 } :: aux tl1 tl2
        | [], [] -> []
        | _, _ -> failwith "They should have the same length"
      in
      aux old_lines payloads
    in
    if List.length new_payloads = 0 then old_block
    else { old_block with payload_in = List.hd new_payloads; payload_out = new_out; lines = generate_new_lines old_block.lines new_payloads }

  let new_default_block old_block =
    {
      id = old_block.Basic_blocks.id;
      lines = new_default_lines old_block.Basic_blocks.lines;
      payload_in = Dfc.boundary;
      payload_out = Dfc.boundary;
    }

  let old_lines_from_new_lines (new_lines: new_line list) : Basic_blocks.line list =
    List.map (fun v -> { Basic_blocks.no = v.no; Basic_blocks.stmt = v.stmt }) new_lines

  let old_basic_block_from_new_basic_block (new_block: new_basic_block) : Basic_blocks.block =
    {
      Basic_blocks.id = new_block.id;
      Basic_blocks.lines = old_lines_from_new_lines new_block.lines
    }

  let initialize_graph (edges: (string, Basic_blocks.block_id list) Hashtbl.t) (block_map: (string, int) Hashtbl.t) : int Graph.adj_list * int Graph.adj_list =
    let g : int Graph.adj_list = Hashtbl.create @@ Hashtbl.length block_map in
    let grev : int Graph.adj_list = Hashtbl.create @@ Hashtbl.length block_map in
    (* Create empty adjacency list *)
    let () = Hashtbl.iter (fun k _ -> Hashtbl.add g (Hashtbl.find block_map k) []) block_map in
    let () = Hashtbl.iter (fun k _ -> Hashtbl.add grev (Hashtbl.find block_map k) []) block_map in
    (* Hashtbl.iter (fun k v -> print_endline @@ "Got " ^ k ^ " -> " ^ (string_of_int v)) block_map; *)
    let process_edge_entry k v =
      let from_vertex = Hashtbl.find block_map k in
      List.iter
        (fun to_block_id ->
           let to_vertex = Hashtbl.find block_map to_block_id in
           Hashtbl.replace g from_vertex (to_vertex :: (Hashtbl.find g from_vertex));
           Hashtbl.replace grev to_vertex (from_vertex :: (Hashtbl.find grev to_vertex)))
        v
    in
    let () = Hashtbl.iter process_edge_entry edges in
    (* DIFF!!! *)
    grev, g

  let initialize new_blocks_arr block_id_mapping =
    let start_id = Hashtbl.find block_id_mapping "start" in
    new_blocks_arr.(start_id) <- { new_blocks_arr.(start_id) with payload_in = Dfc.init }

  (* Inefficient ..... *)
  (* DIFF!!! *)
  let fixed_point_iteration (old_blocks_arr: Basic_blocks.block array) (new_blocks_arr: new_basic_block array) block_id_mapping g grev =
    let start_id = Hashtbl.find block_id_mapping "exit" in
    let rev_preorder = List.rev @@ Graph.get_preorder g start_id in
    let change = ref true in
    while !change do
      change := false;
      let process_block new_t block_idx =
        if List.length old_blocks_arr.(block_idx).Basic_blocks.lines > 0 then
          let new_payloads = Dfc.transfer new_t old_blocks_arr.(block_idx) in
          let new_basic_block = generate_new_block new_blocks_arr.(block_idx) new_payloads new_t in
          (* Check whether IN[block_idx] changes *)
          if new_basic_block <> new_blocks_arr.(block_idx) then
            begin
              change := true;
              new_blocks_arr.(block_idx) <- new_basic_block
            end;
      in
      List.iter
        (fun block_idx ->
           let new_in = Dfc.meet (List.map (fun i -> (new_blocks_arr.(i)).payload_in) (Hashtbl.find grev block_idx)) in
           process_block new_in block_idx)
        rev_preorder;
    done;
    ()

  let obtain_new_basic_blocks block_collection : new_basic_block list =
    let block_lst = block_collection.Basic_blocks.blocks in
    (* List.iter (fun x -> print_endline @@ "Has node " ^ x.Basic_blocks.id) block_lst; *)
    let edges = block_collection.Basic_blocks.edges_out in
    let old_blocks_arr = Array.of_list block_lst in
    let new_blocks_arr = Array.of_list @@ List.map (fun block -> new_default_block block) block_lst in
    let block_id_mapping = Hashtbl.create @@ List.length block_lst in
    let () = Array.iteri (fun idx block -> Hashtbl.add block_id_mapping block.id idx) new_blocks_arr in
    let g, grev = initialize_graph edges block_id_mapping in
    initialize new_blocks_arr block_id_mapping;
    fixed_point_iteration old_blocks_arr new_blocks_arr block_id_mapping g grev;
    Array.to_list new_blocks_arr
end

(*
  Algorithm 9.14 Live-variable analysis

  Input: A flow graph with def and use computed for each block

  Ouput: IN[B] and OUT[B], the set of variables live on entry and
  exit of each block B of the flow graph

  Method:
    IN[EXIT] = null set
    for (each basic block B other than EXIT) IN[B] = null set;
    while (changes to any IN occur)
      for (each basic block B other than EXIT) {
        OUT[B] = U_S a successor of B IN[S];
        IN[B] = use_B union (OUT[B] - def_B);
      }
 *)

module Liveness_analysis_computation = struct

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

  let find_used_in_idc3 (var_id: Ir3_structs.idc3): id3_set =
    match var_id with
    | Ir3_structs.Var3 v -> StringSet.singleton v
    | Ir3_structs.IntLiteral3 _ | Ir3_structs.BoolLiteral3 _ | Ir3_structs.StringLiteral3 _ -> StringSet.empty

  let rec find_used_in_idc3_list (var_id_list: Ir3_structs.idc3 list): id3_set =
    match var_id_list with
    | head::tail ->
      StringSet.union (find_used_in_idc3 head) (find_used_in_idc3_list tail)
    | [] ->
      StringSet.empty

  (* returns the variables used in an ir3 expression *)
  let rec find_used_in_expr (expr:Ir3_structs.ir3_exp): id3_set =
    match expr with
    | Ir3_structs.BinaryExp3 (_, a, b) ->
      StringSet.union (find_used_in_idc3 a) (find_used_in_idc3 b)
    | Ir3_structs.UnaryExp3 (_, a) ->
      (find_used_in_idc3 a)
    | Ir3_structs.FieldAccess3 (obj, _) ->
      StringSet.singleton obj
    | Ir3_structs.Idc3Expr a ->
      (find_used_in_idc3 a)
    | Ir3_structs.MdCall3 (_, var_list) ->
      (find_used_in_idc3_list var_list)
    | Ir3_structs.ObjectCreate3 _ ->
      StringSet.empty

  (* returns the list of the variables used in a statement *)
  let find_used_vars (stmt:Ir3_structs.ir3_stmt): id3_set =
    match stmt with
    | Ir3_structs.IfStmt3 (expr, _) ->
      find_used_in_expr expr
    | Ir3_structs.PrintStmt3 var_id ->
      find_used_in_idc3 var_id
    | Ir3_structs.AssignStmt3 (_, expr) ->
      find_used_in_expr expr
    | Ir3_structs.AssignFieldStmt3 (expr1, expr2) ->
      StringSet.union (find_used_in_expr expr1) (find_used_in_expr expr2)
    | Ir3_structs.MdCallStmt3 expr ->
      find_used_in_expr expr
    | Ir3_structs.ReturnStmt3 var ->
      StringSet.singleton var
    | Ir3_structs.ReturnVoidStmt3 | Ir3_structs.Label3 _ |
      Ir3_structs.GoTo3 _ | Ir3_structs.ReadStmt3 _  ->
      StringSet.empty

  let find_not_used (stmt:Ir3_structs.ir3_stmt): id3_set =
    match stmt with
    | Ir3_structs.AssignStmt3 (id, _) -> StringSet.singleton id
    | Ir3_structs.AssignFieldStmt3 (expr, _) -> (find_used_in_expr expr)
    | _ -> StringSet.empty

  let rec get_liveness_of_basic_block (blk: Basic_blocks.block) (init: id3_set) =
    let rec helper (lines: Basic_blocks.line list) =
      match lines with
        | [] -> []
        | line :: [] ->  (* Final stmt live out is empty *)
          let next_use = (find_used_vars line.Basic_blocks.stmt) in
          let not_used = (find_not_used line.Basic_blocks.stmt) in
          let diffed = (StringSet.diff init not_used) in
          let live = StringSet.union diffed next_use in
          live::[]
        | line :: tail ->
          let computed = helper tail in
          let head = List.hd computed in
          let next_use = (find_used_vars line.Basic_blocks.stmt) in
          (* e.g. x = x + 1, we remove x from not_live *)
          let not_used = (find_not_used line.Basic_blocks.stmt) in
          let diffed = (StringSet.diff head not_used) in
          let live = StringSet.union diffed next_use in
          live::computed
    in
    helper blk.Basic_blocks.lines


  type t = id3_set

  let string_of_t (set: t) =
    String.concat ", " (StringSet.elements set)

  let boundary = StringSet.empty
  let init = boundary
  let transfer init block = get_liveness_of_basic_block block init
  let meet lst =
    if List.length lst = 0 then boundary
    else
      let hd, tl = match lst with
        | hd::tl -> hd, tl
        | [] -> failwith "Impossible"
      in
      List.fold_left (fun a b -> StringSet.union a b) hd tl
end

module Liveness_analysis = MkBackwardDataFlowAnalysis (Liveness_analysis_computation)
open Liveness_analysis

(* traverse the list of blocks and keep track of the min live value and mindead value for each id *)
let live_interval_from_blocks (blocks: new_basic_block list) adj_list: live_interval list =
  let visited_blocks = Hashtbl.create 10 in
  let liveness_tbl = Hashtbl.create 10 in

  let update_id_liveness line_no id =
    if Hashtbl.mem liveness_tbl id
    then
      let min_live, max_live = Hashtbl.find liveness_tbl id in
      Hashtbl.replace liveness_tbl id (min min_live line_no, max max_live line_no)
    else Hashtbl.add liveness_tbl id (line_no, line_no)
  in

  let process_line (line: new_line) =
    Basic_blocks.StringSet.iter (update_id_liveness line.no) line.payload
  in

  let rec preorder_traverse block_id =
    if not @@ Hashtbl.mem visited_blocks block_id
    then
      let _ = Hashtbl.add visited_blocks block_id true in
      (* print_string ("Finding block " ^ block_id ^ "\n"); *)
      let block = List.find (fun (x: new_basic_block) -> x.id = block_id) blocks in
      List.iter process_line block.lines;

      let next_blocks = Hashtbl.find adj_list block_id in
      List.iter preorder_traverse next_blocks
  in

  let _ = preorder_traverse "start" in
  Hashtbl.fold (fun k (v1, v2) acc -> {id=k; intv=(v1, v2)} :: acc) liveness_tbl []
