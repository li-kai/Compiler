open Ir3_structs
open Basic_blocks

type register = string
type interval = (int * int)

(* Registers available *)
let no_registers = 5
let registers = ["v0"; "v1"; "v2"; "v3"; "v4"]

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
        let free_reg = Intv_to_id.find key lsra.active in
        lsra.active <- Intv_to_id.remove key lsra.active;
        lsra.free_reg <- free_reg::lsra.free_reg;
    in Intv_to_id.iter helper lsra.active
  in
  let spill_at_interval (intv: live_interval) =
    (* Find the last register that is going to be freed  *)
    let (spill_intv, spill_id) = Intv_to_id.max_binding lsra.active in
    let (start_spill, end_spill) = spill_intv in
    let (start_replace, end_replace) = intv.intv in
    if end_spill > end_replace then
      let reg = Hashtbl.find lsra.result_tbl spill_id in
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
      Hashtbl.add lsra.result_tbl intv.id to_be_used;
  in let _ = List.iter def_reg sorted_intvs in
  (* let accum_results (k: id3) (v: register) (acc: result list) =
    let (intv_id, intv) = List.find (fun (id, i) -> id = k) lsra.intervals in
    let res = { id = k; reg = v; intv; } in
    res::acc
  in *)
  lsra.result_tbl

(* traverse the list of blocks and keep track of the min live value and mindead value for each id *)
let live_interval_from_blocks (bc: block_collection) : live_interval list =
  let blocks = bc.blocks in
  let visited_blocks = Hashtbl.create 10 in
  let liveness_tbl = Hashtbl.create 10 in

  let update_id_liveness line_no id =
    if Hashtbl.mem liveness_tbl id
    then
      let min_live, max_live = Hashtbl.find liveness_tbl id in
      Hashtbl.replace liveness_tbl id (min min_live line_no, max max_live line_no)
    else Hashtbl.add liveness_tbl id (line_no, line_no)
  in

  let process_line line =
    Basic_blocks.StringSet.iter (update_id_liveness line.no) line.live
  in

  let rec preorder_traverse block_id =
    if not @@ Hashtbl.mem visited_blocks block_id
    then
      let _ = Hashtbl.add visited_blocks block_id true in
      let block = List.find (fun (x:Basic_blocks.block) -> x.id == block_id) blocks in
      List.iter process_line block.lines;

      let next_blocks = Hashtbl.find bc.edges_out block_id in
      List.iter preorder_traverse next_blocks
    else ()
  in

  let _ = preorder_traverse "start" in
  Hashtbl.fold (fun k (v1, v2) acc -> {id=k; intv=(v1, v2)} :: acc) liveness_tbl []
