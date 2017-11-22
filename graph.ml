type 'a adj_list = ('a, 'a list) Hashtbl.t

(* Returns preorder traversal of the graph *)
let get_preorder (g: 'a adj_list) ~entry : 'a list =
  let n = Hashtbl.length g in
  let visited = Hashtbl.create n in
  let rev_preorder = ref [] in
  let rec dfs_rec u =
    let () = rev_preorder := u :: !rev_preorder in
    let () = Hashtbl.replace visited u true in
    List.iter (fun v -> if (not (Hashtbl.mem visited v)) then dfs_rec v) (Hashtbl.find g u);
  in
  dfs_rec entry;
  Hashtbl.iter (fun k _ -> if not(Hashtbl.mem visited k) then dfs_rec k) g;
  List.rev !rev_preorder
