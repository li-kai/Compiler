let list_find_with_index lst x ~key =
  let rec aux lst idx =
    match lst with
    | hd::tl ->
       if key hd = x then Some idx
       else aux tl (idx+1)
    | [] -> None
  in
  aux lst 0

let offset_of_field (var_decl_list: Ir3_structs.var_decl3 list) (field_name: string) =
  match list_find_with_index var_decl_list field_name (fun (typ, id) -> id) with
  | Some idx -> idx * 4
  | None -> failwith "Invalid field access"

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

let offset_of_var (md_decl3: Ir3_structs.md_decl3) (var_name: string) : int =
  let var_idx = list_find_with_index (md_decl3.Ir3_structs.localvars3 @ md_decl3.Ir3_structs.params3) var_name (fun (typ, id) -> id) in
  match var_idx with
  | Some idx -> 4 * idx
  | _ -> failwith "Invalid local variable access"
