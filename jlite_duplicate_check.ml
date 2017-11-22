module R = Jlite_report
module S = Jlite_structs

(* Helper data type to make processing methods easier *)
type lite_md_decl =
  {
    class_name: string;  (* TODO(raisfathin): Is this really needed? *)
    jliteid: S.var_id;
    rettype: S.jlite_type;
    params: (S.var_decl list);
  }

let lite_md_decl_of_md_decl (class_name:S.class_name) (md_decl:S.md_decl) : lite_md_decl =
  {
    class_name = class_name;
    jliteid = md_decl.S.jliteid;
    rettype = md_decl.S.rettype;
    params = md_decl.S.params;
  }

let string_of_lite_md_decl (lite_md:lite_md_decl) =
  (S.string_of_jlite_type lite_md.rettype)
  ^ " "
  ^ (S.string_of_var_id lite_md.jliteid)
  ^ " "
  ^ "("
  ^ (S.string_of_list lite_md.params S.string_of_arg_decl ",")
  ^ ")"

let ensure_unique_defs_in_class (class_name, var_decls, md_decls) =
  let var_names = List.map (fun (typ, varid) -> S.string_of_var_id varid) var_decls in
  let md_decls = List.map (fun (x:S.md_decl) -> S.string_of_var_id x.S.jliteid) md_decls in
  let count_occ_in_lst lst name = List.fold_left (fun acc v -> if v = name then acc + 1 else acc) 0 lst in
  let clashing_names = List.filter (fun v -> (count_occ_in_lst md_decls v) > 0) var_names in
  if List.length clashing_names > 0 then
    R.report_error @@ "Identifier " ^ (List.hd clashing_names) ^ " is used as both variable and method identifier in class " ^ class_name ^ ". Please fix."

let ensure_unique_defs_in_method (class_name, var_decls, md_decl) =
  let localvars_names = List.map (fun (typ, varid) -> S.string_of_var_id varid) md_decl.S.localvars in
  let count_occ_in_lst lst name = List.fold_left (fun acc v -> if v = name then acc + 1 else acc) 0 lst in
  let clashing_names = List.filter (fun v -> (count_occ_in_lst localvars_names v) > 1) localvars_names in
  if List.length clashing_names > 0 then
    R.report_error @@ "Identifier " ^ (List.hd clashing_names) ^ " is used multiple times in the local variable declarations of method named " ^ (S.string_of_var_id md_decl.S.jliteid) ^ " in class named " ^ class_name

let ensure_unique_defs_in_methods (class_name, var_decls, md_decls) =
  List.iter (fun v -> ensure_unique_defs_in_method (class_name, var_decls, v)) md_decls

let find_duplicate_classes prog =
  let (main_class, aux_classes) = prog in
  let (main_class_name, _) = main_class in
  let aux_classes_names = List.map (fun (name, _, _) -> name) aux_classes in
  let class_names = main_class_name :: aux_classes_names in
  let rec aux lst acc =
    match lst with
    | cname :: tl -> if List.mem cname tl then aux tl (cname::acc)
                     else aux tl acc
    | [] -> acc
  in
  let duplicates = List.sort_uniq compare (aux class_names []) in
  if List.length duplicates > 0 then
    R.report_error @@ "Found the following duplicate classes: " ^ (String.concat " " duplicates)
  else
    ()

let find_duplicate_methods (class_name, var_decls, (md_decls: S.md_decl list)) =
  let lite_md_decls = List.map (lite_md_decl_of_md_decl class_name) md_decls in
  let duplicates =
    List.filter
      (fun md_decl ->
        let param_type_list = List.map (fun (typ, id) -> typ) md_decl.params in
        let md_name = S.string_of_var_id md_decl.jliteid in
        let has_same_params_and_name md_decl2 =
          let param_type_list2 = List.map (fun (typ, id) -> typ) md_decl2.params in
          let md_name2 = S.string_of_var_id md_decl2.jliteid in
          param_type_list = param_type_list2 && md_name = md_name2
        in
        let occurence =
          List.fold_left
            (fun acc md_decl2 -> if has_same_params_and_name md_decl2 then acc + 1 else acc)
            0 lite_md_decls
        in
        occurence > 1)
      lite_md_decls
    |> List.sort_uniq compare
  in
  let report_duplicate_method lite_md_decl =
    let class_name = lite_md_decl.class_name in
    let method_name = lite_md_decl.jliteid in
    let params_types_str = List.map (fun (typ, id) -> S.string_of_jlite_type typ) lite_md_decl.params in
    let rettype_str = S.string_of_jlite_type lite_md_decl.rettype in
    R.report_error @@ "Found multiple return types for \"" ^ (S.string_of_var_id method_name) ^ "\" with parameter types \"" ^ (String.concat "," params_types_str) ^ "\" in class \"" ^ class_name ^ "\" (this returns \"" ^ rettype_str ^ "\")"
  in
  if List.length duplicates > 0 then
    report_duplicate_method @@ List.hd duplicates
  else
    ()

let find_duplicate_class_vars (class_name, var_decls, md_decls) =
  let duplicates =
    List.filter
      (fun (typ, varid) ->
        let occurence =
          List.fold_left
            (fun acc (_, varid2) -> if varid = varid2 then acc + 1 else acc)
            0 var_decls
        in
        occurence > 1)
    var_decls
    |> List.map (fun (typ, varid) -> varid)
  in
  if List.length duplicates > 0 then
    R.report_error @@ "Found duplicate variables called \"" ^ (S.string_of_var_id @@ List.hd duplicates) ^ "\" in class \"" ^ class_name
  else
    ()

let find_duplicate_method_args_ids (class_name, var_decls, md_decls) =
  let lite_md_decls = List.map (lite_md_decl_of_md_decl class_name) md_decls in
  let has_duplicate_param_ids lite_md_decl =
    let param_ids = List.map (fun (typ, id) -> id) lite_md_decl.params in
    let uniq_len = List.length @@ List.sort_uniq compare param_ids in
    let original_len = List.length param_ids in
    uniq_len <> original_len
  in
  let invalid_lite_md_decls = List.filter has_duplicate_param_ids lite_md_decls in
  if List.length invalid_lite_md_decls > 0 then
    R.report_error @@ "Found a method with non-unique parameter names " ^ (string_of_lite_md_decl @@ List.hd invalid_lite_md_decls)
  else
    ()
  
