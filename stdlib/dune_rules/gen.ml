let get_target modname =
  let base = String.uncapitalize_ascii modname in
  if base = "stdlib" || base = "std_exit" || String.starts_with base ~prefix:"camlinternal"
  then base
  else "stdlib__" ^ String.capitalize_ascii base

let rename_file target =
  get_target (Filename.remove_extension target) ^ Filename.extension target

let rule ~ppf ~target ~exts ~src ~deps ~action =
  Printf.fprintf ppf
    "(rule\n  (targets %s)\n  (deps (:src %s) %s)\n  (action (run %s)))\n\n"
    (String.concat " " (List.map (fun e -> target ^ "." ^ e) exts))
    src (String.concat " " deps)
    action

let copy_ml_files ~ppf files =
  Printf.fprintf ppf
    "(copy_files# %s)\n"
    files

let copy_bin_files ~ppf files =
  Printf.fprintf ppf
    "(copy_files %s)\n" files

let sprintf = Printf.sprintf

module StrTbl = Hashtbl.Make (String)
module StrSet = Set.Make (String)

let process_line ~table s =
  let tgt_file, deps =
    match String.split_on_char ':' s with
    | [file; deps] ->
       let file = String.trim file and deps = String.trim deps in
       file,
       (deps
        |> String.trim
        |> function "" -> [] | s -> String.split_on_char ' ' s)
    | _ -> failwith ("Cannot parse " ^ s)
  in
  let deps = List.map rename_file deps in
  let basename = Filename.remove_extension tgt_file in
  StrTbl.add table (rename_file tgt_file) (basename, StrSet.of_list deps)

let rec trans_closure ~dst ~src s =
  match StrTbl.find dst s with
  | deps -> deps
  | exception Not_found ->
     let (_base, imm_deps) =
       try StrTbl.find src s with Not_found -> failwith s in
     let deps = StrSet.fold (fun dep acc -> StrSet.union acc (trans_closure ~dst ~src dep)) imm_deps imm_deps in
     StrTbl.add dst s deps;
     deps

let gen_rule ~ppf ~tgt_file ~base ~deps =
  let ext, annot =
    (* FIXME: cmo or cmx for annot? Need to pick one *)
    match Filename.extension tgt_file with
    | ".cmo" -> `Cmo, true
    | ".cmi" -> `Cmi, true
    | ".cmx" -> `Cmx, false
    | s -> failwith ("Unexpected extension " ^ s)
  in
  let flags = "-nopervasives -directory stdlib -strict-sequence -g -absname -extension runtime_metaprogramming -nostdlib -safe-string -strict-formats -no-alias-deps -w +a-4-9-40-41-42-44-45-48-66-67-70 -w -221" in
  let flags = if annot then flags ^ " -bin-annot -bin-annot-occurrences -bin-annot-cms" else flags in
  let src =
    match ext with
    | `Cmi -> base ^ ".mli"
    | `Cmx | `Cmo -> base ^ ".ml"
  in
  let target = get_target base in
  let flags =
    if target = "stdlib"
    then flags ^ " -pp \"awk -f %{dep:../expand_module_aliases.awk}\""
    else flags
  in
  if ext <> `Cmo then copy_ml_files ~ppf (sprintf "../%s" src);
  match ext with
  | `Cmi ->
     let action =
       Printf.sprintf "%%{exe:../../main_native.exe} %s -o %s.cmi -c %%{src}"
         flags target
     in
     rule ~ppf ~target ~exts:["cmi";"cmsi";"cmti"] ~src ~deps ~action
  | `Cmx ->
     let action =
       Printf.sprintf "%%{exe:../../boot_ocamlopt.exe} %s -cmi-file %s.cmi -o %s.cmx -c %%{src}"
         flags target target
     in
     rule ~ppf ~target ~exts:["o";"cmx"] ~src ~deps ~action
  | `Cmo ->
     let action =
       Printf.sprintf "%%{exe:../../main_native.exe} %s -cmi-file %s.cmi -o %s.cmo -c %%{src}"
         flags target target
     in
     rule ~ppf ~target ~exts:["cmo";"cms";"cmt"] ~src ~deps ~action

let () =
  let modnames =
    let ch = open_in Sys.argv.(1) in
    let mods = input_line ch |> String.split_on_char ' ' in
    close_in ch;
    mods @ ["std_exit"]
  in
  let ppf = open_out "stdlib.dune" in
  let table = StrTbl.create 10 in
  let ch = open_in Sys.argv.(2) in
  while
    match input_line ch with
    | s -> process_line ~table s; true
    | exception End_of_file -> false
  do () done;
  close_in ch;
  let trans_table = StrTbl.create 10 in
  table |> StrTbl.iter (fun tgt_file (base, _imm_deps) ->
    let deps = trans_closure ~src:table ~dst:trans_table tgt_file |> StrSet.to_list in
    gen_rule ~ppf ~tgt_file ~base ~deps);
  rule ~ppf ~target:"stdlib" ~exts:["type"] ~src:"stdlib.mli"
    ~deps:(modnames |> List.map (fun m -> get_target m ^ ".cmi"))
    ~action:"touch stdlib.type";
  let action =
    let flags = "-g -nostdlib" in
    Printf.sprintf "%%{exe:../../boot_ocamlopt.exe} %s -a -o stdlib.cmxa %s"
      flags
      (modnames |> List.map (fun m -> Printf.sprintf "%%{dep:%s.cmx}" (get_target m)) |> String.concat " ")
  in
  rule ~ppf ~target:"stdlib" ~exts:["cmxa";"a"] ~src:"stdlib.ml"
    ~deps:(modnames |> List.map (fun m -> get_target m ^ ".o"))
    ~action;
  let action =
    let flags = "-g -nostdlib" in
    Printf.sprintf "%%{exe:../../main_native.exe} %s -a -o stdlib.cma %s"
      flags
      (modnames |> List.map (fun m -> Printf.sprintf "%%{dep:%s.cmo}" (get_target m)) |> String.concat " ")
  in
  rule ~ppf ~target:"stdlib" ~exts:["cma"] ~src:"stdlib.ml"
    ~deps:(modnames |> List.map (fun m -> get_target m ^ ".cmo"))
    ~action;
  copy_bin_files ~ppf "../../runtime/build/libasmrun.a"

let () =
  let ppf = open_out "runtime.dune" in
  copy_bin_files ~ppf "../../runtime/build/libasmrun.a"
