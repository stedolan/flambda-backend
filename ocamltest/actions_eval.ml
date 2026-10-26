open Ocamltest_stdlib

let promoted_output (files : Filecompare.files) =
  let ref_dir = Filename.concat (Filename.dirname files.reference_filename) "" in
  let name =
    match String.remove_prefix ~prefix:ref_dir files.output_filename with
    | Some s -> s
    | None -> files.output_filename
  in
  let name = String.concat "__" (String.split_on_char '/' name) in (* dune limitation *)
  Filename.concat ref_dir name

let run_task_item ~log : Actions.task_item -> Result.t = function
  | Chdir dir  ->
     Builtin_actions.do_chdir dir
  | Check_file_exists { filename; reason } ->
     Builtin_actions.do_check_file_exists ~filename ~reason
  | Check_file_absent { filename } ->
     Builtin_actions.do_check_file_absent ~filename
  | Copy_file { src; dst } ->
     Builtin_actions.do_copy_file log ~src ~dst
  | Setup_build_env { source_dir; build_dir; remove; files; subdirs } ->
     Actions_helpers.do_setup_build_env
       ~source_dir ~build_dir ~remove ~files ~subdirs
  | Run_cmd { settings; message; exits } ->
     Actions_helpers.do_run_cmd ~settings ~message ~exits log
  | Compare_files { kind_of_output; files; tool; reference_exists; promote } ->
     let is_empty file =
       let ch = open_in_bin file in
       Fun.protect ~finally:(fun () -> close_in_noerr ch)
         (fun () -> in_channel_length ch = 0)
     in
     if Options.no_diff then
       (if not (Sys.file_exists files.output_filename) then
         Result.fail_with_reason
           ("Failed to produce output " ^ files.output_filename)
        else if is_empty files.output_filename && not reference_exists then
          (* Special case: empty output and no reference file is OK *)
          Result.pass
        else
          Builtin_actions.do_copy_file log
            ~src:files.output_filename
            ~dst:(promoted_output files))
     else
       Actions_helpers.do_compare_files
         ~kind_of_output ~files ~tool ~promote log

let rec run_tasks ~log tasks =
  match tasks with
  | [] -> Result.pass
  | t :: ts ->
     match run_task_item ~log t with
     | r when ts = [] -> r
     | { status = Pass | Predicate true; _ } -> run_tasks ~log ts
     | { status = Skip | Fail | Predicate false; _ } as r -> r

let run_task ~log = function
  | Actions.Static r -> r
  | Actions.Task tasks -> run_tasks ~log tasks

let run log testenv actions =
  let total = List.length actions in
  let rec run_actions_aux action_number env = function
    | [] -> (Result.pass, env)
    | action::remaining_actions ->
      begin
        Printf.fprintf log "\nRunning action %d/%d (%s)\n%!"
          action_number total (Actions.name action);
        let (task, env') = Actions.run log env action in
        let result =
          match task with
          | Static r -> r
          | Task tasks -> run_tasks ~log tasks
        in
        Printf.fprintf log "Action %d/%d (%s) %s\n%!"
          action_number total (Actions.name action)
          (Result.string_of_result result);
        if Result.is_pass result
        then (
          (* Avoid clobbering [Predicate true] results *)
          match remaining_actions with
          | [] -> result, env'
          | _::_ -> run_actions_aux (action_number+1) env' remaining_actions
        )
        else (result, env')
      end in
  run_actions_aux 1 testenv actions

let quote ?kind env name =
  let get_dir v =
    match Environments.lookup v env with
    | Some "" | None -> failwith ("Expected directory " ^ Variables.name_of_variable v)
    | Some dir -> dir
  in
  let src = get_dir Builtin_variables.test_source_directory in
  let build = get_dir Builtin_variables.test_build_directory_prefix in
  let quote_filename fn =
    let ok_char = function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' | '/' | '.' -> true
      | _ -> false
    in
    if String.for_all ok_char fn then fn else Filename.quote fn
  in
  let prefixes =
    [build, `Build;
     src, `Src;
     Ocaml_directories.srcdir_path [], `OCaml]
  in
  let prefix =
    prefixes
    |> List.map (fun (p, k) -> Filename.concat p "", k) (* add trailing sep *)
    |> List.find_map (fun (prefix, kind) ->
      if String.starts_with ~prefix name then
        let len = String.length prefix in
        Some (String.sub name len (String.length name - len), kind)
      else None)
  in
  let s, kind' =
    match prefix with
    | Some (name, `Build) ->
      Filename.concat "BUILD" (quote_filename name), `Build
    | Some (name, `Src) ->
      Filename.concat "SRC" (quote_filename name), `Src
    | Some (name, `OCaml) ->
      Filename.concat "OCAML" (quote_filename name), `OCaml
    | None ->
    if String.starts_with ~prefix:"/@" name then
      let pos = try String.index_from name 2 '@' with Not_found -> String.length name in
      let pfx =
        match String.sub name 2 (pos - 2) with
        | "src" -> "\"${SRC_DIR}\""
        | "build" -> "\"${BUILD_DIR}\""
        | s -> failwith ("unknown prefix tag " ^ s)
      in
      let pos = pos + 1 in (* skip '@' *)
      pfx ^ quote_filename (String.sub name pos (String.length name - pos)), `Misc
    else
      quote_filename name, `Misc
  in
  (match kind with Some k -> assert (k = kind') | None -> ());
  s

let print_task_item env (t : Actions.task_item) : unit =
  let quote ?kind t = quote ?kind env t in
  match t with
  | Chdir dir ->
     Printf.printf "cd %s\n" (quote ~kind:`Build dir)
  | Check_file_exists {filename; reason=_} -> Printf.printf "@FIXMEcheck %s\n" filename
  | Check_file_absent {filename} -> Printf.printf "@FIXMEcheck_abs %s\n" filename
  | Copy_file {src;dst} -> Printf.printf "cp %s %s\n" (quote src) (quote dst)
  | Setup_build_env {source_dir;build_dir;remove;files;subdirs} ->
     let source_dir' = Environments.safe_lookup Builtin_variables.test_source_directory env in
     if source_dir <> source_dir' then failwith "setup_build_env: invalid source_dir";
(*
     List.iter (check_name `Src) files;
     List.iter (check_name `Src) subdirs;
*)
     Printf.printf "rm -f %s\n" (String.concat " " (List.map (fun x -> quote x) remove));
     Printf.printf "mkdir -p %s\n" (quote ~kind:`Build build_dir);
     if files <> [] then
       Printf.printf "cp %s %s\n" (String.concat " " (List.map (fun f -> quote (Filename.concat source_dir f)) files)) (quote build_dir);
     if subdirs <> [] then
       subdirs |> List.iter (fun dir -> Printf.printf "cp -R %s %s\n" (quote (Filename.concat source_dir dir)) (quote build_dir));
     Printf.printf "cd %s\n" (quote build_dir)
  | Run_cmd cmd ->
     let Run_command.{ progname = _; argv; envp; stdin_filename; stdout_filename; stderr_filename; append; timeout=_; log=_ } = cmd.settings in
     let redirects =
       let op = if append then ">>" else ">" in
       (if stdin_filename <> "" then ["<" ^ quote stdin_filename] else []) @
       (if stdout_filename <> "" then [op ^ quote stdout_filename] else []) @
       (if stderr_filename <> "" then if stderr_filename = stdout_filename then ["2>&1"] else [op ^ quote stderr_filename] else [])
     in
     envp |> Array.iter (fun s -> Printf.printf "# %s\n" s);
     let cmd = String.concat " " ((Array.to_list argv |> List.map (quote ?kind:None)) @ redirects) in
     Printf.printf "%s\n" cmd
  | Compare_files cmp ->
     Printf.printf "diff %s %s\n" (quote cmp.files.reference_filename) (quote cmp.files.output_filename)

let _print_task_items env its = List.iter (print_task_item env) its

let ocaml_dependency_paths : (string * string * Sexp.t list option) list=
  [ "ocamltest", "ocamltest/ocamltest.native", None;
    "ocamlc", "main_native.bc",
    Some ([A "%{project_root}/main_native.bc";
           S [A "glob_files"; A "%{project_root}/stdlib/build/libcamlrun{,d}.a"];
           S [A "glob_files"; A "%{project_root}/boot_stublibs/*"]]);
    "ocamlopt", "boot_ocamlopt.bc",
    Some ([A "%{project_root}/boot_ocamlopt.bc";
           S [A "glob_files"; A "%{project_root}/stdlib/build/libasmrun{,d}.a"];
           S [A "glob_files"; A "%{project_root}/boot_stublibs/*"]]);
    "ocamlc.opt", "main_native.exe",
    Some ([A "%{project_root}/main_native.exe";
           S [A "glob_files"; A "%{project_root}/stdlib/build/libcamlrun{,d}.a"]]);
    "ocamlopt.opt", "boot_ocamlopt.exe",
    Some ([A "%{project_root}/boot_ocamlopt.exe";
           S [A "glob_files"; A "%{project_root}/stdlib/build/libasmrun{,d}.a"]]);
    "runtime", "stdlib/build",
    Some ([S [A "glob_files"; A "%{project_root}/stdlib/build/lib{caml,asm}run{,d}.a"];
           S [A "glob_files"; A "%{project_root}/stdlib/build/caml/*.{h,tbl}"]]);
    "runtime/ocamlrun", "runtime/ocamlrun", None;
    "runtime/ocamlrund", "runtime/ocamlrund", None;
    "stdlib", "stdlib/build",
    Some ([S [A "glob_files"; A "%{project_root}/stdlib/build/{*.{cmi,cmx,cmo},stdlib.{cma,cmxa,a},std_exit.{cmo,cmx,o},runtime-launch-info}"]]);
    "tools/ocamlfilt", "tools/ocamlfilt.exe", None;
  ]

let ocaml_dependency_table =
  let table = Hashtbl.create 10 in
  ocaml_dependency_paths |> List.iter (fun (k, file, dep) ->
    let file = "%{project_root}/" ^ file in
    let dep = Option.value ~default:[Sexp.A file] dep in
    Hashtbl.replace table k (file, dep));
  table

let generate_dune_dependencies ~dir:relative_dir env its =
  let add_trailing_sep s = Filename.concat s "" in
  let src_dir' =
    Environments.safe_lookup Builtin_variables.test_source_directory env
  in
  let src_dir = add_trailing_sep src_dir' in
  let build_dir' =
    Environments.safe_lookup Builtin_variables.test_build_directory_prefix env
  in
  let build_dir = add_trailing_sep build_dir' in
  let ocaml_dir' = Ocaml_directories.srcdir in
  let ocaml_dir = add_trailing_sep ocaml_dir' in
  let test_file = Environments.safe_lookup Builtin_variables.test_file env in
  let dep_files = ref [test_file] in
  let dep_dirs = ref [] in
  let dep_ocaml = ref [] in
  let cmp = ref [] in
  let check_path path =
    (* FIXME: is the -I hack useful? *)
    if false && String.starts_with ~prefix:"-I" path then
      Printf.eprintf "PATH %s\n" path;
    let path =
      (* Special case: -I/path/to/foo options *)
      Option.value ~default:path (String.remove_prefix ~prefix:"-I" path)
    in
    (match String.remove_prefix ~prefix:src_dir path with
     | Some dep when not (String.starts_with ~prefix:build_dir path) ->
        dep_files := dep :: !dep_files
     | _ -> ());
    (match String.remove_prefix ~prefix:ocaml_dir path with
     | Some dep -> dep_ocaml := dep :: !dep_ocaml
     | None -> ())
  in
  let collect_deps : Actions.task_item -> unit = function
    | Chdir _ -> ()
    | Copy_file {src; dst=_} -> check_path src
    | Check_file_exists p -> check_path p.filename
    | Check_file_absent _ -> ()
    | Setup_build_env { source_dir; build_dir = _; remove = _; files; subdirs } ->
       if source_dir <> src_dir' then failwith (Printf.sprintf "setup_build_env: invalid source_dir %s (expected %s)" source_dir src_dir') ;
       dep_files := files @ !dep_files;
       dep_dirs := subdirs @ !dep_dirs
    | Run_cmd { settings = { progname; argv; _ }; _ } ->
       List.iter check_path (progname :: Array.to_list argv)
    | Compare_files {files; _} ->
       let maybe_strip_src s =
         Option.value (String.remove_prefix ~prefix:src_dir s) ~default:s in
       let reference =
         Filename.concat relative_dir (maybe_strip_src files.reference_filename) in
       let output = maybe_strip_src (promoted_output files) in
       cmp := (reference, output) :: !cmp;
  in
  List.iter collect_deps its;
  let dep_files = List.sort_uniq String.compare !dep_files in
  let dep_dirs = List.sort_uniq String.compare !dep_dirs in
  let dep_ocaml, missing_dep_ocaml =
    List.sort_uniq String.compare !dep_ocaml
    |> List.partition_map (fun cmd ->
      match Hashtbl.find ocaml_dependency_table cmd with
      | (path, deps) -> Left (cmd, path, deps)
      | exception Not_found -> Right cmd)
  in

  match missing_dep_ocaml with
  | [] ->
     let open Sexp in
     let ocamltest_cmd, ocamltest_deps =
       Hashtbl.find ocaml_dependency_table "ocamltest" in
     let deps =
       (dep_files |> List.map (fun s -> A s)) @
       (dep_dirs |> List.map (fun s ->
         S [A "glob_files_rec"; A (Filename.concat s "**")])) @
       ocamltest_deps @
       (dep_ocaml |> List.concat_map (fun (_,_,deps) -> deps))
     in
     let ldpath =
       String.concat ":" ("%{project_root}/boot_stublibs" :: [](*Option.to_list (Sys.getenv_opt "CAML_LD_LIBRARY_PATH")*))
     in
     let cmd =
       [A ocamltest_cmd; A "-no-diff"; A "-no-ocamlsrcdir"; A test_file;
        A "-override"; A "ocamlrun_sys=ocamlrun"] @
       (dep_ocaml |> List.concat_map (fun (cmd,path,_) ->
         [A "-override"; A (cmd ^ "=" ^ path)]))
     in
     let action =
       S (A "progn" ::
           S [A "setenv"; A "CAML_LD_LIBRARY_PATH"; A ldpath;
              S (A "run" :: cmd)] ::
          (!cmp |> List.map (fun (r, p) -> S [A "diff?"; A r; A p])))
     in
     Ok (S [A "rule";
            S [A "aliases"; A test_file; A "test"];
            S (A "deps" :: deps);
            S [A "action"; action]])
  | missing ->
     Error missing

let print_task_items ~dir env its =
  let test_file = Environments.safe_lookup Builtin_variables.test_file env in
  match generate_dune_dependencies ~dir env its with
  | Ok dune ->
     Format.printf ";; TEST OK %s@." test_file;
     Format.printf "%a@.@." Sexp.pp dune;
  | Error missing ->
     Format.printf ";; TEST UNAVAILABLE %s@." test_file;
     Format.printf ";; Unavailable dependencies %s@.@." (String.concat ", " missing)
