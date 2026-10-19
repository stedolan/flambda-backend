
let run_task_item ~log : Actions.task_item -> Result.t = function
  | Chdir dir  ->
     Builtin_actions.do_chdir dir
  | Check_file_exists { filename; reason } ->
     Builtin_actions.do_check_file_exists ~filename ~reason
  | Check_file_absent { filename } ->
     Builtin_actions.do_check_file_absent ~filename
  | Copy_file { src; dst } ->
     Builtin_actions.do_copy_file ~src ~dst log
  | Setup_build_env { source_dir; build_dir; files; subdirs } ->
     Actions_helpers.do_setup_build_env
       ~source_dir ~build_dir ~files ~subdirs
  | Run_cmd { settings; message; exits } ->
     Actions_helpers.do_run_cmd ~settings ~message ~exits log
  | Compare_files { kind_of_output; files; tool; promote } ->
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
     Ocaml_directories.srcdir, `OCaml]
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
  | Setup_build_env {source_dir;build_dir;files;subdirs} ->
     let source_dir' = Environments.safe_lookup Builtin_variables.test_source_directory env in
     if source_dir <> source_dir' then failwith "setup_build_env: invalid source_dir";
(*
     List.iter (check_name `Src) files;
     List.iter (check_name `Src) subdirs;
*)
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

let print_task_items env its = List.iter (print_task_item env) its
