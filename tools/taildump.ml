open Cmx_format
open Clambda

let rec dump_tail_functs ui = function
  | Value_closure (_, {fun_may_tail = []; _}, _) -> ()
  | Value_unknown | Value_const _ | Value_global_field _ -> ()
  | Value_tuple (_, vs) -> Array.iter (dump_tail_functs ui) vs

  | Value_closure (_, fdesc, _) ->
     Format.printf "%s: %s\n" ui.ui_name fdesc.fun_label;
     fdesc.fun_may_tail |> List.iter (fun (lbl, d) -> Format.printf "  %s: %a@." (Option.value ~default:"?" lbl) Debuginfo.print_compact d)

let taildump file =
  let open Misc.Magic_number in
  let ic = open_in file in
  let kind = Cmx { flambda = false } in
  match read_current_info ~expected_kind:(Some kind) ic with
  | Error e ->
     let msg =
       match e with
       | Parse_error e -> explain_parse_error (Some kind) e
       | Unexpected_error e -> explain_unexpected_error e
     in
     Printf.fprintf stderr "%s: %s\n%!" file msg
  | Ok _ ->
     let ui = (input_value ic : unit_infos) in
     close_in ic;
     let apx =
       match ui.ui_export_info with
       | Clambda a -> a
       | _ -> assert false
     in
     dump_tail_functs ui apx
    

let () =
  List.iter taildump (List.tl (Array.to_list Sys.argv))
