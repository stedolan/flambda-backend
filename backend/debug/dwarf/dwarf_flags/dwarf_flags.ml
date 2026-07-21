open! Int_replace_polymorphic_compare

let restrict_to_upstream_dwarf = ref (not Config.oxcaml_dwarf)

(* Currently the maximum number of stack slots, see asmgen.ml *)
let dwarf_max_function_complexity = ref 50

let dwarf_for_startup_file = ref false

type dwarf_version =
  | Four
  | Five

let default_gdwarf_version = Four

let gdwarf_version = ref default_gdwarf_version

let default_gdwarf_offsets = false

let gdwarf_offsets = ref default_gdwarf_offsets

let default_ddebug_invariants = false

let ddebug_invariants = ref default_ddebug_invariants

let default_ddebug_available_regs = false

let ddebug_available_regs = ref default_ddebug_available_regs

let default_ddwarf_types = false

let ddwarf_types = ref default_ddwarf_types

type dwarf_format =
  | Thirty_two
  | Sixty_four

let default_gdwarf_format = Thirty_two

let gdwarf_format = ref default_gdwarf_format

let default_gdwarf_self_tail_calls = true

let gdwarf_self_tail_calls = ref default_gdwarf_self_tail_calls

let gdwarf_may_alter_codegen = ref false

let gdwarf_may_alter_codegen_experimental = ref false

let dwarf_inlined_frames = ref false

let default_gdwarf_compression = "zlib"

let gdwarf_compression = ref default_gdwarf_compression

let ddwarf_metrics = ref false

let ddwarf_metrics_output_file : string option ref = ref None

let get_dwarf_compression_flag () =
  if !dwarf_inlined_frames || not !restrict_to_upstream_dwarf
  then Some !gdwarf_compression
  else None

let get_dwarf_compression_format () =
  match get_dwarf_compression_flag () with
  | Some compression
    when (not (String.equal compression ""))
         && not (String.equal compression "none") ->
    Some compression
  | _ -> None

let get_dwarf_objcopy_compression_format () =
  (* Only use compression with objcopy if it supports it *)
  if String.equal Config.objcopy_compress_debug_sections_flag ""
  then None
  else get_dwarf_compression_format ()

let get_dwarf_c_toolchain_flag () =
  match get_dwarf_compression_flag () with
  | Some compression ->
    if not (String.equal Config.cc_compress_debug_sections_flag "")
    then " " ^ Config.cc_compress_debug_sections_flag ^ "=" ^ compression
    else ""
  | None -> ""

let get_dwarf_as_toolchain_flag () =
  match get_dwarf_compression_flag () with
  | Some compression ->
    if not (String.equal Config.as_compress_debug_sections_flag "")
    then " " ^ Config.as_compress_debug_sections_flag ^ "=" ^ compression
    else ""
  | None -> ""
