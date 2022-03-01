let () =
  (match Sys.backend_type with
   | Native -> Memtrace.trace_if_requested ~context:"ocamlopt" ()
   | Bytecode | Other _ -> ());
  exit (Optmaindriver.main Sys.argv Format.err_formatter
    ~flambda2:(fun ~ppf_dump -> assert false))
