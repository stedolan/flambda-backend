let () =
  exit (Optmaindriver.main Sys.argv Format.err_formatter
    ~flambda2:(fun ~ppf_dump -> assert false))
