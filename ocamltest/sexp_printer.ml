type sexp = A of string | S of sexp list

let example = S [A "foo"; A "bar"; A "baz"; S []; S [S[A "1"; A "2"]; A "b"; A "x y \""; S[S[A "a"]]; S (List.init 100 (fun _i -> A "asdfasdfasdfasdf"))]]

let unquoted s =
  s |> String.for_all (function
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' -> true
    | _ -> false)

let rec pp_sexp ppf =
  let open Format in
  function
  | A s when unquoted s ->
     fprintf ppf "%s" s
  | A s ->
     fprintf ppf "%S" s
  | S items ->
     fprintf ppf "@[<hv>(%a)@]"
       (Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "@;<1 1>") pp_sexp)
       items

let () =
  Format.printf "%a@." pp_sexp example
  
