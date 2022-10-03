(* TEST
   modules = "async_exns_stubs.c"
*)

let () = Sys.catch_break true

(* Lifted out to ensure this works in bytecode, where [b] is easily
   held on to as a root on the stack. *)
let[@inline never] allocate_bytes finished =
  let b = Bytes.create 42 in
  Gc.finalise_last (fun () ->
      finished := true;
      raise Sys.Break)
    b;
  ref (Some b)

(* Ensure that an async exn raised from a finaliser skips an exception
   handler, placed around the point where the GC was invoked, instead arriving
   at an outer [Sys.with_async_exns] point. *)
let () =
  let finished = ref false in
  let r = allocate_bytes finished in
  try
    Sys.with_async_exns (fun () ->
      try
        r := None;
        while true do
          (* This allocation will eventually trigger the finaliser *)
          let _ = Sys.opaque_identity (42, Random.int 42) in
          ()
        done
      with exn -> Printf.printf "1. wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> assert !finished; Printf.printf "1. OK\n%!"
  | _ -> assert false

(* Ensure that [Sys.Break] can be raised and caught as a normal exception. *)
let () =
  try
    Sys.with_async_exns (fun () ->
      try raise (Sys.opaque_identity Sys.Break)
      with Sys.Break -> Printf.printf "2. OK\n%!"
    )
  with
  | _ -> Printf.printf "2. wrong handler\n%!"; assert false

(* Ensure that [caml_callback_exn] collects async exns arising from the
   callback. *)
let raise_break_from_finaliser () =
  let finished = ref false in
  let r = allocate_bytes finished in
  try
    r := None;
    while true do
      let _ = Sys.opaque_identity (42, Random.int 42) in
      ()
    done
  with exn -> Printf.printf "3a. wrong handler\n%!"; exit 1

external test_caml_callback_exn_collects_async_exns : (unit -> unit) -> unit
  = "test_caml_callback_exn_collects_async_exns"

let () =
  try
    Sys.with_async_exns (fun () ->
      test_caml_callback_exn_collects_async_exns raise_break_from_finaliser);
    Printf.printf "3a. OK\n%!"
  with
  | _ -> assert false

(* Same but for a 2-parameter callback *)

external test_caml_callback2_exn_collects_async_exns
  : (unit -> unit -> unit) -> unit
  = "test_caml_callback2_exn_collects_async_exns"

let raise_break_from_finaliser2 () () =
  raise_break_from_finaliser ()

let () =
  try
    Sys.with_async_exns (fun () ->
      test_caml_callback2_exn_collects_async_exns raise_break_from_finaliser2);
    Printf.printf "3b. OK\n%!"
  with
  | _ -> assert false

(* Same but for a 3-parameter callback *)

external test_caml_callback3_exn_collects_async_exns
  : (unit -> unit -> unit -> unit) -> unit
  = "test_caml_callback3_exn_collects_async_exns"

let raise_break_from_finaliser3 () () () =
  raise_break_from_finaliser ()

let () =
  try
    Sys.with_async_exns (fun () ->
      test_caml_callback3_exn_collects_async_exns raise_break_from_finaliser3);
    Printf.printf "3c. OK\n%!"
  with
  | _ -> assert false

(* Same but for a 4-parameter callback *)

external test_caml_callbackN_exn_collects_async_exns
  : (unit -> unit -> unit -> unit -> unit) -> unit
  = "test_caml_callbackN_exn_collects_async_exns"

let raise_break_from_finaliser4 () () () () =
  raise_break_from_finaliser ()

let () =
  try
    Sys.with_async_exns (fun () ->
      test_caml_callbackN_exn_collects_async_exns raise_break_from_finaliser4);
    Printf.printf "3d. OK\n%!"
  with
  | _ -> assert false

external test_caml_callback_reraises_async_exns_as_async_exns
  : (unit -> unit) -> unit
  = "test_caml_callback_reraises_async_exns_as_async_exns"

(* Ensure that [caml_callback] reraises an async exn arising from the
   callback, and that such reraise is done as an async raise. *)
let () =
  try
    Sys.with_async_exns (fun () ->
      try
        test_caml_callback_reraises_async_exns_as_async_exns
          raise_break_from_finaliser
      with exn -> Printf.printf "4a. wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> Printf.printf "4a. OK\n%!"
  | _ -> assert false

(* Same but for a 2-parameter callback *)

external test_caml_callback2_reraises_async_exns_as_async_exns
  : (unit -> unit -> unit) -> unit
  = "test_caml_callback2_reraises_async_exns_as_async_exns"

let () =
  try
    Sys.with_async_exns (fun () ->
      try
        test_caml_callback2_reraises_async_exns_as_async_exns
          raise_break_from_finaliser2
      with exn -> Printf.printf "4b. wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> Printf.printf "4b. OK\n%!"
  | _ -> assert false

(* Same but for a 3-parameter callback *)

external test_caml_callback3_reraises_async_exns_as_async_exns
  : (unit -> unit -> unit -> unit) -> unit
  = "test_caml_callback3_reraises_async_exns_as_async_exns"

let () =
  try
    Sys.with_async_exns (fun () ->
      try
        test_caml_callback3_reraises_async_exns_as_async_exns
          raise_break_from_finaliser3
      with exn -> Printf.printf "4c. wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> Printf.printf "4c. OK\n%!"
  | _ -> assert false

(* Same but for a 4-parameter callback *)

external test_caml_callbackN_reraises_async_exns_as_async_exns
  : (unit -> unit -> unit -> unit -> unit) -> unit
  = "test_caml_callbackN_reraises_async_exns_as_async_exns"

let () =
  try
    Sys.with_async_exns (fun () ->
      try
        test_caml_callbackN_reraises_async_exns_as_async_exns
          raise_break_from_finaliser4
      with exn -> Printf.printf "4d. wrong handler\n%!"; assert false
    )
  with
  | Sys.Break -> Printf.printf "4d. OK\n%!"
  | _ -> assert false
