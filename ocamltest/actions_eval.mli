val run_task : log:out_channel -> Actions.task -> Result.t

val run :
  out_channel -> Environments.t -> Actions.t list -> Result.t * Environments.t

val print_task_items : dir:string -> Environments.t -> Actions.task_item list -> unit
