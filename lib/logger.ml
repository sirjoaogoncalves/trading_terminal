(* lib/logger.ml *)
let debug_enabled = ref true

let log msg =
  if !debug_enabled then
    Lwt_io.printl ("[DEBUG] " ^ msg)
  else
    Lwt.return_unit
