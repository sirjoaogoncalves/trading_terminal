open Trading_terminal_lib

let () =
  let open Lwt.Syntax in
  Lwt_main.run (
    let* result = Terminal.initialize () in
    match result with
    | Ok () -> 
        Lwt_io.printl "Trading Terminal Closed Successfully"
    | Error err -> 
        Lwt_io.printf "Error: %s\n" err
  )
