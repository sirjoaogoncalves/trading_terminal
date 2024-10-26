(lang dune 3.16)
(name trading_terminal)
(version 0.1.0)

(generate_opam_files true)

(package
 (name trading_terminal)
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.0))
  (lwt (>= 5.6.1))
  (cohttp-lwt-unix (>= 5.0.0))
  (yojson (>= 2.1.0))
  (cmdliner (>= 1.1.0))
  (notty (>= 0.2.3))
  (ANSITerminal (>= 0.8.5))
  lwt_ppx))
