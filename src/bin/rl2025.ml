(* Copyright (c) 2025 Jacob LeCoq (Yendor). All rights reserved. *)

open Logs

let () =
  Clap.description "Rougelike Tutorial 2025";
  (* [flag_enum] is a generalization of [flag] for enums with more than 2 possible values. *)
  let level =
    Clap.flag_enum ~description:"Logging level"
      [
        ([ "app" ], [ 'a' ], App);
        ([ "info" ], [ 'i' ], Info);
        ([ "debug" ], [ 'd' ], Debug);
        ([ "warn" ], [ 'w' ], Warning);
        ([ "error" ], [ 'e' ], Error);
      ]
      Info
  in
  Clap.close ();

  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some level);

  (* Blast Off *)
  let () = Logs.info (fun m -> m "Starting main") in
  Rl_ui.Modules.run ()
