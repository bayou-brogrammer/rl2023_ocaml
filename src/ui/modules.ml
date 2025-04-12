(* Main modules of game. They don't carry much state between them *)
module R = Renderer
module B = Backend
open Modules_d

let handle_tick (s : State.t) =
  let state =
    match s.screen with
    (* | MainMenu m -> Mainmenu.handle_tick m *)
    (* | Play -> Play.handle_tick s *)
    | _ -> s
  in
  state

let handle_event (s : State.t) =
  let open Raylib in
  match s.screen with
  | MainMenu m ->
      let key = get_key_pressed () in
      (* let new_m, should_quit =
        match key with
        | Key.Q -> ({ s with screen = MainMenu m }, true)
        | Key.Up -> ({ m with selected = max 0 (m.selected - 1) }, false)
        | Key.Down -> ({ m with selected = min 1 (m.selected + 1) }, false)
        | Key.Enter -> (m, false)
        | _ -> (m, false)
      in *)

      ({ s with screen = MainMenu m }, false)
  | MapGen -> (s, false)
  | Playing ->
      let new_s = Play.handle_event s in
      (new_s, false)

let render (s : State.t) =
  match s.screen with
  | MainMenu s -> Mainmenu.render s
  | MapGen -> ()
  | Playing -> Play.render s

let run () : unit =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);

  Printf.printf "Loading resources...";
  print_newline ();

  (* Initialize Game *)
  let init_fn font =
    (* Create State *)
    let create_state ?backend ?ui_options ?ui_view screen =
      let backend =
        match backend with
        | Some b -> b
        | None ->
            (* Used by different elements *)
            let random = Rng.get_state () in
            let seed = Rng.seed_int in
            B.make ~debug:true ~w:80 ~h:50 ~random ~seed
      in

      let _ = ui_options in
      let _ = ui_view in
      { State.screen; backend; player_pos = (10, 10); font }
    in

    (* Let's Roll *)
    let state = create_state (Modules_d.MainMenu Mainmenu.init) in
    (Logs.info @@ fun m -> m "initialization done.");
    (state, Mainloop.{ handle_event; handle_tick; render })
  in
  Mainloop.main init_fn
