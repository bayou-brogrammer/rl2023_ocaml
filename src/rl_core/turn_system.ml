open Base
module B = Backend
module E = Entity

let monster_reschedule_delay = 100

(* Helper: get actor or log error and skip *)
let get_actor_safe actor_manager entity =
  match entity.E.data with
  | E.PlayerData { actor_id; _ } | E.CreatureData { actor_id; _ } -> (
      try Some (Actor_manager.get_unsafe actor_manager actor_id)
      with _ ->
        Logs.err (fun m -> m "Actor not found for entity: %d" entity.id);
        None)
  | _ ->
      Logs.err (fun m -> m "Entity %d is not a valid actor" entity.id);
      None

let get_entity_safe entities entity_id =
  try Some (Entity.EntityManager.find_unsafe entities entity_id)
  with _ ->
    Logs.err (fun m -> m "Entity not found: %d" entity_id);
    None

let get_next_actor actor_manager turn_queue entities =
  match Turn_queue.get_next_actor turn_queue with
  | None -> None
  | Some (entity_id, time) -> (
      Logs.info (fun m -> m "Processing turn for entity: %d" entity_id);
      let entity = get_entity_safe entities entity_id in
      match entity with
      | None -> None
      | Some entity -> (
          match get_actor_safe actor_manager entity with
          | None -> None
          | Some actor -> Some (entity_id, time, actor)))

(* Helper: should wait for player input? *)
let should_wait_for_player_input entity actor =
  match entity.E.kind with
  | E.Player -> Option.is_none (Actor.peek_next_action actor)
  | _ -> false

(* Remove dead actor from queue *)
let remove_dead_actor turn_queue entity_id =
  Logs.info (fun m -> m "Removing dead actor %d from queue" entity_id);
  Turn_queue.remove_actor turn_queue entity_id

(* Add helper to process a single actor's turn *)
let process_actor_event (gs : State.game_state) turn_queue entities entity_id time
    : State.game_state =
  Logs.info (fun m -> m "Processing turn for entity: %d" entity_id);
  let backend = gs.backend in
  match get_entity_safe entities entity_id with
  | None -> gs
  | Some entity -> (
      match get_actor_safe backend.actor_manager entity with
      | None -> gs
      | Some actor -> (
          if not (Actor.is_alive actor) then (
            Logs.info (fun m ->
                m "Actor %d is dead. Removing from queue." entity_id);
            remove_dead_actor turn_queue entity_id;
            gs)
          else if should_wait_for_player_input entity actor then (
            Logs.info (fun m -> m "Player is awaiting input");
            Turn_queue.schedule_turn turn_queue entity_id time;
            { gs with backend = { backend with mode = Mode.CtrlMode.WaitInput } })
          else
            match Actor.next_action actor with
            | None ->
                Logs.info (fun m ->
                    m "No action for entity: %d. Rescheduling turn." entity_id);
                Turn_queue.schedule_turn turn_queue entity_id time;
                gs
            | Some action -> (
                Logs.info (fun m ->
                    m "Action for entity: %d. Executing..." entity_id);
                match action#execute gs with
                | Ok (gs', d_time) ->
                    Turn_queue.schedule_turn turn_queue entity_id (time + d_time);
                    gs'
                | Error e ->
                    Logs.err (fun m ->
                        m "Failed to perform action: %s" (Exn.to_string e));
                    let delay =
                      match entity.E.kind with
                      | E.Player -> 0
                      | _ -> monster_reschedule_delay
                    in
                    Turn_queue.schedule_turn turn_queue entity_id (time + delay);
                    gs))))

let process_turns (gs : State.game_state) : State.game_state =
  let turn_queue = gs.backend.turn_queue in
  let entities = gs.backend.entities in
  if gs.backend.debug then Turn_queue.print_queue turn_queue;

  let rec process_loop (gs : State.game_state) : State.game_state =
    match gs.backend.mode with
    | Mode.CtrlMode.WaitInput ->
        Logs.info (fun m -> m "Waiting for player input");
        gs
    | _ -> (
        match Turn_queue.get_next_actor turn_queue with
        | None -> gs
        | Some (entity_id, time) ->
            let gs' =
              process_actor_event gs turn_queue entities entity_id time
            in
            process_loop gs')
  in
  process_loop gs
