module MultiLevelState = struct
  type t = {
    current_level : int;
    total_levels : int;
    player_has_amulet : bool;
    maps : (Base.int, Map.Tilemap.t) Base.Hashtbl.t;
  }

  let current_map state = Base.Hashtbl.find_exn state.maps state.current_level
  let has_next_level state = state.current_level < state.total_levels

  let next_level state =
    if has_next_level state then
      { state with current_level = state.current_level + 1 }
    else state

  let set_amulet state = { state with player_has_amulet = true }
  let has_amulet state = state.player_has_amulet
end

type game_state = { backend : Backend.t; multi_level : MultiLevelState.t }

(* Load the next level: if not on the last, generate its map if needed, update backend and multi_level *)
let load_next_level (gs : game_state) : game_state =
  let open Mapgen in
  let open Backend in
  let ml = gs.multi_level in
  if not (MultiLevelState.has_next_level ml) then gs
  else
    let config = Config.default ~seed:gs.backend.seed in
    let new_ml = MultiLevelState.next_level ml in
    let level = new_ml.current_level in
    (* Generate map for this level if not already present *)
    (match Base.Hashtbl.find new_ml.maps level with
    | Some _ -> ()
    | None ->
        let tilemap =
          Generator.generate ~config ~level ~total_levels:new_ml.total_levels
        in
        Base.Hashtbl.set new_ml.maps ~key:level ~data:tilemap);
    let new_map = MultiLevelState.current_map new_ml in
    let new_backend =
      { gs.backend with map = new_map; mode = Mode.CtrlMode.Normal }
    in
    (* Reposition player to entry stair on new level, or default start if missing *)
    let player_id = gs.backend.player.entity_id in
    let entry_pos =
      match new_map.stairs_up with
      | Some pos -> pos
      | None -> new_map.player_start
    in
    E.EntityManager.update new_backend.entities player_id (fun ent ->
        { ent with pos = entry_pos });
    { backend = new_backend; multi_level = new_ml }

(* Load the previous level: if above the first, reuse or generate map, update state *)
let load_prev_level (gs : game_state) : game_state =
  let open Mapgen in
  let open Backend in
  let ml = gs.multi_level in
  if ml.current_level <= 1 then gs
  else
    let config = Config.default ~seed:gs.backend.seed in
    let new_ml = { ml with current_level = ml.current_level - 1 } in
    let level = new_ml.current_level in
    let new_map =
      match Base.Hashtbl.find new_ml.maps level with
      | Some m -> m
      | None ->
          let tilemap =
            Generator.generate ~config ~level ~total_levels:new_ml.total_levels
          in
          Base.Hashtbl.set new_ml.maps ~key:level ~data:tilemap;
          tilemap
    in
    let new_backend =
      { gs.backend with map = new_map; mode = Mode.CtrlMode.Normal }
    in
    (* Reposition player to entry stair on previous level, or default start if missing *)
    let player_id = gs.backend.player.entity_id in
    let entry_pos =
      match new_map.stairs_down with
      | Some pos -> pos
      | None -> new_map.player_start
    in
    E.EntityManager.update new_backend.entities player_id (fun ent ->
        { ent with pos = entry_pos });
    { backend = new_backend; multi_level = new_ml }
