open Base
open Raylib
module State = State
module CoreState = Rl_core.State
module Backend = Rl_core.Backend
module T = Rl_core.Types
module A = Rl_core.Actor
module AM = Rl_core.Actor_manager
module Actions = Rl_core.Actions
module Mode = Rl_core.Mode

(** Key actions exposed to the UI loop. *)
type key_action =
  | Move of T.direction
  | Interact
  | Pickup
  | Drop
  | Use
  | Equip
  | StairsUp
  | StairsDown

(** Map a raw Raylib key into one of our UI key_actions. *)
let of_key (key : Key.t) : key_action option =
  match key with
  | Key.W | Key.Up -> Some (Move T.North)
  | Key.S | Key.Down -> Some (Move T.South)
  | Key.A | Key.Left -> Some (Move T.West)
  | Key.D | Key.Right -> Some (Move T.East)
  | Key.E -> Some Interact
  | Key.I -> Some Pickup
  | Key.O -> Some Drop
  | Key.U -> Some Use
  | Key.P -> Some Equip
  | Key.Comma -> Some StairsUp
  | Key.Period -> Some StairsDown
  | _ -> None

(** Apply a key_action to the current UI state, producing a new state. *)
let apply (state : State.t) (action : key_action) : State.t =
  match action with
  | Move dir ->
      let backend = state.game_state.backend in
      let actor_mgr = backend.actor_manager in
      let player = Backend.get_player backend in
      let move_act = Actions.make_move_action dir player in
      AM.update actor_mgr player.id (fun actor -> A.queue_action actor move_act);
      let new_backend =
        { backend with actor_manager = actor_mgr; mode = Mode.CtrlMode.Normal }
      in
      {
        state with
        game_state = { state.game_state with backend = new_backend };
      }
  | StairsUp ->
      let backend = state.game_state.backend in
      let tile =
        Backend.Tilemap.get_tile backend.map (Backend.get_player_pos backend)
      in
      if Backend.Tile.equal tile Backend.Tile.Stairs_up then (
        let player = Backend.get_player backend in
        let act = Actions.make_stairs_up_action player in
        AM.update backend.actor_manager player.id (fun actor ->
            A.queue_action actor act);
        let new_backend =
          {
            backend with
            actor_manager = backend.actor_manager;
            mode = Mode.CtrlMode.Normal;
          }
        in
        {
          state with
          game_state = { state.game_state with backend = new_backend };
        })
      else state
  | StairsDown ->
      let backend = state.game_state.backend in
      let tile =
        Backend.Tilemap.get_tile backend.map (Backend.get_player_pos backend)
      in
      if Backend.Tile.equal tile Backend.Tile.Stairs_down then (
        let player = Backend.get_player backend in
        let act = Actions.make_stairs_down_action player in
        AM.update backend.actor_manager player.id (fun actor ->
            A.queue_action actor act);
        let new_backend =
          {
            backend with
            actor_manager = backend.actor_manager;
            mode = Mode.CtrlMode.Normal;
          }
        in
        {
          state with
          game_state = { state.game_state with backend = new_backend };
        })
      else state
  | Interact | Pickup | Drop | Use | Equip ->
      (* TODO: implement these actions later *)
      state
