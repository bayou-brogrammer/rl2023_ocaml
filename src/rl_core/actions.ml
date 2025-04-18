open Base
open Types
open Entity
open State

(** All actions that can be queued by actors and processed by the turn system.
*)
type game_action =
  | Move of direction * entity_id
  | StairsUp of entity_id
  | StairsDown of entity_id
  | Interact of entity_id
  | Pickup of entity_id
  | Drop of entity_id
  | Use of entity_id
  | Equip of entity_id

(** Execute a [game_action] on a [game_state], returning updated state and time
    cost. *)
let execute (action : game_action) (gs : game_state) :
    (game_state * int, exn) Result.t =
  match action with
  | Move _ -> Error (Failure "Move not yet implemented")
  | StairsUp _ -> Ok (load_next_level gs, 0)
  | StairsDown _ -> Ok (load_prev_level gs, 0)
  | Interact _ | Pickup _ | Drop _ | Use _ | Equip _ -> Ok (gs, 0)

(** Convert a [game_action] to a string for logging/debug. *)
let to_string = function
  | Move (dir, eid) ->
      Printf.sprintf "Move(%s, entity=%d)" (show_direction dir) eid
  | StairsUp _ -> "StairsUp"
  | StairsDown _ -> "StairsDown"
  | Interact _ -> "Interact"
  | Pickup _ -> "Pickup"
  | Drop _ -> "Drop"
  | Use _ -> "Use"
  | Equip _ -> "Equip"
