(* Defunctionalized game actions and executor *)

open Base
module Types = Types
module E = Entity
module State = State

(** All possible actions that an actor can queue *)
type game_action =
  | Move of Types.direction * E.entity_id
  | StairsUp of E.entity_id
  | StairsDown of E.entity_id
  | Interact of E.entity_id
  | Pickup of E.entity_id
  | Drop of E.entity_id
  | Use of E.entity_id
  | Equip of E.entity_id

(** Execute a [game_action] on a [State.game_state], returning updated state and
    time cost. *)
let execute (action : game_action) (gs : State.game_state) :
    (State.game_state * int, exn) Result.t =
  match action with
  | Move _ -> Error (Failure "Move not yet implemented")
  | StairsUp _ -> Ok (State.load_next_level gs, 0)
  | StairsDown _ -> Ok (State.load_prev_level gs, 0)
  | Interact _ | Pickup _ | Drop _ | Use _ | Equip _ -> Ok (gs, 0)

(** Convert a [game_action] to a string for logging/debug. *)
let to_string = function
  | Move (dir, eid) ->
      Printf.sprintf "Move(%s, entity=%d)" (Types.show_direction dir) eid
  | StairsUp _ -> "StairsUp"
  | StairsDown _ -> "StairsDown"
  | Interact _ -> "Interact"
  | Pickup _ -> "Pickup"
  | Drop _ -> "Drop"
  | Use _ -> "Use"
  | Equip _ -> "Equip"
