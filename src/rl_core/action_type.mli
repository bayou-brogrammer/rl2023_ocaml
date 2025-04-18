(* Abstract representation of game actions for the turn system *)

open Types
open Entity
open State

(** All actions that can be queued and processed in a turn. *)
type game_action =
  | Move     of direction * entity_id
  | StairsUp of entity_id
  | StairsDown of entity_id
  | Interact of entity_id
  | Pickup   of entity_id
  | Drop     of entity_id
  | Use      of entity_id
  | Equip    of entity_id

(** Execute a [game_action] on a [State.game_state], returning the updated state and time cost. *)
val execute : game_action -> game_state -> (game_state * int, exn) Result.t

(** String representation of [game_action] for logging/debug. *)
val to_string : game_action -> string
