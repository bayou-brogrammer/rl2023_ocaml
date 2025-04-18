(* Abstract world interface for game_state mutations *)

(** World interface implementation delegating to State *)

type game_state = State.game_state
(** Abstract game_state type for action execution *)

(** Load the next level, preserving other state fields *)
let load_next_level = State.load_next_level

(** Load the previous level, preserving other state fields *)
let load_prev_level = State.load_prev_level
