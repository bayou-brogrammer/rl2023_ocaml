(* Abstract world interface for game_state mutations *)

(** Opaque game_state type for action execution *)
type game_state = State.game_state

(** Load the next level, preserving other game_state fields. *)
val load_next_level : game_state -> game_state

(** Load the previous level, preserving other game_state fields. *)
val load_prev_level : game_state -> game_state
