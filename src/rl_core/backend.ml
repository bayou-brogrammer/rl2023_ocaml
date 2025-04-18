module Actor = Actor
module Tilemap = Map.Tilemap
module Tile = Map.Tile
module E = Entity
open Base
open Mode
open Types

let src = Logs.Src.create "backend" ~doc:"Backend"

module Log = (val Logs.src_log src : Logs.LOG)

type t = {
  seed : int;
  debug : bool;
  map : Tilemap.t;
  mode : Mode.CtrlMode.t;
  random : Random.State.t;
  entities : E.EntityManager.t;
  actor_manager : Actor_manager.t;
  turn_queue : Turn_queue.t;
  player : E.player;
}

let make ~debug ~w ~h ~seed =
  Logs.info (fun m -> m "Creating backend with seed: %d" seed);
  Logs.info (fun m -> m "Width: %d, Height: %d" w h);

  let random = Random.State.make [| seed |] in

  let entities = E.EntityManager.create () in
  let actor_manager = Actor_manager.create () in
  let turn_queue = Turn_queue.create () in

  let open Mapgen in
  let config = Config.make ~seed ~w ~h in
  let map = Generator.generate ~config ~level:1 ~total_levels:5 in

  {
    debug;
    seed;
    random;
    map;
    entities;
    actor_manager;
    turn_queue;
    mode = CtrlMode.Normal;
    player = { entity_id = 0 };
  }

(* Helper function to get all entities *)
let get_entities (backend : t) : E.entity list =
  E.EntityManager.to_list backend.entities

let get_entity_manager (backend : t) : E.EntityManager.t = backend.entities
let get_actor_manager (backend : t) : Actor_manager.t = backend.actor_manager

let get_entity (backend : t) (entity_id : E.entity_id) : E.entity option =
  E.EntityManager.find backend.entities entity_id

let get_actor (backend : t) (actor_id : Actor_manager.actor_id) : Actor.t option
    =
  Actor_manager.get backend.actor_manager actor_id

let add_actor (backend : t) (actor : Actor.t)
    (actor_id : Actor_manager.actor_id) : t =
  Actor_manager.add backend.actor_manager actor_id actor;
  backend

let remove_actor (backend : t) (actor_id : Actor_manager.actor_id) : t =
  Actor_manager.remove backend.actor_manager actor_id;
  backend

let get_entity_at_pos (entities : E.EntityManager.t) (pos : loc) :
    E.entity option =
  E.EntityManager.find_by_pos entities pos

(* Helper function to get player entity *)
let get_player (backend : t) : E.entity =
  E.EntityManager.find_unsafe backend.entities backend.player.entity_id

let get_player_pos (backend : t) : loc =
  let player = get_player backend in
  player.pos

let get_player_actor (backend : t) : Actor.t =
  let player = get_player backend in
  match player.data with
  | E.PlayerData { actor_id; _ } ->
      Actor_manager.get_unsafe backend.actor_manager actor_id
  | _ -> failwith "Player actor not found"

let get_actor (backend : t) (entity_id : E.entity_id) : Actor.t =
  let entity = E.EntityManager.find_unsafe backend.entities entity_id in
  match entity.data with
  | E.PlayerData { actor_id; _ } | E.CreatureData { actor_id; _ } ->
      Actor_manager.get_unsafe backend.actor_manager actor_id
  | _ -> failwith "Actor not found"

let move_entity (backend : t) (entity_id : E.entity_id) (x : int) (y : int) :
    unit =
  E.EntityManager.update backend.entities entity_id (fun ent ->
      { ent with pos = (x, y) })

(* Convert to simplified backend interface *)
let to_common_backend (b : t) =
  object
    method get_player_id = b.player.entity_id
    method get_map_width = Tilemap.get_width b.map
    method get_map_height = Tilemap.get_height b.map
    method is_tile_walkable pos = Tile.is_walkable (Tilemap.get_tile b.map pos)
    method move_entity entity_id x y = move_entity b entity_id x y
  end
