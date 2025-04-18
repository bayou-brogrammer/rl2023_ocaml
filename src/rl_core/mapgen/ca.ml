module Tile = Map.Tile (* your Tile.Wall | Tile.Floor *)
open Base
open Core

let idx ~width x y = x + (y * width)

(* 1) Carve some random rooms *)
let carve_rooms grid ~rng ~width ~height ~attempts ~room_min ~room_max =
  for _ = 1 to attempts do
    let rw = Random.State.int rng (room_max - room_min + 1) + room_min in
    let rh = Random.State.int rng (room_max - room_min + 1) + room_min in
    let x0 = Random.State.int rng (width - rw - 2) + 1 in
    let y0 = Random.State.int rng (height - rh - 2) + 1 in
    for dy = 0 to rh - 1 do
      for dx = 0 to rw - 1 do
        grid.(idx ~width (x0 + dx) (y0 + dy)) <- Tile.Floor
      done
    done
  done

(* 2) Init: border walls + random interior *)
let init_grid ~rng ~width ~height ~(wall_prob : float) =
  let n = width * height in
  let grid =
    Array.init n ~f:(fun i ->
        let x = i % width and y = i / width in
        if x = 0 || y = 0 || x = width - 1 || y = height - 1 then Tile.Wall
        else if Float.(Random.State.float rng 1.0 < wall_prob) then Tile.Wall
        else Tile.Floor)
  in
  (* carve some big rooms first *)
  carve_rooms grid ~rng ~width ~height ~attempts:8 ~room_min:4 ~room_max:10;
  grid

(* count 8‑neigh walls *)
let count_wall_nbrs grid ~width ~height x y =
  let c = ref 0 in
  for dy = -1 to 1 do
    for dx = -1 to 1 do
      if dx <> 0 || dy <> 0 then
        let nx = x + dx and ny = y + dy in
        if nx < 0 || ny < 0 || nx >= width || ny >= height then Int.incr c
        else if Tile.equal grid.(idx ~width nx ny) Tile.Wall then Int.incr c
    done
  done;
  !c

(* 3) CA smoothing: two‑pass rule *)
let smooth grid ~width ~height ~passes =
  let cur = Array.copy grid in
  let next = Array.copy grid in
  for _ = 1 to passes do
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        let i = idx ~width x y in
        if x = 0 || y = 0 || x = width - 1 || y = height - 1 then
          next.(i) <- Tile.Wall
        else
          let w = count_wall_nbrs cur ~width ~height x y in
          next.(i) <-
            (if w >= 5 then Tile.Wall
             else if w <= 2 then Tile.Floor
             else cur.(i))
      done
    done;
    Array.blit ~src:next ~dst:cur ~src_pos:0 ~dst_pos:0 ~len:(width * height)
  done;
  cur

(* 4) Flood‑fill floor regions and remove small ones *)
let remove_small_regions grid ~width ~height ~min_size =
  let total = width * height in
  let seen = Array.init total ~f:(fun _ -> false) in
  let q = Deque.create () in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let i = idx ~width x y in
      if (not seen.(i)) && Tile.equal grid.(i) Tile.Floor then (
        (* collect one region *)
        Deque.enqueue_back q (x, y);
        seen.(i) <- true;
        let region = ref [ (x, y) ] in
        while not (Deque.is_empty q) do
          let cx, cy = Deque.dequeue_front_exn q in
          List.iter
            [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
            ~f:(fun (dx, dy) ->
              let nx = cx + dx and ny = cy + dy in
              if nx >= 0 && ny >= 0 && nx < width && ny < height then
                let ni = idx ~width nx ny in
                if (not seen.(ni)) && Tile.equal grid.(ni) Tile.Floor then (
                  seen.(ni) <- true;
                  region := (nx, ny) :: !region;
                  Deque.enqueue_back q (nx, ny)))
        done;
        (* if too small, fill with walls *)
        if List.length !region < min_size then
          List.iter !region ~f:(fun (rx, ry) ->
              grid.(idx ~width rx ry) <- Tile.Wall))
    done
  done

(* 5) Run it all *)
let run ~width ~height ~rng =
  let wall_prob = 0.4 in
  let ca_passes = 4 in
  let min_region = 80 in
  init_grid ~rng ~width ~height ~wall_prob |> fun g ->
  smooth g ~width ~height ~passes:ca_passes |> fun g ->
  remove_small_regions g ~width ~height ~min_size:min_region;
  g
