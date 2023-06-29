let active = 0
let paused = 1
let finalized = 2

type t = {
  mutable elapsed : float;
  mutable last_active : float;
  mutable status : int;
}

let pause t =
  if t.status = active then (
    let cur_time = Unix.gettimeofday () in
    t.elapsed <- t.elapsed +. (cur_time -. t.last_active);
    t.status <- paused)

let continue t =
  if t.status = paused then (
    let cur_time = Unix.gettimeofday () in
    t.status <- active;
    t.last_active <- cur_time)

let finalize t =
  (if t.status = active then
     let cur_time = Unix.gettimeofday () in
     t.elapsed <- t.elapsed +. (cur_time -. t.last_active));
  t.status <- finalized;
  ()

let finalize_and_get_elapsed t =
  finalize t;
  t.elapsed

let create () =
  let cur_time = Unix.gettimeofday () in
  { elapsed = 0.0; last_active = cur_time; status = active }
