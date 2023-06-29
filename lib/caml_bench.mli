module Timer : sig
  type t

  val continue : t -> unit
  val pause : t -> unit
  val finalize : t -> unit
end

type result = Types.bench_result = {
  bench_name : string;
  median_exec_time : float;
  avg_exec_time : float;
  num_of_runs : int;
  shortest_exec_time : float;
  longest_exec_time : float;
}

val run :
  ?pre:(unit -> unit) ->
  ?post:('a -> unit) ->
  ?runs:int ->
  name:string ->
  f:(Timer.t -> 'a) ->
  unit ->
  result

val report : result list -> unit
