let time_fn ~(f : Timer.t -> 'a) =
  let t = Timer.create () in
  let res = f t in
  let time_elapsed = Timer.finalize_and_get_elapsed t in
  (res, time_elapsed)

type result = Types.bench_result = {
  bench_name : string;
  median_exec_time : float;
  avg_exec_time : float;
  num_of_runs : int;
  shortest_exec_time : float;
  longest_exec_time : float;
}

let run ?(pre = Fun.id) ?(post = ignore) ?(runs = 10) ~name ~f () =
  assert (runs > 0);
  let runtimes =
    Array.init runs (fun _ ->
        pre ();
        let f_res, time_elapsed = time_fn ~f in
        let () = post f_res in
        time_elapsed)
  in
  let () = Array.sort Float.compare runtimes in
  let runtime_sum = Array.fold_left (fun acc x -> acc +. x) 0. runtimes in
  let median =
    if runs mod 2 = 0 then
      (runtimes.((runs + 1) / 2) +. runtimes.((runs - 1) / 2)) /. 2.0
    else runtimes.(runs / 2)
  in
  {
    bench_name = name;
    median_exec_time = median;
    avg_exec_time = runtime_sum /. Float.of_int runs;
    num_of_runs = runs;
    shortest_exec_time = runtimes.(0);
    longest_exec_time = runtimes.(runs - 1);
  }

let calc_delta ~old ~cur = (cur -. old) /. old *. 100.

let report ?(in' = `Ms) res =
  let name_padding =
    List.fold_left
      (fun acc x -> Int.max acc (String.length x.bench_name))
      10 res
  in
  let pad_with_space x len =
    assert (String.length x <= len);
    x ^ String.init (len - String.length x) (fun _ -> ' ')
  in
  let f = pad_with_space in
  let time_unit_s =
    match in' with `S -> "s" | `Ms -> "ms" | `Us -> "µs" | `Ns -> "ns"
  in
  let h x = Utils.from_seconds ~to':in' x in

  let g x =
    (* let y' = String.sub ( Float.to_string y ) *)
    Utils.process (Float.to_string (h x)) ^ " " ^ time_unit_s
  in
  Printf.printf
    "===============================================================================\n";
  Printf.printf
    "===============================================================================\n";
  let () =
    Printf.printf "%s %s %s %s %s %s\n" (f "Name" name_padding) (f "Median" 18)
      (f "Avg" 18) (f "Runs" 8) (f "Max time" 18) (f "Min time" 18)
  in
  List.iter
    (fun x ->
      Printf.printf "%s %s %s %s %s %s\n"
        (f x.bench_name name_padding)
        (f (g x.median_exec_time) 18)
        (f (g x.avg_exec_time) 18)
        (f (Int.to_string x.num_of_runs) 8)
        (f (g x.longest_exec_time) 18)
        (f (g x.shortest_exec_time) 18))
    res;
  let db_handle = Db.init_and_get_handle "benchmarks.sqlite3" in
  let cmp_results =
    List.map
      (fun x ->
        let prior_bench_res = Db.get_latest x.bench_name db_handle in
        Option.map
          (fun (res : Db.db_entry) ->
            Printf.sprintf
              "%s | Median delta: %f%% (%s -> %s) | Avg delta: %f%% (%s -> %s) \n"
              res.name
              (calc_delta ~old:res.median ~cur:x.median_exec_time)
              (g res.median) (g x.median_exec_time)
              (calc_delta ~old:res.avg ~cur:x.avg_exec_time)
              (g res.avg) (g x.avg_exec_time))
          prior_bench_res)
      res
  in
  if List.for_all (fun x -> Option.is_none x) cmp_results |> not then (
    Printf.printf
      "===============================================================================\n";
    List.iter (fun x -> Option.iter (fun x -> print_string x) x) cmp_results);
  List.iter (fun x -> Db.insert x db_handle) res

module Timer = struct
  include Timer
end
