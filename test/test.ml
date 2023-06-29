open Caml_bench

let () =
  let bench =
    run ~name:"test"
      ~f:(fun t ->
        Unix.sleep 1;
        Timer.pause t;
        Unix.sleep 1;
        Timer.continue t;
        ())
      ()
  in
  report [ bench ]
