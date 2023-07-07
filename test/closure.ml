open Caml_bench

let f g x = g x |> ignore
let f' g x y = g x y |> ignore

let () =
  let capturing =
    run ~name:"capturing-lambda" ~runs:1000
      ~f:(fun _ ->
        for i = 0 to 10000 do
          f (fun y -> i + y) 10
        done)
      ()
  in
  let non_capturing =
    run ~name:"non-capturing-lambda" ~runs:1000
      ~f:(fun _ ->
        for i = 0 to 10000 do
          f' (fun x y -> x + y) i 10
        done)
      ()
  in
  report ~in':`Us [ capturing; non_capturing ]
