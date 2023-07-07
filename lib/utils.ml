let time_unit_to_s u =
  match u with `S -> "s" | `Ms -> "ms" | `Us -> "µs" | `Ns -> "ns"

let string_to_time_unit s =
  match s with
  | "s" -> `S
  | "ms" -> `Ms
  | "µs" -> `Us
  | "ns" -> `Ns
  | _ -> failwith "Impossible"

let from_seconds ~to' x =
  match to' with
  | `S -> x
  | `Ms -> x *. 1000.
  | `Us -> x *. 1000. *. 1000.
  | `Ns -> x *. 1000. *. 1000. *. 1000.

let to_seconds ~from' x =
  match from' with
  | `S -> x
  | `Ms -> x /. 1000.
  | `Us -> x /. (1000. *. 1000.)
  | `Ns -> x /. (1000. *. 1000. *. 1000.)

let process s =
  let first_n_after_nonzero n s =
    let non_zero_at = ref (-1) in
    let () =
      String.iteri
        (fun i c -> if c <> '0' && !non_zero_at = -1 then non_zero_at := i)
        s
    in
    String.sub s 0 (min (!non_zero_at + n) (String.length s))
  in

  let l = String.split_on_char '.' s in
  match l with
  | [ x ] -> x
  | [ before; after ] -> before ^ "." ^ first_n_after_nonzero 4 after
  | _ -> failwith "Impossible"
