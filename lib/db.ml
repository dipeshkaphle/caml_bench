type t = Sqlite3.db
type db_entry = { name : string; median : float; avg : float }

let table_name = "benches"

let to_db_entry row headers =
  assert (headers.(0) = "name");
  assert (headers.(1) = "median");
  assert (headers.(2) = "avg");
  {
    name = row.(0);
    median = Float.of_string row.(1);
    avg = Float.of_string row.(2);
  }

let init_and_get_handle file =
  let db = Sqlite3.db_open file in
  assert (
    Sqlite3.exec db
      "CREATE TABLE IF NOT EXISTS benches (name varchar(255) not null , median \
       float not null, avg float not null, inserted_at float not null)"
    |> Sqlite3.Rc.is_success);
  db

let close t = assert (Sqlite3.db_close t)

let insert (result : Types.bench_result) (t : t) =
  let sql =
    Printf.sprintf "INSERT into %s VALUES ('%s', %f, %f, %f)" table_name
      result.bench_name result.median_exec_time result.avg_exec_time
      (Unix.time ())
  in
  assert (Sqlite3.exec t sql |> Sqlite3.Rc.is_success)

let get_latest name t =
  let res = ref None in
  let sql =
    Printf.sprintf
      "select name,median,avg from %s where name='%s' order by inserted_at \
       desc limit 1"
      table_name name
  in
  assert (
    Sqlite3.exec_not_null
      ~cb:(fun row headers -> res := Some (to_db_entry row headers))
      t sql
    |> Sqlite3.Rc.is_success);
  !res
