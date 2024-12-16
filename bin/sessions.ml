(** Create, delete and access metadata about different sessions. *)

type db = Sqlite3.db

let create (name : string) =
  let db = Init.initialize () in
  let s = "INSERT INTO sessions_tbl (name) VALUES (?)" in
  let open Sqlite3 in
  let stm = prepare db s in
  let stm' = bind_text stm 1 name in
  match Rc.is_success stm' with
  | true ->
    (match step stm with
     | Rc.DONE -> ()
     | _ -> failwith "could not create session")
  | false -> failwith "could not create session"
;;

let list () =
  let db = Init.initialize () in
  let s = "SELECT name FROM sessions_tbl" in
  let sessions = ref [] in
  let cb x _ =
    Array.iter
      (fun x ->
        match x with
        | Some v -> sessions := v :: !sessions
        | None -> ())
      x
  in
  let res = Sqlite3.exec db ~cb s in
  match Sqlite3.Rc.is_success res with
  | true -> List.iter (fun x -> Format.printf "%s\n" x) !sessions
  | false -> failwith "could not list table"
;;

let reset () =
  let db = Init.initialize () in
  let rc =
    Sqlite3.exec
      db
      "DELETE FROM sessions_tbl; DELETE FROM SQLITE_SEQUENCE WHERE name='sessions_tbl';"
  in
  match Sqlite3.Rc.is_success rc with
  | true -> Format.printf "successfully reset\n"
  | false -> failwith "could not truncate table"
;;
