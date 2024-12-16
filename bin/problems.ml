let rec iter_row (sref : int64 option ref) (stm : Sqlite3.stmt) =
  let open Sqlite3 in
  match step stm with
  | Rc.DONE -> !sref
  | Rc.ROW ->
    (match column stm 1 with
     | Data.INT i ->
       sref := Some i;
       iter_row sref stm
     | _ -> failwith "not correct header")
  | _ -> failwith "could not add problem"
;;

let is_succ_or a b ~msg =
  match Sqlite3.Rc.is_success a with
  | true -> b ()
  | false -> failwith msg
;;

let insert_problem (id : int) (sid : int64) (db : Sqlite3.db) =
  let open Sqlite3 in
  let s = "INSERT INTO records_tbl (problem, session) VALUES (?, ?)" in
  let stm = prepare db s in
  let stm' = bind_int stm 1 id in
  is_succ_or ~msg:"could not add problem" stm' (fun () ->
    let stm'' = bind_int64 stm 2 sid in
    is_succ_or ~msg:"could not add problem" stm'' (fun () ->
      match step stm with
      | Rc.DONE -> ()
      | _ -> failwith "could not add problem"))
;;

let get_session_id (session : string) (db : Sqlite3.db) =
  let open Sqlite3 in
  let s = "select id from sessions_tbl where name = ?;" in
  let stm = prepare db s in
  let stm' = bind_text stm 1 session in
  match Rc.is_success stm' with
  | false -> failwith "could not find session id"
  | true -> iter_row (ref None) stm
;;

let add (id : int) (session : string) =
  let open Sqlite3 in
  let db = Init.initialize () in
  let sid_opt = get_session_id session db in
  match sid_opt with
  | None -> failwith "could not find session id"
  | Some sid -> insert_problem id sid db
;;
