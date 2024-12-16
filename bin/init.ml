let create_xdg () =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> Xdg.create ~win32:true ~env:Sys.getenv_opt ()
  | "Unix" -> Xdg.create ~win32:false ~env:Sys.getenv_opt ()
  | _ -> failwith "unknown o/s"
;;

let open_db (path : string) =
  let parent = Filename.dirname path in
  let () =
    match Sys.file_exists parent with
    | true -> ()
    | false -> Sys.mkdir parent 0o755
  in
  Sqlite3.db_open path
;;

let check_default_tables (db : Sqlite3.db) =
  let rc =
    (* Use the tbl module*)
    Sqlite3.exec
      db
      "CREATE TABLE IF NOT EXISTS sessions_tbl (id INTEGER PRIMARY KEY AUTOINCREMENT, \
       name TEXT NOT NULL UNIQUE); CREATE TABLE IF NOT EXISTS records_tbl (problem \
       INTEGER NOT NULL, session INTEGER NOT NULL, PRIMARY KEY (problem, session) ON \
       CONFLICT IGNORE)"
  in
  match Sqlite3.Rc.is_success rc with
  | true -> db
  | false -> failwith "could not create sessions table"
;;

let initialize () =
  let xdg = create_xdg () in
  let db_dir = Xdg.data_dir xdg ^ "/lcsession/db" in
  let db = open_db db_dir in
  check_default_tables db
;;
