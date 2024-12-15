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

let initialize (xdg : Xdg.t) =
  let db_dir = Xdg.data_dir xdg ^ "/lcsession/db" in
  open_db db_dir
;;
