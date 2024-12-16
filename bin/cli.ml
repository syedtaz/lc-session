open Core

let create =
  Command.basic
    ~summary:"Create a new session."
    ~readme:(fun () ->
      "Creates a new session with the name passed in. If a session\n\
       with that name already exists, then it is a no-op.")
    Command.Param.(map (anon ("name" %: string)) ~f:(fun name () -> Sessions.create name))
;;

let add =
  Command.basic
  ~summary:"Add a completed problem to a session."
  ~readme:(fun () -> "Add a completed problem with [id] to a session with named [name].")
  Command.Param.(map (anon))

let list =
  Command.basic
    ~summary:"List all sessions"
    ~readme:(fun () -> "Lists all sessions.")
    Command.Param.(return (fun () -> Sessions.list ()))
;;

let reset =
  let prompt () =
    let noop () = printf "nothing was reset.\n" in
    printf "are you sure you want to reset your progress? type 'yes' to confirm: ";
    Out_channel.flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> noop ()
    | Some v ->
      (match v with
       | "yes" -> Sessions.reset ()
       | _ -> noop ())
  in
  Command.basic
    ~summary:"List all sessions"
    ~readme:(fun () -> "Lists all sessions.")
    Command.Param.(return (fun () -> prompt ()))
;;

let root =
  Command.group
    ~summary:"Leetcode session tracker"
    [ "list", list; "create", create; "reset", reset ]
;;
