open! Core
open! Async
open! Map

let handle_steps ~game ~writer  ~player1_name ~player2_name ~n=
  let ivar = Ivar.create () in
  let game_over = Ivar.read ivar in
  let keep_writing = ref true in
  let time = Core_private.Span_float.create ?ms:(Some 100) () in
  Async_unix.every ?stop:(Some game_over) time (fun () ->
      Game.step game;
      match Game.game_state game with
      | Game_over _ 
      | Win ->
        let () = 
        (if !keep_writing then
        Pipe.write_without_pushback_if_open writer game;
        keep_writing := false) in 
        () 
      | In_progress -> Pipe.write_without_pushback_if_open writer game)
;;

let handle_keys ~game ~key =
    match key with
      | 'r' ->
        Game.handle_key game key;
      | _ ->
        Game.handle_key game key;
        ()
;;

let run ~game ~writer ~n ~player1_name ~player2_name =
  handle_steps ~game ~writer ~player1_name ~player2_name ~n;
;;


let handle_query client () ~game =
  Core.print_s
    [%message
      "Starting to heartbeat"
        (client : Socket.Address.Inet.t)];
  let reader, writer = Pipe.create () in
  run ~game ~writer ~n:10 ~player1_name:"Cristian" ~player2_name:"Cristian2";
  return (Ok reader)
;;

let implementations ~game=
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Pipe_rpc.implement Protocol.Send_Game_Updates.rpc ((handle_query ~game); (handle_query_non_streaming ~game)) ]
;;

let serve port ~game =
  let%bind server =
    Rpc.Connection.serve
      ~implementations:(implementations ~game)
      ~initial_connection_state:(fun addr conn ->
        upon (Rpc.Connection.close_finished conn) (fun () ->
            Core.print_s
              [%message
                "Client disconnected" (addr : Socket.Address.Inet.t)]);
        addr)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  Tcp.Server.close_finished server
;;


let handle_query_non_streaming client query ~game=
  Core.print_s
    [%message
      "Received query"
        (client : Socket.Address.Inet.t)
        (query : Echo_protocol.Query.t)];
  let () = handle_keys ~game ~key:(Echo_protocol.Query.to_char query) in
  return ()
    (*{ Echo_protocol.Response.response_message =
        [%string
          "I have received your query! You said: %{query#Protocol.Query}"]
    }*)
;;
let implementations_non_streaming ~game=
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Echo_protocol.rpc (handle_query_non_streaming ~game) ]
;;

let serve_non_streaming port ~game =
  let%bind server =
    Rpc.Connection.serve
      ~implementations:(implementations_non_streaming ~game)
      ~initial_connection_state:(fun addr _conn -> addr)
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ()
  in
  Tcp.Server.close_finished server
;;

let main =
  let%map_open.Command port =
    flag
      "-port"
      (required int)
      ~doc:"INT port that the server should listen on"
  in
  fun () -> serve_non_streaming port
;;

let implementations ~game=
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Pipe_rpc.implement Protocol.Send_Game_Updates.rpc (handle_query ~game; handle_query_non_streaming ~game) ]
;;
let command =
  let game = Game.create ~height:(400/18) ~width:(450/18) ~initial_snake_length:3 in
  Command.async
    ~summary:"Snake Game"
    (* This "%map_open.Command" does two things:

       1. It's calling [Command.map] to parse the command line args.

       2. It makes [Command.Param] available (in scope) when you define the
       args.

       Again, it's not important to fully understand the mechanics here, and
       to accept this as standard syntax for defining commandline
       commands. *)
    [%map_open.Command
      (* This first item [ let () = return () ] does nothing. We've chosen to
         keep it here in the example because it's easy to copy-paste and
         shows you the syntax for a "command with zero arguments". If you
         hate it, you can remove it. *)
      let () = return ()
      (* Here are a few examples of flags that can be defined with [Command].
         You can check out
         https://ocaml.org/p/core/v0.15.0/doc/Core/Command/Flag/index.html
         for documentation on other bells and whistles that are available. *)
      and port = flag "-port" (required int) ~doc:"INT port that the server should listen on" in
      (* Once you get down here, [Command.Param] is no longer in scope
         Thanks to [Command.map], we're now in a context where all three
         params have been neatly parsed. We can use them below like ordinary
         OCaml values. *)
      fun () ->
      don't_wait_for(serve port ~game);
      serve_non_streaming (port+1) ~game ]
;;





(*let command = Command.async ~summary:"start rpc server" main
end*)



























(*let handle_keys (game : Game.t) ~key ~game_over ~ivar ~player1_name ~player2_name ~n=
      match key with
      | 'r' ->
        let new_ivar = Ivar.create () in
        let new_game_over = Ivar.read new_ivar in
        Game.handle_key game key;
        handle_steps game ~game_over:(Some new_game_over) ~ivar:new_ivar ~player1_name ~player2_name ~n;
      | _ ->
        Game.handle_key game key;
        Snake_graphics.render game;
        ()
;;*)