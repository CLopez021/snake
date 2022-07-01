open Core
open Async

let send_message server2_addr ~key =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port server2_addr)
    (fun connection ->
      let%map.Deferred response =
        Rpc.Rpc.dispatch
          Echo_protocol.rpc
          connection
          { user_input = key }
      in
      Core.print_s
        [%message "Received response" (response: Echo_protocol.Response.t Or_error.t)])
  >>| Result.ok_exn
;;
let read_keys ~server2_addr () =
  let temp = Ivar.read (Ivar.create ()) in
  let time = Core_private.Span_float.create ?ms:(Some 1) () in
  Async_unix.every ?stop:(Some temp) time (fun () ->
      match Snake_graphics.read_key () with
      | None -> ()
      | Some key -> don't_wait_for(send_message server2_addr ~key) 
  )
;;

let render pipe_reader =
  let () = Snake_graphics.init_exn () in
  Pipe.iter_without_pushback pipe_reader ~f:(fun game ->
      
    Core.print_s
      [%message
        "Received heartbeat"
          ~now:(Time_ns_unix.now () : Time_ns_unix.t)];
    match Game.game_state game with
    | Game_over _ 
    | Win -> ()
      (*don't_wait_for(let%bind leaderboard = Leaderboard.leaderboard_creation game ~n ~player1_name ~player2_name in
      return (print_endline leaderboard))*)
    | In_progress -> Snake_graphics.render game;())
;;

let run ~server_addr ~server2_addr r~n ~player1_name ~player2_name =     
  Rpc.Connection.with_client 
  (Tcp.Where_to_connect.of_host_and_port server_addr)
  (fun connection ->
    let%bind.Deferred pipe_reader, _metadata =
      Rpc.Pipe_rpc.dispatch_exn Protocol.Send_Game_Updates.rpc connection ()
    in
    read_keys ~server2_addr (); 
    render pipe_reader)
  >>| Result.ok_exn
  (*read_keys ()*)
;;

let request_heartbeats_command =
  Command.async
    ~summary:"send single ping to server"
    (let%map_open.Command server_addr =
       flag
         "-server"
         (required host_and_port)
         ~doc:"HOST_AND_PORT server to query (e.g. localhost:1337)"
         and server2_addr = flag "-server2" (required host_and_port) ~doc: "HOST_AND_PORT for non-streaming server"
         and player1_name = flag "-player1" (required string) ~doc: "STRING for the name of player1"
         and player2_name = flag "-player2" (required string) ~doc: "STRING for the name of player2"
         and n = flag "-n" (required int) ~doc: "INT for number of places of leaderboard to post"
     in
     fun () -> run ~server_addr ~server2_addr ~n ~player1_name ~player2_name)
;;




let command = 
  Command.group
    ~summary:"rpc client"
    [ "request-game", request_heartbeats_command ]
;;
