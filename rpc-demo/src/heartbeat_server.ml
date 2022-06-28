open Core
open Async

module Protocol : sig
  module Query : sig
    type t = { frequency_of_heartbeats : Time_ns_unix.Span.t }
    [@@deriving sexp_of]
  end

  module Response = Unit

  (* Unlike the echo server's [rpc], which is of type [Rpc.Rpc.t], this RPC
     is a [Rpc.Pipe_rpc.t]. For the echo server, a single query from the
     client will yield a single response from the server. With pipe RPC, the
     heartbeat server will be able to stream events to the client. *)
  val rpc : (Query.t, Response.t, Nothing.t) Rpc.Pipe_rpc.t
end = struct
  module Query = struct
    type t = { frequency_of_heartbeats : Time_ns_unix.Span.t }
    [@@deriving sexp_of, bin_io]
  end

  module Response = Unit

  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"start-heartbeating"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~bin_error:Nothing.bin_t
      ()
  ;;
end

module Server : sig
  val command : Command.t
end = struct
  (* In the implementation for the heartbeat RPC, we need to construct a pipe
     reader to return to the client. In contrast, the implementation for the
     echo server returned a single value. *)
  let handle_query client { Protocol.Query.frequency_of_heartbeats } =
    Core.print_s
      [%message
        "Starting to heartbeat"
          (client : Socket.Address.Inet.t)
          (frequency_of_heartbeats : Time_ns_unix.Span.t)];
    let reader, writer = Pipe.create () in
    Clock_ns.every frequency_of_heartbeats (fun () ->
        Pipe.write_without_pushback_if_open writer ());
    return (Ok reader)
  ;;

  let implementations =
    Rpc.Implementations.create_exn
      ~on_unknown_rpc:`Close_connection
      ~implementations:[ Rpc.Pipe_rpc.implement Protocol.rpc handle_query ]
  ;;

  let serve port =
    let%bind server =
      Rpc.Connection.serve
        ~implementations
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

  let main =
    let%map_open.Command port =
      flag
        "-port"
        (required int)
        ~doc:"INT port that the server should listen on"
    in
    fun () -> serve port
  ;;

  let command = Command.async ~summary:"start rpc server" main
end

module Client : sig
  val command : Command.t
end = struct
  (* In the client, rather than getting back a single response from the
     server, we get back a pipe of responses. Here, we iterate over the pipe
     to print out every new update we get from the server. *)
  let request_heartbeats server_addr ~frequency_of_heartbeats =
    Rpc.Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port server_addr)
      (fun connection ->
        let%bind.Deferred pipe_reader, _metadata =
          Rpc.Pipe_rpc.dispatch_exn
            Protocol.rpc
            connection
            { frequency_of_heartbeats }
        in
        Pipe.iter_without_pushback pipe_reader ~f:(fun () ->
            Core.print_s
              [%message
                "Received heartbeat"
                  ~now:(Time_ns_unix.now () : Time_ns_unix.t)]))
    >>| Result.ok_exn
  ;;

  let request_heartbeats_command =
    Command.async
      ~summary:"send single ping to server"
      (let%map_open.Command server_addr =
         flag
           "-server"
           (required host_and_port)
           ~doc:"HOST_AND_PORT server to query (e.g. localhost:1337)"
       and frequency_of_heartbeats =
         flag
           "-frequency"
           (required Time_ns_unix.Span.arg_type)
           ~doc:"TIME_SPAN frequency to ask for heartbeats"
       in
       fun () -> request_heartbeats server_addr ~frequency_of_heartbeats)
  ;;

  let command =
    Command.group
      ~summary:"rpc client"
      [ "request-heartbeats", request_heartbeats_command ]
  ;;
end

let command =
  Command.group
    ~summary:"simple streaming rpc client and server application"
    [ "server", Server.command; "client", Client.command ]
;;
