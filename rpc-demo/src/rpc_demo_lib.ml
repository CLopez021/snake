open! Core
open Async

let command =
  Command.group
    ~summary:"rpc demo"
    [ "echo", Echo_server.command; "heartbeat", Heartbeat_server.command ]
;;
