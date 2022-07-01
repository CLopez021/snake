open! Core
open! Snake_lib

let command =
  Command.group
    ~summary:"simple streaming rpc client and server application"
    [ "server", Server_run.command; "client", Client_run.command ] 
;;

let () = Command_unix.run command
;;
