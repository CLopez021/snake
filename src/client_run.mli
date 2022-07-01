open Async
open Core
val read_keys:server_addr:Host_and_port.t -> unit -> unit
  
val render: Game.t Pipe.Reader.t -> unit Deferred.t

val run: server_addr:Host_and_port.t -> server2_addr: Host_and_port.t-> n:int -> player1_name:string-> player2_name:string -> unit Deferred.t

val command: Command.t