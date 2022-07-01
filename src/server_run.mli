open! Core
open! Async
open! Map
val run :game:Game.t ->writer:Game.t Pipe.Writer.t ->n:int ->player1_name:string -> player2_name:string -> unit
val command:Command.t 