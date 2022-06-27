(** This module contains an implementation of a heartbeating server. The
    client can ask the server to send heartbeats at a given frequency.

    [command] implements command-line commands for starting up servers and
    clients. *)
val command : Async.Command.t
