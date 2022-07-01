open Core
open Async
module Send_Game_Updates : sig
    module Query : sig
      type t = unit
      [@@deriving sexp_of]
    end
  
    module Response : sig
      type t = Game.t
      [@@deriving sexp_of, bin_io]
    end
  
    (* Unlike the echo server's [rpc], which is of type [Rpc.Rpc.t], this RPC
       is a [Rpc.Pipe_rpc.t]. For the echo server, a single query from the
       client will yield a single response from the server. With pipe RPC, the
       heartbeat server will be able to stream events to the client. *)
    val rpc : (Query.t, Response.t, Nothing.t) Rpc.Pipe_rpc.t
  end