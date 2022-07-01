open Core
open Async
module Send_Game_Updates = 
struct
  module Query = struct
    type t = unit
    [@@deriving sexp_of, bin_io]
  end


  module Response = struct
    type t = Game.t
    [@@deriving sexp_of, bin_io]
  end
  let rpc =
    Rpc.Pipe_rpc.create
      ~name:"Send Game Updates"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
      ~bin_error:Nothing.bin_t
      ()
  ;;
end