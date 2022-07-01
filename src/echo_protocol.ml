open Core
open Async
  module Query = struct
    type t = {user_input : char}
    [@@deriving sexp_of, bin_io]
  let to_char{ user_input } = user_input
  end
  module Response = struct
    type t = unit 
    [@@deriving sexp_of, bin_io]
  end
  let rpc =
    Rpc.Rpc.create
      ~name:"send-message"
      ~version:0
      ~bin_query:Query.bin_t
      ~bin_response:Response.bin_t
  ;;
