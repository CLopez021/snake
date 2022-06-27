open! Core
open! Async

(* This module is currently entirely unimplemented. It will be implemented in
   a future exercise. *)

module One_score = struct
  type t =
    { num_score : int
    ; user : string
    }
  [@@deriving sexp]
end

type t = One_score.t list [@@deriving sexp]

let file_path = "/home/ubuntu/snake_scores.sexp"
let load () : t Deferred.t = Reader.load_sexp_exn file_path t_of_sexp

let update ~player_name ~score ~player_name2 ~score2 () =
  let%bind t = load () in
  let temp =
    t
    @ [ { num_score = score; user = player_name }
      ; { num_score = score2; user = player_name2 }
      ]
  in
  let%bind () = Writer.save_sexp file_path (sexp_of_t temp) in
  return temp
;;

let to_table t ~n =
  let comp (current : One_score.t) (current2 : One_score.t) =
    compare current.num_score current2.num_score
  in
  let temp =List.rev(List.sort t ~compare:comp)in
  let temp = List.take temp n in
  let temp =
    List.map temp ~f:(fun current_score ->
        Core.sprintf
          "Num_score: %d; User: %s"
          current_score.num_score
          current_score.user)
  in
  let leaderboard_string = String.concat ~sep:"\n" temp in
  leaderboard_string
;;

let leaderboard_creation game ~n =
  let score, score2 = Game.score game in
  (*let player_name, player2_name = Game.player_name game in*)
  let%bind t =
    update ~player_name:"Cristian" ~score ~player_name2:"Cristia" ~score2 ()
  in
  let leaderboard_table_string = to_table t ~n in
  return leaderboard_table_string
;;
