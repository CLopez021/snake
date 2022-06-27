(** This module implements a leaderboard for the game. It saves scores to a
    file on the local disk. *)
open! Core

open Async

(** The type [t] here is abstract, but it should be thought of as holding the
    state about the players and their scores on the leaderboard. *)
type t [@@deriving sexp_of]

(** The [update] function accepts the name of a player (optionally) and the
    score. It should be called after each game ends to update the scores
    file. It asynchronously updates the leaderboard to use the provided
    score, and then returns the newly updated state of the
    leaderboard/scores. *)
val update
  :  player_name:string
  -> score:int
  -> player_name2:string
  -> score2:int
  -> unit
  -> t Deferred.t

(** The [to_table] function renders the scores for the top n players as a
    string. *)
val to_table : t -> n:int -> string

val leaderboard_creation : Game.t -> n:int -> string Deferred.t
