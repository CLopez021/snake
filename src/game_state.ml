open! Core

type t =
  | In_progress
  | Game_over of string
  | Win
[@@deriving sexp_of, compare,bin_io]

let to_string t =
  match t with
  | In_progress -> ""
  | Game_over x -> "Game over: " ^ x
  | Win -> "WIN!"
;;
