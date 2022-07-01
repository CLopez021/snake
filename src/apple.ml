open! Core

module Color = struct
  type t =
    | Red
    | Gold
  [@@deriving sexp_of, enumerate, compare,bin_io]
end

type t =
  { location : Position.t
  ; color : Color.t
  }
[@@deriving sexp_of,bin_io]

let location t = t.location
let color t = t.color

let amount_to_grow t =
  print_endline "ammount to grow";
  match color t with Red -> 2 | Gold -> 10
;;

let return_score t =
  let color = t.color in
  match color with Red -> 2 | Gold -> 10
;;

(* Exercise 05:

   [create] takes in a board and a snake and creates a new apple with a
   location chosen randomly from all possible locations inside the board that
   are not currently occupied by the snake. If there is no location possible
   it should return [None]. (This will happen if the player wins!)

   We have used records before, but hard-coding the apple's location is the
   first time we'll make a new one. We can define a record like so:

   {[ { field_name1 = value1 ; field_name2 = value2 } ]}

   As it happens, we may not need to do this any more once we stop
   hard-coding it.

   We've already written the [create] logic; take a look at the function to
   make sure you understand what it's doing.

   Our implementation of [create] calls [possible_apple_locations], which
   you'll need to implement. It should return the locations on the board
   where an apple can be generated (in any order).

   One function you might find handy for this exercise is
   [Board.all_locations], which is defined in board.ml. [Board.all_locations]
   takes a board and returns a list of all positions on the board.

   Another function that may be useful is [Snake.all_locations], which is
   defined in snake.ml. This function takes a snake and returns all locations
   it currently occupies.

   Before you get started, check out LIST_FUNCTIONS.mkd for some useful new
   List functions. You probably won't need all of these functions, but they
   should give you a few options for how to write [possible_apple_locations].

   You may feel like your solution to this function is inefficient. That's
   perfectly fine, since the board is quite small. Talk to a TA if you'd like
   to learn other tools that might help make this function more efficient.

   When you're done writing [possible_apple_locations], make sure to check
   that tests pass by running

   $ dune runtest tests/exercise05 *)
let possible_apple_locations ~board ~snake =
  let boardlocations = Board.all_locations board in
  let snakepositions = Snake.all_locations snake in
  List.filter boardlocations ~f:(fun boardpos ->
      not (List.mem snakepositions boardpos ~equal:Position.equal))
;;

let create ~board ~snake =
  let possible_locations = possible_apple_locations ~board ~snake in
  match List.random_element possible_locations with
  | None -> None
  | Some location ->
    let rand = Random.int 3 in
    let color = match rand with 0 -> Color.Gold | _ -> Color.Red in
    Some { location; color }
;;

module Exercises = struct
  let exercise05 = possible_apple_locations
  let create_with_location location color = { location; color }
end
