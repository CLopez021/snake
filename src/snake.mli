open! Core

(** A [t] represents a snake on the board.

    Snake is a mutable type, so when we make changes to it, the functions
    return unit. They affect the internal state of the snake, rather than
    making a new snake. *)
type t [@@deriving sexp_of]

(** Used for pretty-printing snake contents for tests. *)
val to_string : ?indent:int -> t -> string

(** [create] makes a new snake with the given length. The length must be
    positive.

    The snake will initially be occupy the (column, row) locations: (0,0),
    (1,0), (2,0), ..., (length - 1, 0)

    The head will be at position (length - 1, 0) and the initial direction
    should be towards the right. *)
val create : length:int -> t * t

(** [grow_over_next_steps t n] tells the snake to grow by [n] in length over
    the next [n] steps. *)
val grow_over_next_steps : t -> int -> unit

(** [tail] returns the current locations that the snake's body occupies,
    excluding the head. The first element of the list is the tail of the
    snake. [tail] can be empty *)
val tail : t -> Position.t list

(** [head] returns the location of the snake's head. *)
val head : t -> Position.t

(** [all_locations] returns the current locations that the snake occupies *)
val all_locations : t -> Position.t list

(** [set_direction] tells the snake to move in a specific direction the next
    time [step] is called. *)
val set_direction : t -> Direction.t -> unit

(** [direction] returns the current direction the snake is facing *)
val direction : t -> Direction.t

(** [step] moves the snake forward by 1. [step] returns false if the snake
    collided with itself. *)
val step : t -> bool

(** Functions in [Exercises] modules shouldn't be used. They are only exposed
    so they can be tested *)
module Exercises : sig
  val create_of_locations : Position.t list -> t
  val set_head : t -> Position.t -> unit
  val set_tail : t -> Position.t list -> unit

  val exercise01
    :  Position.t
    -> Position.t list
    -> Direction.t
    -> Position.t * Position.t list

  val exercise04a : t -> bool
  val exercise06a : t -> int -> unit
  val exercise06c : t -> Position.t * Position.t list
  val exercise06c2 : t -> bool
end
