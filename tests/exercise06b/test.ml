open! Base
open! Snake_lib

(*let%expect_test "Exercise 06b" = Backtrace.elide := false; Random.init 42;
  let game = Game.create ~height:10 ~width:10 ~initial_snake_length:1 in let
  snake = Snake.Exercises.create_of_locations (Position.of_col_major_coords [
  1, 0; 2, 0; 3, 0 ]) in Game.Exercises.set_snake game snake; let test
  (apple_row, apple_col) = let apple = Apple.Exercises.create_with_location
  Position.{ row = apple_row; col = apple_col } in Game.Exercises.set_apple
  game apple; Game.Exercises.exercise06b game; Stdio.printf !"%{Game}\n%!"
  game in test (0, 0); [%expect {| Game state: In_progress Apple: ((location
  ((col 0) (row 0)))) Board: ((height 10) (width 10)) Snake: Head position:
  1, 0 Tail positions: [ 3, 0; 2, 0 ] Direction: Right Extensions remaining:
  0 |}]; test (0, 1); [%expect {| Game state: In_progress Apple: ((location
  ((col 3) (row 5)))) Board: ((height 10) (width 10)) Snake: Head position:
  1, 0 Tail positions: [ 3, 0; 2, 0 ] Direction: Right Extensions remaining:
  2 |}]; (* Eating another apple will further increase the number of
  extensions. *) test (0, 1); [%expect {| Game state: In_progress Apple:
  ((location ((col 9) (row 0)))) Board: ((height 10) (width 10)) Snake: Head
  position: 1, 0 Tail positions: [ 3, 0; 2, 0 ] Direction: Right Extensions
  remaining: 4 |}] ;; *)
