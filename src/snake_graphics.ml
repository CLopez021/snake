open! Core

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let head_color = Graphics.rgb 100 100 125
  let red = Graphics.rgb 255 000 000
  let game_in_progress = Graphics.rgb 100 100 200
  let game_lost = Graphics.rgb 200 100 100
  let game_won = Graphics.rgb 100 200 100
  let gold = Graphics.rgb 255 215 0
  let blue = Graphics.rgb 0 0 255
  let head2_color = Graphics.rgb 255 105 180

  let apple_color apple =
    match Apple.color apple with Red -> red | Gold -> gold
  ;;
end

module Constants = struct
  let play_area_height = 400
  let header_height = 50
  let play_area_width = 450
  let block_size = 18
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (play_area_height + header_height)
       play_area_width);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  Game.create ~height ~width ~initial_snake_length:3
;;

let draw_block { Position.row; col } ~color =
  let open Constants in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color color;
  Graphics.fill_rect (col + 1) (row + 1) (block_size - 1) (block_size - 1)
;;

let draw_header ~game_state ~score ~score2 =
  let open Constants in
  let header_color =
    match (game_state : Game_state.t) with
    | In_progress -> Colors.game_in_progress
    | Game_over _ -> Colors.game_lost
    | Win -> Colors.game_won
  in
  Graphics.set_color header_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  let header_text = Game_state.to_string game_state in
  Graphics.set_color Colors.black;
  Graphics.set_text_size 20;
  Graphics.moveto 0 (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf " %s" header_text);
  Graphics.moveto (play_area_width - 75) (play_area_height + 25);
  Graphics.draw_string (Printf.sprintf "Green: %d" score);
  Graphics.moveto (play_area_width - 75) (play_area_height + 10);
  Graphics.draw_string (Printf.sprintf "Blue: %d" score2)
;;

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

let draw_apple apple =
  let apple_location = Apple.location apple in
  draw_block apple_location ~color:(Colors.apple_color apple)
;;

let draw_snake snake_head snake_tail =
  List.iter snake_tail ~f:(draw_block ~color:Colors.green);
  (* Snake head is a different color *)
  draw_block ~color:Colors.head_color snake_head
;;

let draw_snake2 snake2_head snake2_tail =
  List.iter snake2_tail ~f:(draw_block ~color:Colors.blue);
  (* Snake head is a different color *)
  draw_block ~color:Colors.head2_color snake2_head
;;

let render game =
  Graphics.display_mode false;
  let snake = Game.snake game in
  let snake2 = Game.snake2 game in
  let apple = Game.apple game in
  let game_state = Game.game_state game in
  let score, score2 = Game.score game in
  draw_header ~game_state ~score ~score2;
  draw_play_area ();
  draw_apple apple;
  draw_snake (Snake.head snake) (Snake.tail snake);
  draw_snake2 (Snake.head snake2) (Snake.tail snake2);
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;
