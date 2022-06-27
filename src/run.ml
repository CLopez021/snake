open! Core
open! Async
open! Map

(* This is the core logic that actually runs the game. We have implemented
   enough of this for you to get started, but feel free to read this file as
   a reference because you'll end up modifying it eventually. *)
let read_keys =
  let temp = Ivar.read (Ivar.create ()) in
  let pipe_reader, pipe_writer = Pipe.create () in
  let time = Core_private.Span_float.create ?ms:(Some 1) () in
  Async_unix.every ?stop:(Some temp) time (fun () ->
      match Snake_graphics.read_key () with
      | None -> ()
      | Some key -> Pipe.write_without_pushback_if_open pipe_writer key);
  pipe_reader
;;

let handle_steps (game : Game.t) ~game_over ~ivar ~player1_name ~player2_name =
  let time = Core_private.Span_float.create ?ms:(Some 100) () in
  Async_unix.every ?stop:game_over time (fun () ->
      Game.step game;
      Snake_graphics.render game;
      match Game.game_state game with
      | Game_over _ | Win ->
        Ivar.fill ivar ();
        don't_wait_for(let%bind leaderboard = Leaderboard.leaderboard_creation game ~n:10 in
        return (print_endline leaderboard))
      | In_progress -> ())
;;

let handle_keys (game : Game.t) ~game_over ~ivar ~player1_name ~player2_name =

  let pipe_reader = read_keys in
  don't_wait_for (Pipe.iter_without_pushback pipe_reader ~f:(fun key ->
      match key with
      | 'r' ->
        let new_ivar = Ivar.create () in
        let new_game_over = Ivar.read new_ivar in
        Game.handle_key game key;
        handle_steps game ~game_over:(Some new_game_over) ~ivar:new_ivar ~player1_name:player1_name ~player2_name:player2_name;
        
      | _ ->
        Game.handle_key game key;
        Snake_graphics.render game;
      ))
;;

let run ~player1_name ~player2_name =
  let game = Snake_graphics.init_exn () in
  Snake_graphics.render game;
  let ivar = Ivar.create () in
  let game_over = Ivar.read ivar in
  handle_steps game ~game_over:(Some game_over) ~ivar ~player1_name: player1_name ~player2_name: player2_name;
  handle_keys game ~game_over:(Some game_over) ~ivar ~player1_name:player1_name ~player2_name:player2_name
;;

let command =
  Command.async
    ~summary:"Snake Game"
    (* This "%map_open.Command" does two things:

       1. It's calling [Command.map] to parse the command line args.

       2. It makes [Command.Param] available (in scope) when you define the
       args.

       Again, it's not important to fully understand the mechanics here, and
       to accept this as standard syntax for defining commandline
       commands. *)
    [%map_open.Command
      (* This first item [ let () = return () ] does nothing. We've chosen to
         keep it here in the example because it's easy to copy-paste and
         shows you the syntax for a "command with zero arguments". If you
         hate it, you can remove it. *)
      let () = return ()
      (* Here are a few examples of flags that can be defined with [Command].
         You can check out
         https://ocaml.org/p/core/v0.15.0/doc/Core/Command/Flag/index.html
         for documentation on other bells and whistles that are available. *)
      and player1_name = flag "-player1" (required string) ~doc:"STRING e.g., Brett"
      and player2_name = flag "-player2" (required string) ~doc: "STRING e.g., Brett" in
      (* Once you get down here, [Command.Param] is no longer in scope
         Thanks to [Command.map], we're now in a context where all three
         params have been neatly parsed. We can use them below like ordinary
         OCaml values. *)
      fun () ->
      let()= run ~player1_name ~player2_name in
      Deferred.never ()]
;;
(*let () = Command_unix.run command*)