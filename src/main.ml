open Lwt

let t = ref 0.
let rate = 1. /. 840. (* 840 instruction / sec *)

let rec game_loop late =
  Chip8.emulate_cycle ();

  if Chip8.draw_flag () then
    Display.display ();

  Key.check ();

  let t' = Unix.gettimeofday () in
  let d = t' -. !t in
  t:=t';

  let late = rate -. d +. late in

  if late < 0. then
    game_loop late
  else
    game_loop 0.

let _ =
  let game =
    if Array.length (Sys.argv) > 1 then Sys.argv.(1)
    else "games/PONG2"
  in

  Chip8.load_game game ;

  Lwt_main.run (t := (Unix.gettimeofday ()); Chip8.timer_t := Unix.gettimeofday (); game_loop 0.)
