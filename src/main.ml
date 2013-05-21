open Lwt

let t = ref (Unix.gettimeofday ())
let interval = 1. /. 840. (* 840 instruction / sec *)

let rec game_loop () =
  let _ = Chip8.emulate_cycle () in

  if Chip8.draw_flag () then
    Display.display ();
  Key.check ();

  let t' = Unix.gettimeofday () in
  let d = t' -. !t in
  t:=t';
  if d > interval
  then
    game_loop ()
  else
    Lwt_unix.sleep (interval -. d) >>=
    game_loop

let _ =
  let game =
    if Array.length (Sys.argv) > 1 then Sys.argv.(1)
    else "games/PONG2"
  in

  Chip8.load_game game ;

  Lwt_main.run (game_loop ())
