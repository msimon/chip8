open Lwt

let t = ref (Unix.gettimeofday ())
let interval = 0.01 (* 60 Hz *)

let rec game_loop () =
  let op = Chip8.emulate_cycle () in

  if Chip8.draw_flag () then
    Display.display ();
  Key.check ();

  let t' = Unix.gettimeofday () in
  let d = t' -. !t in
  t:=t';
  if d > interval
  then (
    Printf.printf "too late (%f) -> opcode %X\n" d op;
    game_loop ()
  )
  else
    let s = (interval -. d ) in
    (* Printf.printf "sleep %f\r                           " s; *)
    Lwt_unix.sleep s >>=
    game_loop

let _ =
  let game =
    if Array.length (Sys.argv) > 1 then Sys.argv.(1)
    else "games/PONG2"
  in

  Chip8.load_game game ;

  Lwt_main.run (game_loop ())
