let rec game_loop () =
  Chip8.emulate_cycle () ;

  if Chip8.draw_flag () then
    Display.display ();

  Key.check ();

  game_loop ()

let _ =
  let game =
    if Array.length (Sys.argv) > 1 then Sys.argv.(1)
    else "pong"
  in

  Chip8.load_game game ;

  game_loop ()
