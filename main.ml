

let rec game_loop () =
  Chip8.emulate_cycle () ;

  if !Chip8.draw_flag () then ();
  (* draw graphics *)

  Chip8.set_keys () ;

  game_loop ()


let _ =
  let game =
    if Array.length (Sys.argv) > 1 then Sys.argv.(1)
    else "pong"
  in

  Chip8.load_game game ;

  game_loop ()
