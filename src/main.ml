

let rec game_loop () =
  Chip8.emulate_cycle () ;

  if Chip8.draw_flag () then ();
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


(* first_bits = 6 | opcode = 6A02 *)
(* first_bits = 6 | opcode = 6B0C *)
(* first_bits = 6 | opcode = 6C3F *)
(* first_bits = 6 | opcode = 6D0C *)
(* first_bits = A | opcode = A2EA *)
(* first_bits = D | opcode = DAB6 *)
(* Draw a sprite at x = 2 y = 12 *)
