let gfx_width = 64
let gfx_height = 32
let sprite_width = 8

(* 0x000-0x1FF - Chip 8 interpreter (contains font set in emu) *)
(* 0x050-0x0A0 - Used for the built in 4x5 pixel font set (0-F) *)
(* 0x200-0xFFF - Program ROM and work RAM *)

(* total memory *)
let memory = String.make 4096 '\000'
(* register *)
let reg = String.make 16 '\000'

(* index register *)
let i = ref 0
(* program counter *)
let pc = ref 0

(* let gfx = String.make (gfx_width * gfx_height) '\000' *)

let gfx = Array.make_matrix gfx_height gfx_width 0

(* Some timer *)
let delay_timer = ref 0
let sound_timer = ref 0

let stack : int list ref = ref []

let key = Array.make 16 0

(* only the first 4 byte are used *)
let chip8_fontset =
  [|
    0xF0; 0x90; 0x90; 0x90; 0xF0; (* 0 *)
    0x20; 0x60; 0x20; 0x20; 0x70; (* 1 *)
    0xF0; 0x10; 0xF0; 0x80; 0xF0; (* 2 *)
    0xF0; 0x10; 0xF0; 0x10; 0xF0; (* 3 *)
    0x90; 0x90; 0xF0; 0x10; 0x10; (* 4 *)
    0xF0; 0x80; 0xF0; 0x10; 0xF0; (* 5 *)
    0xF0; 0x80; 0xF0; 0x90; 0xF0; (* 6 *)
    0xF0; 0x10; 0x20; 0x40; 0x40; (* 7 *)
    0xF0; 0x90; 0xF0; 0x90; 0xF0; (* 8 *)
    0xF0; 0x90; 0xF0; 0x10; 0xF0; (* 9 *)
    0xF0; 0x90; 0xF0; 0x90; 0x90; (* A *)
    0xE0; 0x90; 0xE0; 0x90; 0xE0; (* B *)
    0xF0; 0x80; 0x80; 0x80; 0xF0; (* C *)
    0xE0; 0x90; 0x90; 0x90; 0xE0; (* D *)
    0xF0; 0x80; 0xF0; 0x80; 0xF0; (* E *)
    0xF0; 0x80; 0xF0; 0x80; 0x80  (* F *)
  |]


(*
   program counter start at 0x200
   reset other value
*)
let clear_screen () =
  let clear_matrix m =
    let leny = Array.length m in
    let lenx = Array.length m.(0) in
    let rec clear n =
      Array.fill m.(n) 0 (lenx - 1) 0 ;
      if n = leny - 1 then ()
      else clear (n + 1)
    in

    clear 0
  in

  clear_matrix gfx

let initialized () =
  let clear s =
    String.fill s 0 (String.length s) '\000'
  in
  let clear_array a =
    Array.fill a 0 ((Array.length a) - 1) 0 ;
  in


  pc := 0x200 ;
  i := 0 ;

  clear memory ;
  clear reg ;
  clear_array key ;
  clear_screen ();

  stack := [];

  delay_timer := 0 ;
  sound_timer := 0 ;

  Array.iteri (
    fun i c ->
      String.set memory i (char_of_int c)
  ) chip8_fontset;

  Random.self_init ()
