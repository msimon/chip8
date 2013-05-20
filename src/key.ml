open Sdlevent
open Sdlkey
module M = Mem_req

(*
   Chip 8 has a HEX based keypad (0x0-0xF), you can use an array to store the current state of the key

   Mapping :
   0x0 -> x
   0x1 -> 1
   0x2 -> 2
   0x3 -> 3
   0x4 -> q
   0x5 -> w
   0x6 -> e
   0x7 -> a
   0x8 -> s
   0x9 -> d
   0xa -> z
   0xb -> c
   0xc -> 4
   0xd -> r
   0xe -> f
   0xf -> v

   Chip8 Keypad             Keyboard
   +-+-+-+-+                +-+-+-+-+
   |1|2|3|C|                |1|2|3|4|
   +-+-+-+-+                +-+-+-+-+
   |4|5|6|D|                |Q|W|E|R|
   +-+-+-+-+      >>>       +-+-+-+-+
   |7|8|9|E|                |A|S|D|F|
   +-+-+-+-+                +-+-+-+-+
   |A|0|B|F|                |Z|X|C|V|
   +-+-+-+-+                +-+-+-+-+

*)

let quit () =
  Sdl.quit (); exit 0

let handle_key k =
  let key k n =
    let v = match k.ke_state with
      | PRESSED -> 1
      | RELEASED -> 0
    in

    M.key.(n) <- v
  in

  match k.keysym with
    | KEY_ESCAPE -> quit ()

    | KEY_x -> key k 0x0

    | KEY_1 -> key k 0x1
    | KEY_2 -> key k 0x2
    | KEY_3 -> key k 0x3
    | KEY_q -> key k 0x4
    | KEY_w -> key k 0x5
    | KEY_e -> key k 0x6
    | KEY_a -> key k 0x7
    | KEY_s -> key k 0x8
    | KEY_d -> key k 0x9
    | KEY_z -> key k 0xa
    | KEY_c -> key k 0xb

    | KEY_4 -> key k 0xc
    | KEY_r -> key k 0xd
    | KEY_f -> key k 0xe
    | KEY_v -> key k 0xf
    | _ ->  ()

let check () =
  match Sdlevent.poll () with
    | Some e ->
      begin
        match e with
          | KEYDOWN k | KEYUP k -> handle_key k
          | QUIT -> quit ()
          | _ -> ()
      end
    | _ -> ()
