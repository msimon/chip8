module M = Mem_req

exception Unknow_opcode of int
exception Empty_stack of int

let draw_cnt = ref 0

let gfx_width = 64
let gfx_height = 32
let sprite_width = 8

let load_game game =
  M.initialized () ;
  Display.init () ;

  let in_chan =
    try open_in game
    with Sys_error _ ->
      failwith (Printf.sprintf "Couldn't find game %s" game)
  in

  let rec load i =
    try
      String.set M.memory i (input_char in_chan) ;
      load (i + 1)
    with End_of_file -> ()
  in

  load 0x200

let emulate_cycle () =
  let fetch_opcode () =
    let fc = (int_of_char M.memory.[!M.pc]) lsl 8 in
    let lc = int_of_char M.memory.[!M.pc + 1] in

    fc lxor lc
  in
  let incr_pc ?(skip=false) () =
    if skip then M.pc := !M.pc + 4
    else M.pc := !M.pc + 2
  in

  let decode opcode =
    let first_bits = Byte.get_bits 4 opcode in

    (* Printf.printf "f: %X | opcode: %X\n%!" first_bits opcode ; *)

    match first_bits with
      | 0 ->
        let last_bits = Byte.get_bits ~len:2 2 opcode in
        if last_bits = 0xe0 then begin (* 00E0: clear the screen *)
          M.clear_screen ();
          incr(draw_cnt) ;
          incr_pc ();
        end else if last_bits = 0xee then begin (* 00EE: Returns from a subroutine *)
          match !M.stack with
            | [] -> raise (Empty_stack opcode)
            | pc::stack ->
              M.stack := stack ;
              M.pc := pc
        end else raise (Unknow_opcode opcode)

      | 1 -> (* 1NNN: Jumps to address NNN *)
        let last_bits = Byte.get_bits ~len:3 3 opcode in
        M.pc := last_bits;

      | 2 -> (* 2NNN: Calls subroutine at NNN *)
        let last_bits = Byte.get_bits ~len:3 3 opcode in
        M.stack := (!M.pc + 2)::!M.stack ;
        M.pc := last_bits

      | 3 -> (* 3XNN: Skips the next instruction if VX equals NN *)
        let vx = Byte.get_bits 3 opcode in
        let n = Byte.get_bits ~len:2 2 opcode in
        if (int_of_char M.reg.[vx]) = n then incr_pc ~skip:true ()
        else incr_pc ()

      | 4 -> (* 4XNN: Skips the next instruction if VX doesn't equal NN *)
        let vx = Byte.get_bits 3 opcode in
        let n = Byte.get_bits ~len:2 2 opcode in
        if (int_of_char M.reg.[vx]) <> n then incr_pc ~skip:true ()
        else incr_pc ()

      | 5 -> (* 5XY0: Skips the next instruction if VX equals VY *)
        let vx = Byte.get_bits 3 opcode in
        let vy = Byte.get_bits 2 opcode in
        if M.reg.[vx] = M.reg.[vy] then incr_pc ~skip:true ()
        else incr_pc ()

      | 6 -> (* 6XNN: Sets VX to NN *)
        let vx = Byte.get_bits 3 opcode in
        let n = Byte.get_bits ~len:2 2 opcode in
        M.reg.[vx] <- char_of_int n;
        incr_pc ()

      | 7 -> (* 7XNN: Adds NN to VX *)
        let vx = Byte.get_bits 3 opcode in
        let n = Byte.get_bits ~len:2 2 opcode in
        let x = int_of_char M.reg.[vx] in
        let add = (x + n) land 0xFF (* only take the last 8 bits *) in
        M.reg.[vx] <- char_of_int add;
        incr_pc ()

      | 8 -> (* 8XYZ *)
        let vx = Byte.get_bits 3 opcode in
        let vy = Byte.get_bits 2 opcode in
        let z = Byte.get_bits 1 opcode in

        let x = int_of_char M.reg.[vx] in
        let y = int_of_char M.reg.[vy] in

        begin match z with
          | 0 -> (* 8XY0: Sets VX to the value of VY *)
            M.reg.[vx] <- M.reg.[vy]

          | 1 -> (* 8XY1:	Sets VX to VX or VY *)
            M.reg.[vx] <- char_of_int (x lor y)

          | 2 -> (* 8XY2: Sets VX to VX and VY *)
            M.reg.[vx] <- char_of_int (x land y)

          | 3 -> (* 8XY3: Sets VX to VX xor VY *)
            M.reg.[vx] <- char_of_int (x lxor y)

          | 4 -> (* 8XY4: Adds VY to VX. VF is set to 1 when there's a carry, and to 0 when there isn't *)
            if x > (0xFF - y) then M.reg.[0xF] <- '\001'
            else M.reg.[0xF] <- '\000';

            M.reg.[vx] <- char_of_int ((x + y) land 0xFF)

          | 5 -> (* 8XY5:	VY is subtracted from VX. VF is set to 0 when there's a borrow, and 1 when there isn't *)
            if x < y then M.reg.[0xF] <- '\000'
            else M.reg.[0xF] <- '\001';

            M.reg.[vx] <- char_of_int ((x - y) land 0xFF)

          | 6 -> (* 8XY6: Shifts VX right by one. VF is set to the value of the least significant bit of VX before the shift *)
            M.reg.[0xF] <- char_of_int (x land 0b1);
            M.reg.[vx] <- char_of_int (x lsr 1)

          | 7 -> (* 8XY7:	Sets VX to VY minus VX. VF is set to 0 when there's a borrow, and 1 when there isn't *)
            if y < x then M.reg.[0xF] <- '\000'
            else M.reg.[0xF] <- '\001';

            M.reg.[vx] <- char_of_int ((y - x) land 0xFF)

          | 0xe -> (* 8XYE:	Shifts VX left by one. VF is set to the value of the most significant bit of VX before the shift *)
            M.reg.[0xF] <- char_of_int ((x lsr 7) land 0b1);
            M.reg.[vx] <- char_of_int ((x lsl 1) land 0xFF)

          | _ -> raise (Unknow_opcode opcode)
        end;
        incr_pc ()

      | 9 -> (* 9XY0:	Skips the next instruction if VX doesn't equal VY *)
        let vx = Byte.get_bits 3 opcode in
        let vy = Byte.get_bits 2 opcode in

        if M.reg.[vx] <> M.reg.[vy] then incr_pc ~skip:true ()
        else incr_pc ()

      | 0xa -> (* ANNN: Sets I to the address NNN *)
        let n = Byte.get_bits ~len:3 3 opcode in
        M.i := n ;
        incr_pc ()

      | 0xb -> (* BNNN: Jumps to the address NNN plus V0 *)
        let last_bits = Byte.get_bits ~len:3 3 opcode in
        let x = int_of_char (M.reg.[0]) in
        M.pc := (last_bits + x) land 0xFFFF

      | 0xc -> (* CXNN:	Sets VX to a random number and NN *)
        let vx = Byte.get_bits 3 opcode in
        let n = Byte.get_bits ~len:2 2 opcode in
        let r = Random.int 256 in

        M.reg.[vx] <- char_of_int (r land n);
        incr_pc ()

      | 0xd -> (* DXYN: Draws a sprite at coordinate (VX, VY) that has a width of 8 pixels and a height of N pixels.
                  Each row of 8 pixels is read as bit-coded (with the most significant bit of each byte displayed on the left) starting from memory location I;
                  I value doesn't change after the execution of this instruction.
                  As described above, VF is set to 1 if any screen pixels are flipped from set to unset when the sprite is drawn, and to 0 if that doesn't happen.
               *)

        let vx = Byte.get_bits 3 opcode in
        let vy = Byte.get_bits 2 opcode in
        (* here n represent the height *)
        let height = Byte.get_bits 1 opcode in

        let x = int_of_char M.reg.[vx] in
        let y = int_of_char M.reg.[vy] in

        (* Printf.printf "Draw a sprite at x = %d from vx = %d |  y = %d from vy = %d\n%!" x vx y vy; *)
        let rec fill_gfx_height h_pos =
          let rec fill_gfx_width pixel w_pos =
            (* we scan bit by bit, to check if a gfx pixel should be modify or not *)
            if pixel land (0b1 lsl (M.sprite_width - 1 - w_pos)) > 0 then begin
              let y_ = (y + h_pos) mod M.gfx_height in
              let x_ = (x + w_pos) mod M.gfx_width in

              if M.gfx.(y_).(x_) = 1 then begin
                M.reg.[0xf] <- '\001';
                M.gfx.(y_).(x_) <- 0 ;
              end else
                M.gfx.(y_).(x_) <- 1 ;
            end;

            if w_pos = M.sprite_width - 1 then ()
            else fill_gfx_width pixel (w_pos + 1)
          in

          let pixel = int_of_char M.memory.[!M.i + h_pos] in
          fill_gfx_width pixel 0 ;

          if h_pos = height - 1 then ()
          else fill_gfx_height (h_pos + 1)
        in

        (* Printf.printf "x = %d | y = %d\n%!" x y; *)
        M.reg.[0xf] <- '\000';
        begin try
          fill_gfx_height 0 ;
        with _ ->
          Printf.printf "End: x = %d | y = %d | h + %d\n%!" x y height;
          exit 0
        end;
        (* Array.iter ( *)
        (*   fun el -> *)
        (*     Array.iter ( *)
        (*       fun el -> *)
        (*         Printf.printf "%d" el *)
        (*     ) el; *)
        (*     Printf.printf "\n" *)
        (* ) M.gfx ; *)

        (* Printf.printf "\n ------------------------------------ \n\n%!" ; *)

        incr(draw_cnt);
        incr_pc ()

      | 0xe -> (* EX9E *)
        let vx = Byte.get_bits 3 opcode in
        let x = int_of_char M.reg.[vx] in
        let opcode_res = Byte.get_bits ~len:2 2 opcode in
        if opcode_res = 0x9E then begin (* EX9E: Skips the next instruction if the key stored in VX is pressed *)
          let skip = (M.key.(x) = 1) in
          incr_pc ~skip ()
        end else if opcode_res = 0xA1 then begin (* EXA1: Skips the next instruction if the key stored in VX isn't pressed *)
          let skip = (M.key.(x) = 0) in
          incr_pc ~skip ()
        end else
          raise (Unknow_opcode opcode)

      | 0xf ->
        let vx = Byte.get_bits 3 opcode in
        let opcode_res = Byte.get_bits ~len:2 2 opcode in

        (* FX0A: A key press is awaited, and then stored in VX *)
        (* we check every single key to check if any is press *)
        (* if not we don't increment pc and try again *)
        if opcode_res = 0x0A then begin
          let k_len = Array.length M.key in
          let rec check i =
            if M.key.(i) = 1 then incr_pc ()
            else begin
              if i = k_len - 1 then ()
              else check (i + 1)
            end
          in
          check 0
        end else begin
          incr_pc ();
          match opcode_res with
            | 0x07 -> (* FX07: Sets VX to the value of the delay timer *)
              M.reg.[vx] <- char_of_int !M.delay_timer

            (* (\* TODO *\) *)
            (* | 0x0A -> (\* FX0A: A key press is awaited, and then stored in VX *\) *)
            (*   () *)

            | 0x15 -> (* FX15: Sets the delay timer to VX *)
              M.delay_timer := int_of_char M.reg.[vx]

            | 0x18 -> (* FX18: Sets the sound timer to VX *)
              M.sound_timer := int_of_char M.reg.[vx]

            | 0x1E -> (* FX1E: Adds VX to I *)
              let i = !M.i + (int_of_char M.reg.[vx]) in
              M.i := i land 0xFFF;
              if i > 0xFFF then M.reg.[0xf] <- '\001'
              else M.reg.[0xf] <- '\000'

            | 0x29 -> (* FX29: Sets I to the location of the sprite for the character in VX. Characters 0-F (in hexadecimal) are represented by a 4x5 font *)
              let x = int_of_char M.reg.[vx] in
              M.i := x * 5

            | 0x33 -> (* FX33: Stores the Binary-coded decimal representation of VX, with the most significant of three digits at the address in I,
                         the middle digit at I plus 1, and the least significant digit at I plus 2.
                         (In other words, take the decimal representation of VX, place the hundreds digit in memory at location in I,
                         the tens digit at location I+1,and the ones digit at location I+2.)
                      *)
              let x = int_of_char M.reg.[vx] in

              let n1 = x / 100 in
              let n2 = (x mod 100) / 10 in
              let n3 = x mod 10 in

              M.memory.[!M.i] <- char_of_int n1 ;
              M.memory.[!M.i + 1] <- char_of_int n2 ;
              M.memory.[!M.i + 2] <- char_of_int n3

            | 0x55 -> (* FX55: Stores V0 to VX in memory starting at address I *)
              let rec fill i n =
                M.memory.[i] <- M.reg.[n];
                if n >= vx then ()
                else
                  fill (i + 1) (n + 1)
              in

              fill !M.i 0

            | 0x65 -> (* FX65: Fills V0 to VX with values from memory starting at address I *)
              let rec fill i n =
                M.reg.[n] <- M.memory.[i];
                if n >= vx then ()
                else
                  fill (i + 1) (n + 1)
              in

              fill !M.i 0;

            | _ -> raise (Unknow_opcode opcode)
        end

      | _ -> raise (Unknow_opcode opcode)
  in

  decode (fetch_opcode ()) ;

  if !M.delay_timer > 0 then begin
    M.delay_timer := !M.delay_timer / 50
  end;
  if !M.sound_timer > 0 then decr(M.sound_timer)

let draw_flag () =
  (* Printf.printf "draw_cnt is %d\n" !draw_cnt; *)
  if !draw_cnt >= 1 then begin
    draw_cnt := 0;
    true
  end else false
