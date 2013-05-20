module M = Mem_req

let screen = ref None

let init () =
  Callback.register_exception "SDL_init_exception" (Sdl.SDL_init_exception "");
  Callback.register_exception "sdlevent_exn" (Sdlevent.Event_exn "");
  Callback.register_exception "SDLvideo2_exception" (Sdlvideo.Video_exn "");
  Callback.register_exception "SDLjoystick_exception" (Sdljoystick.SDLjoystick_exception "");
  Sdl.init [ `EVERYTHING];

  screen := Some (Sdlvideo.set_video_mode (M.gfx_width * 10) (M.gfx_height * 10) [`DOUBLEBUF])


let display () =
  (* Printf.printf "display!\n%!"; *)

  let screen =
    match !screen with
      | Some s -> s
      | None -> raise Not_found
  in

  let rec draw_y y =
    let rec draw_x x =
      if M.gfx.(y).(x) = 1 then begin
        let rect = Sdlvideo.rect ~x:(x * 10) ~y:(y * 10) ~w:10 ~h:10 in
        Sdlvideo.fill_rect ~rect screen (Int32.of_int 0xFFFFFF)
      end;

      if x >= M.gfx_width - 1 then ()
      else draw_x (x + 1)
    in

    draw_x 0 ;
    if y >= M.gfx_height - 1 then ()
    else draw_y (y + 1)
  in

  Sdlvideo.fill_rect screen (Int32.of_int 0);
  draw_y 0;
  Sdlvideo.flip screen
