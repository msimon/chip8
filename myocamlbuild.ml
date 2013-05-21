open Ocamlbuild_plugin
open Command

let _ =
  dispatch begin function
    | After_rules ->
      begin
        try
          match Ocamlbuild_pack.My_unix.run_and_read "uname -s" with
            | "Darwin" -> flag ["ocaml"; "use_cocoa_framework"] (S[A "-cclib";A "-framework Cocoa"])
            | _ -> ()
        with _ ->
          Printf.eprintf "Cannot find OS type"
      end
    | _ -> ()
  end
