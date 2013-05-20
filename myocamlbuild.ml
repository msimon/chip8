open Ocamlbuild_plugin
open Command

let _ =
  dispatch begin function
    | After_rules ->
      flag ["ocaml"; "use_cocoa_framework"] (S[A "-cclib";A "-framework Cocoa"])

    | _ -> ()
  end
