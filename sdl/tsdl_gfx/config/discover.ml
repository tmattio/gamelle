module C = Configurator.V1

let () =
  C.main ~name:"sdl2_gfx" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lSDL2_gfx" ]; cflags = [] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc -> (
            match C.Pkg_config.query pc ~package:"SDL2_gfx" with
            | None -> default
            | Some deps -> deps)
      in

      let conf =
        if C.ocaml_config_var c "system" = Some "macosx" then
          { conf with libs = "-L/opt/homebrew/lib" :: conf.libs }
        else conf
      in

      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
