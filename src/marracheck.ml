open Opamcheck2_lib
open OpamTypes
open OpamStateTypes

let log fmt = Printf.fprintf stderr ("LOG: " ^^ fmt ^^ "\n%!")

let validate_compiler_variant s =
  let open OpamPackage in
  match of_string_opt s with
  | None ->
    None
  | Some pkg ->
    if List.mem (Name.to_string pkg.name)
        ["ocaml-base-compiler"; "ocaml-variants"]
    then Some pkg
    else None

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "run" ::
    "--repo" :: repo_url ::
    (* "-k/--kind" :: repo_kind *)
    (* "--packages" :: packages_selection :: *)
    working_dir ::
    compiler_variant ::
    [] ->

    (* TODO: make sure that working_dir exists; if it exists, is not empty, and
       does not contain a valid opamroot -> exit with an error *)

    let compiler = match validate_compiler_variant compiler_variant with
      | None ->
        Printf.eprintf "Invalid compiler variant: %s\n" compiler_variant;
        (* TODO: expand on what is accepted: ocaml-base-compiler.XXX or
           ocaml-variants.XXX *)
        exit 1
      | Some pkg ->
        pkg
    in

    let opamroot =
      let open OpamFilename in
      Op.(Dir.of_string working_dir / "opamroot") in

    OpamStateConfig.update ~root_dir:opamroot ();
    begin match OpamStateConfig.load opamroot with
      | None ->
        (* opam init *)
        log "Initializing a fresh opam root in %s..."
          (OpamFilename.Dir.to_string opamroot);
        let init_config = OpamInitDefaults.init_config ~sandboxing:true () in
        let repo_url = Some repo_url in
        let repo =
          repo_url |> OpamStd.Option.map (fun repo_url ->
            let repo_name = OpamRepositoryName.default in
            let repo_url = OpamUrl.parse (* ~repo_kind *) repo_url in
            let repo_root =
              OpamRepositoryPath.create opamroot repo_name in
            { repo_root; repo_name; repo_url; repo_trust = None }
          )
        in
        let (_global_state, _repos_state, _system_compiler_formula) =
          OpamClient.init
            ~init_config
            ~interactive:false
            ?repo
            ~bypass_checks:false
            ?dot_profile:None
            ~update_config:false
            ~env_hook:false
            ~completion:false
            (OpamStd.Sys.guess_shell_compat ())
        in
        log "Done initializing a fresh opam root";
        ()
      | Some _ ->
        log "Found an existing opam root in %s"
          (OpamFilename.Dir.to_string opamroot);
        log "Updating the repository...";
        OpamGlobalState.with_ `Lock_write @@ begin fun gt ->
        let success, _changed, _rt =
          OpamClient.update gt
            ~repos_only:true
            ~dev_only:false
            []
        in
        if not success then OpamStd.Sys.exit_because `Sync_error;
        log "Done updating the repository."
        end
    end;

    OpamClientConfig.opam_init
      ~solver:(lazy (module OpamZ3))
      ~best_effort:false
      ();

    let switch_name = OpamSwitch.of_string compiler_variant in
    OpamGlobalState.with_ `Lock_write @@ begin fun gt ->
      let (gt, sw) =
        if OpamGlobalState.switch_exists gt switch_name then begin
          log "Existing switch %s found" compiler_variant;
          let sw =
            OpamSwitchAction.set_current_switch `Lock_none gt switch_name in
          (OpamGlobalState.unlock gt, sw)
        end else begin
          log "Creating a new switch %s..." compiler_variant;
          let (gt, sw) =
            OpamRepositoryState.with_ `Lock_none gt @@ begin fun rt ->
              (* setup a new switch with [compiler_variant] as compiler *)
              OpamSwitchCommand.install gt ~rt
                ~update_config:true
                ~local_compiler:false
                ~packages:[compiler.name, Some (`Eq, compiler.version)]
                switch_name
            end in
          log "New switch successfully created";
          (gt, OpamSwitchState.unlock sw)
        end
      in
      ignore (gt : unlocked global_state);
      ignore (sw : unlocked switch_state);
    end;

  | "cache" :: _ ->
    ()
  | _ ->
    Printf.eprintf "usage: ...\n";
    exit 1
