open Opamcheck2_lib
open OpamTypes
module ST = OpamStateTypes

let log fmt = Printf.fprintf stderr ("LOG: " ^^ fmt ^^ "\n%!")


module Versioned = struct
  type 'a t = {
    head : 'a option;
    git_repo : dirname; (* fixme: opam type for git repos? *)
  }

  let load_and_clean ~(repo : dirname) : 'a t =
    let _ = repo in assert false
end

type 'a serialized = {
  data : 'a;
  path : filename;
}

type timestamp = string (* Fixme: git hash *)
type build_log = string
type changes = unit (* fixme *)
type error_cause = [ `Fetch | `Build | `Install ]

type package_report =
  | Success of { log : build_log; changes : changes }
  | Error of { log : build_log; cause : error_cause }
  | Aborted of { deps : OpamPackage.Set.t }

module Cover = struct
  type t = Lib.cover_elt list

  let compute (u : universe) (selection : OpamPackage.Set.t) : t =
    let (cover, remain) = Lib.compute_cover u selection in
    assert (remain = Ok ());
    (* [selection] must only contain installable packages (see
       [compute_package_selection]). *)
    cover
end

module Cover_state = struct
  type element_report = (OpamPackage.t * package_report) list
  type report = element_report list
  type element_id = int

  type t = {
    timestamp : timestamp;
    cover : Cover.t serialized;
    report : report serialized;
    cover_element_id : element_id serialized;
  }
end

module Switch_state = struct
  type t = {
    path : dirname;
    log : filename;
    current_timestamp : Cover_state.t Versioned.t;
    past_timestamps : dirname;
  }
end

module Work_state = struct
  type 'a t = {
    opamroot : dirname;
    cache : dirname;
    view : 'a;
  }

  module View_single = struct
    type t = Switch_state.t
    let load_or_create (compiler:package) ~workdir : t =
      let _ = compiler, workdir in assert false
    let sync ~workdir state =
      let _ = workdir, state in assert false
  end

  module View_all = struct
    type t = Switch_state.t OpamPackage.Map.t
    let load ~workdir : t =
      (* load all things that resemble valid switch states;
         errors for any directory that exists in switches/
         but is not valid *)
      let _ = workdir in assert false
    let sync ~workdir state =
      let _ = workdir, state in assert false
  end

  let load_or_create ~(view : workdir:dirname -> 'view) ~workdir : 'view t =
    let _ = (view, workdir) in assert false

  let sync ~(view : workdir:dirname -> 'view -> unit) ~workdir : unit =
    let _ = (view, workdir) in assert false
end

type package_selection = [
  | `All (* all installable packages for a given compiler *)
  | `Revdeps of package
  | `List of package list
]

let installable_with_compiler (u: universe) (compiler: package) =
  (* The universe comes from a switch in an unknown state, but with the correct
     repository.

     This assumes that the [u_packages] and [u_available] fields are correct
     (for the repository that is set currently), but more packages might be
     installed and we do not want to rely on them.

     TODO: check that assumption in practice

     TODO: This universe is not valid (because the compiler has dependencies);
     is this fine?

     TODO: check that the universe of an old switch that was created with an
     old repo gets up updated if we just update the repo.
  *)
  let u = { u with u_installed = OpamPackage.Set.singleton compiler;
                   u_installed_roots = OpamPackage.Set.singleton compiler;
                   u_base = OpamPackage.Set.singleton compiler;
                   u_pinned = OpamPackage.Set.empty;
          } in
  OpamSolver.installable u

let compute_package_selection (u: universe) (compiler: package)
  : package_selection -> OpamPackage.Set.t
  =
  let allpkgs = installable_with_compiler u compiler in
  function
  | `All -> allpkgs
  | `Revdeps _ | `List _ ->
    assert false

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

    let packages_selection = `All in

    let compiler = match validate_compiler_variant compiler_variant with
      | None ->
        Printf.eprintf "Invalid compiler variant: %s\n" compiler_variant;
        (* TODO: expand on what is accepted: ocaml-base-compiler.XXX or
           ocaml-variants.XXX *)
        exit 1
      | Some pkg ->
        pkg
    in

    let workdir = OpamFilename.Dir.of_string working_dir in
    let opamroot =
      let root_dir = OpamFilename.Op.(workdir / "opamroot") in
      OpamStateConfig.opamroot ~root_dir ()
    in

    OpamClientConfig.opam_init
      ~solver:(lazy (module OpamZ3))
      ~best_effort:false
      ~root_dir:opamroot
      ~no_env_notice:true
      ();

    begin match OpamFile.exists (OpamPath.config opamroot) with
      | false ->
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
        (* TODO: setup the binary cache script *)
        log "Done initializing a fresh opam root";
        ()
      | true ->
        log "Found an existing opam root in %s"
          (OpamFilename.Dir.to_string opamroot);
        (* TODO: If the commandline [repo] argument is not the same as the
           current opamroot repo, change the global repo url. *)
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

    let switch_name = OpamSwitch.of_string compiler_variant in
    OpamGlobalState.with_ `Lock_write @@ begin fun gt ->
      let (gt, sw) =
        if OpamGlobalState.switch_exists gt switch_name then begin
          log "Existing switch %s found" compiler_variant;
          (* TODO: check that the switch repositories are what we expect
             (just our repository) (unlikely to not be the case) *)
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
      ignore (gt : ST.unlocked ST.global_state);
      ignore (sw : ST.unlocked ST.switch_state);
    end;

    (* we have a switch of the right name, attached to the right repository *)

    let work_state =
      let view = Work_state.View_single.load_or_create compiler in
      Work_state.load_or_create ~view ~workdir in
    let switch_state = work_state.view in
    let cover_state =
      match switch_state.current_timestamp.head with
      | Some c -> c
      | None ->
        OpamGlobalState.with_ `Lock_none @@ fun gt ->
        OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
        let u = OpamSwitchState.universe sw
            ~requested:OpamPackage.Name.Set.empty
            OpamTypes.Query (* for historical reasons; should not matter *)
        in
        Cover.compute u (compute_package_selection u compiler package_selection)
    in
    ()

  | "cache" :: _ ->
    ()
  | _ ->
    Printf.eprintf "usage: ...\n";
    exit 1
