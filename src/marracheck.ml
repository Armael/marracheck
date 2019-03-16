open Opamcheck2_lib
open OpamTypes
module ST = OpamStateTypes

let log fmt = Printf.fprintf stderr ("LOG: " ^^ fmt ^^ "\n%!")


module Versioned = struct
  type 'a t = {
    head : 'a option;
    git_repo : dirname; (* fixme: opam type for git repos? *)
  }

  let load_and_clean
      ~(repo : dirname)
      ~(load : dirname -> 'a)
    : 'a t =
    (* cleanup uncommited modifications *)
    let _ = repo, load in assert false

  let commit_new_head (_st: 'a t) ~(sync : dir:dirname -> 'a -> unit) : unit =
    let _ = sync in assert false
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

  let load ~dir:_ : t =
    assert false

  let sync ~dir:_ _state : unit =
    assert false
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
    switches : dirname;
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

let create_new_switch gt ~switch_name ~compiler =
  log "Creating a new switch %s..." (OpamSwitch.to_string switch_name);
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
  (gt, sw)

let remove_switch gt ~switch_name =
  OpamSwitchCommand.remove gt ~confirm:false switch_name

(* Relatives to a timestamp directory *)
let cover_path = OpamFilename.of_string "cover.json"
let report_path = OpamFilename.of_string "report.json"
let cover_element_id_path = OpamFilename.of_string "cover_element_build.id"

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

    let pkgs_selection = `All in

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
          let (gt, sw) = create_new_switch gt ~switch_name ~compiler in
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
    let cover_state, switch_state =
      match switch_state.current_timestamp.head with
      | Some c -> c, switch_state
      | None ->
        OpamGlobalState.with_ `Lock_none @@ fun gt ->
        OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
        let u = OpamSwitchState.universe sw
            ~requested:OpamPackage.Name.Set.empty
            OpamTypes.Query (* for historical reasons; should not matter *)
        in
        let selection = compute_package_selection u compiler pkgs_selection in
        let cover = Cover.compute u selection in
        let cover_state = Cover_state.{
          timestamp = get_repo_timestamp ();
          cover = Cover.{ data = cover; path = cover_path };
          report = Cover.{ data = []; path = report_path };
          cover_element_id = Cover.{ data = 0; path = cover_element_id_path };
        } in
        let current_timestamp =
          { switch_state.current_timestamp with head = Some cover_state } in
        Versioned.commit_new_head ~sync:Cover_state.sync current_timestamp;
        cover_state, { switch_state with current_timestamp }
    in

    (* A cover_state has been fetched, or a fresh one was created if there was
       none. *)

    (* Are the packages currently installed in the opam switch compatible with
       the current cover_state? (are they included in the current cover
       element?) *)
    let cover_elt =
      try
        List.nth cover_state.cover.data
          cover_state.cover_element_id.data
      with Failure _ ->
        Printf.eprintf "In %s: invalid id (out of bounds)\n"
          (OpamFilename.Dir.to_string
             (OpamFilename.(Op.(workdir /
                                Dir.to_string work_state.switches /
                                Dir.to_string switch_state.path /
                                Dir.to_string switch_state.current_timestamp.git_repo /
                                to_string cover_element_id_path))));
        exit 1
    in
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
    let reinstall_switch =
      not (OpamPackage.Set.subset sw.installed (Lib.installable cover_elt))
    in
    let sw, gt =
      if reinstall_switch then begin
        log "Unwanted packages are currently installed; re-creating the switch...";
        let sw, gt =
          OpamGlobalState.with_write_lock gt @@ fun gt ->
          let gt = remove_switch gt ~switch_name in
          let gt, sw = create_new_switch gt ~switch_name ~compiler in
          ignore (OpamSwitchState.unlock sw);
          OpamGlobalState.with_write_lock gt @@ fun gt ->
          OpamSwitchAction.set_current_switch `Lock_none gt switch_name, gt in
        OpamSwitchState.unlock sw, OpamGlobalState.unlock gt
      end else
        sw, gt
    in

    (* The cover element can now be built in the current opam switch *)

    ()

  | "cache" :: _ ->
    ()
  | _ ->
    Printf.eprintf "usage: ...\n";
    exit 1
