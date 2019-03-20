open Marracheck_lib
open OpamTypes
module ST = OpamStateTypes
open Utils
open State
module File = OpamFilename
module Dir = OpamFilename.Dir

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

let validate_workdir working_dir =
  let workdir = Dir.of_string working_dir in
  mkdir workdir;
  if (OpamSystem.dir_is_empty working_dir) ||
     (File.exists_dir File.Op.(workdir / opamroot_path))
  then Some workdir
  else None

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

let validate_repo_url url_s =
  let url = OpamUrl.of_string url_s in
  match url.transport with
  | "file" ->
    let dir = Dir.of_string url.path in
    if Sys.file_exists url.path
    && Sys.is_directory url.path
    && OpamGit.VCS.exists dir
    && OpamProcess.Job.run (OpamGit.VCS.revision dir) <> None
    then Some url
    else None
  | _ -> None

let get_repo_timestamp (repo_url: OpamUrl.t) =
  let dir = Dir.of_string repo_url.path in
  match OpamProcess.Job.run (OpamGit.VCS.revision dir) with
  | Some rev -> rev
  | None ->
    (* We assume repo_url has been validated, which ensures there is at least
       one commit in the repo. *)
    assert false

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

let retire_current_timestamp
    ~(current_timestamp : Cover_state.t Versioned.t)
    ~(past_timestamps : dirname)
  =
  match current_timestamp.Versioned.head with
  | None ->
    (* We could probably also also do nothing ... *)
    assert false
  | Some cover_state ->
    let timestamp_s = cover_state.Cover_state.timestamp.data in
    let cur_basename =
      File.Base.to_string (File.basename_dir current_timestamp.git_repo) in
    mv
      current_timestamp.git_repo
      File.Op.(past_timestamps / (cur_basename ^ "_" ^ timestamp_s))

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "run" ::
    "--repo" :: repo_url ::
    (* "-k/--kind" :: repo_kind *)
    (* "--packages" :: packages_selection :: *)
    working_dir ::
    compiler_variant ::
    [] ->

    let workdir = get_or_fatal (validate_workdir working_dir)
        "%s is not empty but does not contain an %s"
        working_dir opamroot_path in

    let pkgs_selection = `All in

    let compiler = get_or_fatal (validate_compiler_variant compiler_variant)
        "Invalid compiler variant: %s" compiler_variant in
    (* TODO: expand on what is accepted: ocaml-base-compiler.XXX or
       ocaml-variants.XXX *)
    assert (OpamPackage.to_string compiler = compiler_variant);

    let repo_url = get_or_fatal (validate_repo_url repo_url)
        "Repo url must be a local git clone of an opam-repository" in

    let opamroot =
      let root_dir = OpamFilename.Op.(workdir / opamroot_path) in
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
          (OpamFilename.prettify_dir opamroot);
        let init_config = OpamInitDefaults.init_config ~sandboxing:true () in
        let repo =
          let repo_name = OpamRepositoryName.default in
          { repo_name; repo_url; repo_trust = None }
        in
        let (_global_state, _repos_state, _system_compiler_formula) =
          OpamClient.init
            ~init_config
            ~interactive:false
            ~repo
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
          (OpamFilename.prettify_dir opamroot);
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
    OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      let (gt, sw) =
        if OpamGlobalState.switch_exists gt switch_name then begin
          log "Existing opam switch %s found" compiler_variant;
          (* TODO: check that the switch repositories are what we expect
             (just our repository) (unlikely to not be the case) *)
          let sw =
            OpamSwitchAction.set_current_switch `Lock_none gt ~rt switch_name in
          if OpamSwitchState.repos_list sw <> [OpamRepositoryName.default] then
            fatal "Switch %s: unexpected list of repositories (expected [%s])"
              (OpamSwitch.to_string switch_name)
              (OpamRepositoryName.(to_string default));
          (OpamGlobalState.unlock gt, sw)
        end else begin
          log "Creating new opam switch %s" compiler_variant;
          let (gt, sw) = create_new_switch gt ~switch_name ~compiler in
          (gt, OpamSwitchState.unlock sw)
        end
      in
      OpamGlobalState.drop gt;
      OpamRepositoryState.drop rt;
      OpamSwitchState.drop sw
    end;

    log "Using opam switch %s" (OpamSwitch.to_string switch_name);

    (* we have a switch of the right name, attached to the right repository *)

    let work_state =
      let view = Work_state.View_single.load_or_create compiler in
      Work_state.load_or_create ~view ~workdir in
    let switch_state = work_state.view in
    let cover_state, switch_state =
      let repo_timestamp = get_repo_timestamp repo_url in
      match switch_state.current_timestamp.head with
      | Some cover_state ->
        (* Is the cover_state timestamp matching the one of the repository?
           If not, we need to retire the current_timestamp directory, and
           create a new one after computing a new cover *)

        (* TODO *)

        cover_state, switch_state
      | None ->
        OpamGlobalState.with_ `Lock_none @@ fun gt ->
        OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
        let u = OpamSwitchState.universe sw
            ~requested:OpamPackage.Name.Set.empty
            OpamTypes.Query (* for historical reasons; should not matter *)
        in
        let selection = compute_package_selection u compiler pkgs_selection in
        let cover = Cover.compute u selection in
        let repo_dir = switch_state.current_timestamp.git_repo in
        let open OpamFilename in
        let cover_state = Cover_state.{
          timestamp =
            { data = get_repo_timestamp repo_url;
              path = Op.(repo_dir // timestamp_path) };
          cover = { data = cover; path = Op.(repo_dir // cover_path) };
          report = { data = []; path = Op.(repo_dir // report_path) };
          cover_element_id =
            { data = 0; path = Op.(repo_dir // cover_element_id_path) };
        } in
        let current_timestamp =
          { switch_state.current_timestamp with head = Some cover_state } in
        Versioned.commit_new_head ~sync:Cover_state.sync current_timestamp
          "Initial cover";
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
        fatal "In %s: invalid id (out of bounds)\n"
          (OpamFilename.prettify cover_state.cover_element_id.path)
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
          OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
          let gt = remove_switch gt ~switch_name in
          let gt, sw = create_new_switch gt ~switch_name ~compiler in
          OpamSwitchState.drop sw;
          OpamGlobalState.with_write_lock gt @@ fun gt ->
          OpamSwitchAction.set_current_switch `Lock_none gt ~rt switch_name, gt in
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
