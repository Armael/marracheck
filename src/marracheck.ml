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

let init_opam_root ~workdir ~opamroot ~repo_url =
  let init_config = OpamInitDefaults.init_config ~sandboxing:true () in
  (* Setup the hooks for the binary cache script *)
  let wrap cmds (hook_cmd, hook_filter) =
    let fand fopt f = match fopt with
      | None -> f
      | Some f' -> FAnd (f', f)
    in
    CCList.flat_map (fun (cmd, filter) ->
      match hook_filter with
      | None -> [(hook_cmd @ cmd, filter)]
      | Some hook_filter ->
        [hook_cmd @ cmd, Some (fand filter hook_filter);
         cmd, Some (fand filter (FNot hook_filter))]
    ) cmds
  in
  let s x = CString x, None in
  let i x = CIdent x, None in
  let script = s "%{hooks}%/opam-bin-cache.sh" in
  (* TODO: check if these two filters are correct. This is copy-paste from
     opamInitDefaults.ml, I have no idea what I'm doing. *)
  let build_id_isdef =
    FDefined (FIdent ([], OpamVariable.of_string "build-id", None)) in
  let error_code_iszero =
    FOp (FIdent ([], OpamVariable.of_string "error-code", None),
         `Eq, FString "0") in
  let w = OpamFile.InitConfig.wrappers init_config in
  (* This should reflect the "Use as" instructions at the end of the binary
     cache script (see cache_script.ml) *)
  let w =
    let open OpamFile.Wrappers in
    { w with
      pre_install =
        wrap w.pre_install
          ([ script; s "restore"; i "build-id"; i "name" ],
           Some build_id_isdef);
      wrap_build =
        wrap w.wrap_build
          ([ script; s "wrap"; i "build-id" ], Some build_id_isdef);
      wrap_install =
        wrap w.wrap_install
          ([ script; s "wrap"; i "build-id" ], Some build_id_isdef);
      post_install =
        wrap w.post_install
          ([ script; s "store"; i "build-id"; i "installed-files" ],
           Some (FAnd (build_id_isdef, error_code_iszero)));
    } in
  let init_config = OpamFile.InitConfig.with_wrappers w init_config in
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
      (OpamStd.Sys.guess_shell_compat ()) in
  (* Write the binary cache script on disk *)
  let script_file =
    File.(to_string Op.(OpamPath.hooks_dir opamroot // Cache_script.name)) in
  OpamSystem.write script_file (Cache_script.script ~workdir);
  Unix.chmod script_file 0o777;
  ()

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

let universe ~sw =
  OpamSwitchState.universe sw ~requested:OpamPackage.Name.Set.empty
    OpamTypes.Query (* for historical reasons; should not matter *)

let switch_universe () =
  OpamGlobalState.with_ `Lock_none @@ fun gt ->
  OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
  let u = universe ~sw in
  OpamSwitchState.drop sw; OpamGlobalState.drop gt;
  u

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

let recreate_switch gt ~switch_name ~compiler =
  OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
  let gt = remove_switch gt ~switch_name in
  let gt, sw = create_new_switch gt ~switch_name ~compiler in
  OpamSwitchState.drop sw;
  OpamGlobalState.with_write_lock gt @@ fun gt ->
  OpamSwitchAction.set_current_switch `Lock_none gt ~rt switch_name, gt

let build_log_of_exn exn =
  (* Similar to the reporting code in OpamSolution.Json.exc *)
  let (@) l l' = List.rev_append (List.rev l) l' in
  match exn with
  | OpamSystem.Process_error
      { OpamProcess.r_code; r_duration; r_info; r_stdout; r_stderr; _ } ->
    ["======= Process error =======";
     Printf.sprintf "======= Return code: %d =======" r_code;
     Printf.sprintf "======= Duration: %f =======" r_duration;
     "======= Info ======="] @
    (List.map (fun (k, v) -> Printf.sprintf "%s: %s" k v) r_info) @
    ["======= Output ======="] @ r_stdout @
    ["======= Stderr ======="] @ r_stderr
  | OpamSystem.Internal_error s ->
    ["======= Internal error ======="; s]
  | Failure s ->
    ["======= Failure ======="; s]
  | e ->
    ["======= Exception ======="; Printexc.to_string e]

let process_solution_result (result: solution_result) =
  let successes = CCList.filter_map (function
    | `Build p ->
      (* The action itself is not very relevant since everything
         suceeded. It could be `Install or `Fetch as well. *)
      (* FIXME: how do we get the build logs? *)
      Some (p, Success { log = ["TODO"]; changes = Changes })
    | `Install _ | `Fetch _ -> None
    | `Change _ | `Reinstall _ | `Remove _ -> assert false
  ) in
  let failures = CCList.map (fun (action, exn) ->
    match action with
    | `Fetch p ->
      p, Error { log = build_log_of_exn exn; cause = `Fetch }
    | `Build p ->
      p, Error { log = build_log_of_exn exn; cause = `Build }
    | `Install p ->
      p, Error { log = build_log_of_exn exn; cause = `Install }
    | `Change _ | `Reinstall _ | `Remove _ -> assert false
  ) in
  let aborted l = CCList.map (function
    | `Build p | `Install p | `Fetch p -> p
    | `Change _ | `Reinstall _ | `Remove _ -> assert false
  ) l |> CCList.sort_uniq ~cmp:OpamPackage.compare
  in
  match result with
  | Aborted -> fatal "Aborted build"
  | Nothing_to_do -> assert false
  | OK actions -> successes actions, [], []
  | Partial_error actions_result ->
    successes actions_result.actions_successes,
    failures actions_result.actions_errors,
    aborted actions_result.actions_aborted

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

let filter_universe (p: OpamPackage.t -> bool) (u: universe): universe =
  (* It should be enough to remove the package from the "available" set.
     TODO: test that this is the case (i.e. we don't try to build packages
     that have been removed from this set. In particular, there should be
     no cache hit for a "build failure".) *)
  { u with u_available = OpamPackage.Set.filter p u.u_available }

let filtered_switch_universe (cover_state: Cover_state.t) =
  let broken_packages =
    cover_state.report.data
    |> CCList.filter_map (function
      | (pkg, Error _) -> Some pkg
      | _ -> None)
    |> OpamPackage.Set.of_list
  in
  filter_universe
    (fun pkg -> not (OpamPackage.Set.mem pkg broken_packages))
    (switch_universe ())

let cover_pkgs (cover: Cover.t): package_set =
  cover
  |> List.map (fun elt -> elt.Lib.useful)
  |> List.fold_left OpamPackage.Set.union OpamPackage.Set.empty

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "run" ::
    "--repo" :: repo_url ::
    (* "--packages" :: pkgs_selection :: *)
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

    (* Setup the opam-related global state *)

    OpamClientConfig.opam_init
      ~solver:(lazy (module OpamZ3))
      ~best_effort:false
      ~root_dir:opamroot
      ~no_env_notice:true
      ();

    (* Is there anything in the opamroot? *)

    begin match OpamFile.exists (OpamPath.config opamroot) with
      | false ->
        (* No: do "opam init" *)
        log "Initializing a fresh opam root in %s..."
          (OpamFilename.prettify_dir opamroot);
        init_opam_root ~workdir ~opamroot ~repo_url;
        log "Done initializing a fresh opam root"
      | true ->
        (* Yes: do "opam update" *)
        log "Found an existing opam root in %s"
          (OpamFilename.prettify_dir opamroot);
        (* If the commandline [repo] argument is not the same as the
           current opamroot repo, change the global repo url. *)
        begin
          OpamGlobalState.with_ `Lock_none @@ fun gt ->
          OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
          let opamroot_repo =
            OpamRepositoryState.get_repo rt OpamRepositoryName.default in
          if opamroot_repo.repo_url <> repo_url then begin
            log "Existing repo url is different from the one \
                 provided by the user: updating it";
            OpamRepositoryState.with_write_lock rt @@ fun rt ->
            let rt =
              OpamRepositoryCommand.set_url rt
                OpamRepositoryName.default repo_url None in
            OpamRepositoryState.write_config rt;
            (), rt
          end |> fun ((), rt) -> OpamRepositoryState.drop rt
        end;
        log "Updating the repository...";
        OpamGlobalState.with_ `Lock_write @@ begin fun gt ->
          let success, _changed, _rt =
            OpamClient.update gt ~repos_only:true ~dev_only:false [] in
          if not success then OpamStd.Sys.exit_because `Sync_error;
          log "Done updating the repository."
        end
    end;

    (* We now have an initialized opamroot with an updated repository *)

    let switch_name = OpamSwitch.of_string compiler_variant in
    begin
      OpamGlobalState.with_ `Lock_write @@ fun gt ->
      OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
      let (gt, sw) =
        if OpamGlobalState.switch_exists gt switch_name then begin
          log "Existing opam switch %s found" compiler_variant;
          (* Check that the switch repositories are what we expect
             (just our repository)
             (this is a sanity check; it is very most likely the case) *)
          begin
            OpamSwitchState.with_ `Lock_none gt ~switch:switch_name @@ fun sw ->
            let global_repos = OpamGlobalState.repos_list gt in
            let sw_repos = OpamSwitchState.repos_list sw in
            let sw_repos = if sw_repos = [] then (
              log "WARN? Switch has no repos; using the global list of repos";
              global_repos
            ) else sw_repos in
            if sw_repos <> [OpamRepositoryName.default] then
              fatal "The existing switch has unexpected repositories \
                     (expected only \"%s\")"
                OpamRepositoryName.(to_string default)
          end;
          let sw =
            OpamSwitchAction.set_current_switch `Lock_write gt ~rt switch_name in
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

    OpamStateConfig.update ~current_switch:switch_name ();
    log "Using opam switch %s" (OpamSwitch.to_string switch_name);

    (* we have a switch of the right name, attached to the right repository *)

    (* Compute a set of packages from the user package selection.
       This depends on the universe, which is resolved below:
       we might want to filter the universe from the opam switch to remove
       the packages that we already know to be broken
    *)
    let compute_selection_packages : universe -> package_set =
      fun u -> compute_package_selection u compiler pkgs_selection in

    let work_state =
      let view = Work_state.View_single.load_or_create compiler in
      Work_state.load_or_create ~view ~workdir in
    let switch_state = work_state.view in
    let current_timestamp, universe, selection_to_build =
      let repo_timestamp = get_repo_timestamp repo_url in
      match switch_state.current_timestamp.head with
      | Some cover_state ->
        (* Is the cover_state timestamp matching the one of the repository?
           If not, we need to retire the current_timestamp directory, and
           create a new one after computing a new cover *)
        if repo_timestamp <> cover_state.timestamp.data then begin
          retire_current_timestamp
            ~current_timestamp:switch_state.current_timestamp
            ~past_timestamps:switch_state.past_timestamps;
          let current_timestamp =
            Versioned.load_and_clean
              ~repo:switch_state.current_timestamp.git_repo
              ~load:Cover_state.load in
          let universe = switch_universe () in
          let selection_packages = compute_selection_packages universe in
          let cover_state = Cover_state.create
              ~dir:switch_state.current_timestamp.git_repo
              ~timestamp:repo_timestamp
              ~cover:(Cover.compute universe selection_packages) in
          let current_timestamp =
            { current_timestamp with head = Some cover_state } in
          Versioned.commit_new_head ~sync:Cover_state.sync current_timestamp
            "Initial cover";
          current_timestamp, universe, selection_packages
        end else begin
          (* If we are currently building a cover, avoid recomputing it if
             the selection matches what the cover is computing.

             That is, ["useful" packages of the cover] = packages selection.

             If not, compute a new cover (without the packages of the selection
             that we already built, in this cover or even before). *)
          let already_built =
            List.map fst cover_state.report.data
            |> OpamPackage.Set.of_list in
          (* Only consider the cover elements that remain to be built *)
          let cover_pkgs_to_build =
            cover_pkgs @@ match cover_state.build_status.data with
            | Building_element id -> CCList.drop id cover_state.cover.data
            | Build_finished -> []
          in
          let universe = filtered_switch_universe cover_state in
          let selection_packages = compute_selection_packages universe in
          let selection_to_build =
            OpamPackage.Set.diff selection_packages already_built in

          log "User package selection includes %d packages,\
               %d have already been processed"
            (OpamPackage.Set.cardinal selection_packages)
            (OpamPackage.Set.cardinal already_built);

          if OpamPackage.Set.equal selection_to_build cover_pkgs_to_build then
            let () = log "Reusing the current cover for the selection" in
            switch_state.current_timestamp, universe, selection_to_build
          else
            (* Recompute a cover for the remaining packages of the selection to
               build *)
            let () = log "Computing a new cover for the new selection" in
            let cover_state =
              Cover_state.update_cover
                ~cover:(Cover.compute universe selection_to_build) cover_state
            in
            let current_timestamp =
              { switch_state.current_timestamp with head = Some cover_state } in
            Versioned.commit_new_head ~sync:Cover_state.sync current_timestamp
              "New packages selection: compute a new cover";
            current_timestamp, universe, selection_to_build
        end
      | None ->
        (* There is no pre-existing cover. Compute a fresh one. *)
        log "Computing the switch universe";
        let universe = switch_universe () in
        log "Computing the selection packages";
        let selection_packages = compute_selection_packages universe in
        log "Computing the cover state";
        let cover_state = Cover_state.create
            ~dir:switch_state.current_timestamp.git_repo
            ~timestamp:repo_timestamp
            ~cover:(Cover.compute universe selection_packages) in
        log "done";
        let current_timestamp =
          { switch_state.current_timestamp with head = Some cover_state } in
        Versioned.commit_new_head ~sync:Cover_state.sync current_timestamp
          "Initial cover";
        current_timestamp, universe, selection_packages
    in
    log "%d new packages to build"
      (OpamPackage.Set.cardinal selection_to_build);

    (* An adequate cover_state has been fetched or created. *)

    (* Build loop: builds all elements a cover (possibly recomputing it whenever
       broken packages are found)

       Loop invariant:
       - everything other than the opam switch and the cover state stays the
         same (in particular, the current timestamp stays the same)
       - the opam switch can contain some already installed packages
       - there is a valid cover_state which contains the current cover to build
    *)

    let rec build_loop
        (initial_iteration : bool)
        (universe : universe)
        (current_timestamp : Cover_state.t Versioned.t)
      =
      let cover_state = CCOpt.get_exn current_timestamp.head in
      match cover_state.build_status.data with
      | Build_finished ->
        log "Finished building all packages of the selection";
        current_timestamp
      | Building_element cover_elt_id ->
        (* Are the packages currently installed in the opam switch compatible with
           the current cover_state? (are they included in the current cover
           element?) *)
        let cover_elt =
          try List.nth cover_state.cover.data cover_elt_id
          with Failure _ ->
            fatal "In %s: invalid id (out of bounds)\n"
              (OpamFilename.prettify cover_state.build_status.path)
        in
        OpamGlobalState.with_ `Lock_none @@ fun gt ->
        OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
        let reinstall_switch =
          not (OpamPackage.Set.subset sw.installed (Lib.installable cover_elt))
        in
        if reinstall_switch then begin
          if initial_iteration then
            log "Note: unwanted packages are currently installed...";
          log "Re-creating the opam switch.";
          let sw, gt =
            OpamGlobalState.with_write_lock gt @@ fun gt ->
            recreate_switch gt ~switch_name ~compiler in
          OpamSwitchState.drop sw; OpamGlobalState.drop gt
        end;

        (* The cover element can now be built in the current opam switch *)

        let pkgs_success, pkgs_error, pkgs_aborted =
          OpamGlobalState.with_ `Lock_none @@ fun gs ->
          OpamSwitchState.with_ `Lock_write gs @@ fun sw ->
          let (sw, res) =
            OpamSolution.apply sw
              ~ask:false
              ~requested:OpamPackage.Name.Set.empty
              ~assume_built:false
              cover_elt.Lib.solution
          in
          OpamSwitchState.drop sw;
          process_solution_result res
        in

        let universe, cover_state, msg =
          let cover_state =
            cover_state
            |> Cover_state.add_to_report pkgs_success
            |> Cover_state.add_to_report pkgs_error
          in
          if pkgs_error = [] then begin
            assert (pkgs_aborted = []);
            (* Go to the next cover element *)
            let cover_state = Cover_state.go_to_next_elt cover_state in
            universe, cover_state,
            "Cover element build successful; going to next element"
          end else begin
            let pkgs_error_s =
              List.map fst pkgs_error |> OpamPackage.Set.of_list in
            (* Recompute the cover, adding the aborted packages, and remove
               the packages that failed from the universe *)
            let universe = filter_universe
                (fun pkg -> not (OpamPackage.Set.mem pkg pkgs_error_s))
                universe
            in
            let pkgs =
              OpamPackage.Set.union
                (cover_pkgs (CCList.drop cover_elt_id cover_state.cover.data))
                (OpamPackage.Set.of_list pkgs_aborted)
            in
            let cover, uninst = Cover.compute_with_uninst universe pkgs in
            let cover_state =
              cover_state
              |> Cover_state.add_to_report
                (List.map (fun p ->
                   (* TODO(?): deps *)
                   p, Aborted { deps = OpamPackage.Set.empty }) uninst)
              |> Cover_state.update_cover ~cover
            in
            universe, cover_state,
            "Cover element build terminated with some failures, recomputing \
             a new cover for the rest of the packages"
          end
        in
        let current_timestamp =
          { switch_state.current_timestamp with head = Some cover_state } in
        Versioned.commit_new_head ~sync:Cover_state.sync current_timestamp msg;
        log "%s" msg;
        build_loop false universe current_timestamp
    in

    let _current_timestamp = build_loop true universe current_timestamp in
    ()

  | "cache" :: _ ->
    ()
  | _ ->
    Printf.eprintf "usage: ...\n";
    exit 1
