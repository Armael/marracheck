open Marracheck_lib
open OpamTypes
module ST = OpamStateTypes
open Utils
open State
module File = OpamFilename
module Dir = OpamFilename.Dir
module PkgSet = OpamPackage.Set

type package_selection = [
  | `All (* all installable packages for a given compiler *)
  | `Revdeps of package list
  | `Packages of package list
]

let init_opam_root ~workdir ~opamroot ~repo_url =
  let init_config = OpamInitDefaults.init_config ~sandboxing:true () in
  (* Setup the hooks for the binary cache script *)
  let wrap cmds (hook_cmd, hook_filter) =
    let fand fopt f = match fopt with
      | None -> f
      | Some f' -> FAnd (f', f)
    in
    if cmds = [] then
      [(hook_cmd, hook_filter)]
    else
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

(* Receive a universe coming from a switch in an unknown state, but with the
   correct repository.

   Output a universe corresponding to an "empty switch" with base packages only,
   and prepared for the rest of the operations.


   We know that the compiler we want is in the base packages of the switch.

   Since the repository is correct, the [u_packages] and [u_available] fields
   should be correct (for the repository that is set currently), but more
   packages might be installed and we do not want to rely on them.

   TODO: check that the universe of an old switch that was created with an old
   repo gets up updated if we just update the repo. *)
let prepare_universe (u: universe): universe =
  (* Restrict installed packages to the set of base packages, as with a
     fresh switch *)
  let u = { u with u_installed = u.u_base;
                   u_installed_roots =
                     PkgSet.inter u.u_base u.u_installed_roots;
                   u_pinned = PkgSet.empty;
          } in
  (* Lib.universe_exclude_cycles u *)
  u

(* Used to discard packages from the universe that we know are broken
   (e.g. do not build). *)
let filter_universe (p: OpamPackage.t -> bool) (u: universe): universe =
  (* Hopefully it is enough to filter [u_available]?... *)
  { u with
    u_available = PkgSet.filter p u.u_available;
    (* u_packages = PkgSet.filter p u.u_packages;
     * u_installed = PkgSet.filter p u.u_installed; *)
  }

let compute_universe_cycles (u: universe): PkgSet.t list list =
  let pkgs, cycles = OpamAdminCheck.cycle_check u in
  List.map (fun cycle ->
    List.map (fun f -> OpamFormula.packages pkgs f) cycle
  ) cycles

(* Assumes a clean universe *)
let compute_package_selection (u: universe) (compiler: package)
  : package_selection -> PkgSet.t
  =
  (* NB: this is somewhat expensive to compute *)
  let allpkgs = OpamSolver.installable u in
  function
  | `All -> allpkgs
  | `Revdeps _ | `Packages _ ->
    ignore compiler;
    assert false (* TODO *)

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
    | `Install p ->
      (* FIXME: how do we get the build logs? *)
      Some (p, Success { log = ["TODO"]; changes = Changes })
    | `Build _ | `Fetch _ ->
      (* Build and Fetch are dependencies of Install, so they must have succeded
         if Install did succeed *)
      None
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
    | `Build p | `Install p | `Fetch p ->
      p, Aborted { deps = PkgSet.empty (* TODO *) }
    | `Change _ | `Reinstall _ | `Remove _ -> assert false
  ) l
  |> CCList.sort_uniq ~cmp:(fun (p,_) (q,_) -> OpamPackage.compare p q)
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

(* Build loop:
   - compute the next element of the cover (if needed);
   - build all packages of this element;
   - archive it, and update the set of remaining packages;

   Loop invariant:
   - everything other than the opam switch and the cover state stays the
     same (in particular, the current timestamp stays the same)
   - the opam switch can contain some already installed packages
   - we maintain and update a valid cover_state which contains the current
     element, the old elements, the report of built packages, and the
     remaining packages to build
   - the universe is "clean": it corresponds to an empty switch with only
     the base packages installed.
     At the beginning of each loop iteration, we make sure to exclude from
     the universe packages that did not build according to the report.
*)
let rec build_loop
    ~(switch_name : switch)
    ~(compiler : package)
    ~(universe : universe)
    ~(universe_cycles : PkgSet.t list list)
    (current_timestamp : Cover_state.t Versioned.t)
  =
  let cover_state = CCOpt.get_exn current_timestamp.head in
  let broken_pkgs = Cover_state.broken_packages cover_state in
  (* TODO: maybe we don't need to filter out *past* broken packages
     (currently included in the broken_packages result), we can
     maintain the invariant that the universe never contains past
     broken packages *)
  let universe =
    filter_universe (fun p -> not (PkgSet.mem p broken_pkgs))
      universe
  in
  match cover_state.cur_elt.data with
  | None ->
    if PkgSet.is_empty cover_state.future.data then begin
      log "Finished building all packages of the selection \
           (%d uninstallable packages)"
        (PkgSet.cardinal cover_state.uninst.data);
      current_timestamp
    end else begin
      let to_install = cover_state.future.data in
      log "Computing the next element...";
      let elt, remaining =
        Lib.compute_cover_elt
          ~make_request:(Lib.make_request_maxsat ~cycles:universe_cycles)
          ~universe ~to_install in
      let cover_state, msg =
        if PkgSet.is_empty elt.useful then begin
          assert (PkgSet.equal to_install remaining);
          { cover_state with
            future = { cover_state.future with data = PkgSet.empty };
            uninst = { cover_state.uninst with data = remaining };
          },
          "No new element; build loop done"
        end else
          { cover_state with
            cur_elt = { cover_state.cur_elt with data = Some elt };
            future = { cover_state.future with data = remaining };
          },
          "New element computed"
      in
      let current_timestamp =
        Versioned.commit_new_head ~sync:Cover_state.sync msg
          { current_timestamp with head = Some cover_state } in
      build_loop ~switch_name ~compiler ~universe ~universe_cycles
        current_timestamp
    end

  | Some elt ->
    (* Are the packages currently installed in the opam switch compatible with
       the current element? *)
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
    let elt_installs = Lib.elt_installs elt in
    let reinstall_switch =
      not (PkgSet.subset
             sw.installed
             (PkgSet.union
                elt_installs universe.u_installed))
    in
    if reinstall_switch then begin
      log "Re-creating a fresh opam switch.";
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
          elt.Lib.solution
      in
      OpamSwitchState.drop sw;
      process_solution_result res
    in

    (* Update the cover state *)
    let cover_state =
      cover_state
      |> Cover_state.add_items_to_report pkgs_success
      |> Cover_state.add_items_to_report pkgs_error
      |> Cover_state.add_items_to_report pkgs_aborted
      |> Cover_state.archive_cur_elt
    in
    let current_timestamp =
      Versioned.commit_new_head ~sync:Cover_state.sync
        "Built the current cover element"
        { current_timestamp with head = Some cover_state } in
    log "Finished building the current cover element";
    build_loop ~switch_name ~compiler ~universe ~universe_cycles
      current_timestamp

let run_cmd ~repo_url ~working_dir ~compiler_variant ~package_selection =
  let workdir = get_or_fatal (validate_workdir working_dir)
      "%s is not empty but does not contain an %s"
      working_dir opamroot_path in

  let compiler = get_or_fatal (validate_compiler_variant compiler_variant)
      "Invalid compiler variant: %s%t" compiler_variant
      (fun ppf ->
         let heuristic_variant = "ocaml-base-compiler."^compiler_variant in
         match validate_compiler_variant heuristic_variant with
         | None -> ()
         | Some _ ->
           Format.fprintf ppf ".\nDid you mean %s or %s?"
             heuristic_variant
             ("ocaml-variants."^compiler_variant))
  in
  assert (OpamPackage.to_string compiler = compiler_variant);

  let repo_url = get_or_fatal (validate_repo_url repo_url)
      "Repo url must be a local git clone of an opam-repository" in

  let opamroot =
    let root_dir = OpamFilename.Op.(workdir / opamroot_path) in
    OpamStateConfig.opamroot ~root_dir ()
  in

  (* Setup the opam-related global state *)

  OpamClientConfig.opam_init
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
      (* FIXME: if ^C during init_opam_root -> present but corrupted opam root *)
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
      end;
      log "Done updating the repository.";
  end;

  (* We now have an initialized opamroot with an updated repository *)

  let switch_name = OpamSwitch.of_string compiler_variant in
  begin
    OpamGlobalState.with_ `Lock_write @@ fun gt ->
    OpamRepositoryState.with_ `Lock_none gt @@ fun rt ->
    let (gt, sw) =
      if not (OpamGlobalState.switch_exists gt switch_name) then begin
        log "Creating new opam switch %s" compiler_variant;
        let (gt, sw) = create_new_switch gt ~switch_name ~compiler in
        (gt, OpamSwitchState.unlock sw)
      end else begin
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
        (* Reinstall the switch if the compiler is not one of the base
           packages, or if it is not installed *)
        let sw_base_installed =
          OpamSwitchState.with_ `Lock_none gt ~switch:switch_name
            (fun sw ->
               PkgSet.inter
                 sw.compiler_packages sw.installed) in
        if not (PkgSet.mem compiler sw_base_installed) then begin
          log "Either the compiler %s is not installed or not in the \
               base packages of the switch" (OpamPackage.to_string compiler);
          log "Creating a new switch instead";
          let (sw, gt) = recreate_switch gt ~switch_name ~compiler in
          (gt, sw)
        end else begin
          let sw = OpamSwitchAction.set_current_switch
              `Lock_write gt ~rt switch_name in
          (OpamGlobalState.unlock gt, sw)
        end
      end
    in
    OpamGlobalState.drop gt;
    OpamRepositoryState.drop rt;
    OpamSwitchState.drop sw
  end;

  OpamStateConfig.update ~current_switch:switch_name ();
  log "Using opam switch %s" (OpamSwitch.to_string switch_name);

  (* We have a switch of the right name, attached to the right repository.
     Installed packages in the switch include at least our compiler and
     related base packages (there can also be other packages). *)

  (* Compute a set of packages from the user package selection.
     This depends on the universe, which is resolved below:
     we might want to filter the universe from the opam switch to remove
     the packages that we already know to be broken
  *)
  let compute_selection_packages : universe -> package_set =
    fun u -> compute_package_selection u compiler package_selection in

  let work_state =
    let view = Work_state.View_single.load_or_create compiler in
    Work_state.load_or_create ~view ~workdir in
  let switch_state = work_state.view in
  let universe = switch_universe () |> prepare_universe in
  log "Computing cycles in the packages universe...";
  let universe_cycles = compute_universe_cycles universe in
  log "Done";

  let current_timestamp =
    let repo_timestamp = get_repo_timestamp repo_url in
    let selection_packages = compute_selection_packages universe in
    log "User package selection includes %d packages"
      (PkgSet.cardinal selection_packages);
    let current_timestamp : Cover_state.t Versioned.t =
      match switch_state.current_timestamp.head with
      | Some cover_state when repo_timestamp = cover_state.timestamp.data ->
        log "Found an existing cover state for the current timestamp";
        let already_built = Cover_state.resolved_packages cover_state in
        let selection_already_built =
          PkgSet.inter already_built selection_packages in
        let selection_remaining =
          PkgSet.diff selection_packages already_built in

        log "%d packages from the selection have already been built; \
             remaining: %d"
          (PkgSet.cardinal selection_already_built)
          (PkgSet.cardinal selection_remaining);

        let has_changed = false in

        (* Update the cover state with the selection packages *)
        let cover_state, has_changed =
          match Cover_state.update_package_selection selection_packages cover_state with
          | None -> cover_state, has_changed
          | Some cover_state' -> cover_state', true
        in

        (* Archive the current cover element if necessary *)
        let cover_state, has_changed =
          match cover_state.cur_elt.data with
          | None -> cover_state, has_changed
          | Some elt ->
             (* A cover element was already being built.

                We keep this cover element if this does not result
                in useless work: the packages remaining to build
                are all part of the user selection.
              *)
             let current_todo = Cover_state.current_nonbuilt_packages cover_state in
             if PkgSet.subset current_todo selection_packages
             then begin
               log "Reusing the current cover element";
               cover_state, has_changed
             end else begin
               log "Archiving the current cover element";
               (* NB: here we archive the current element even though it has only
                  been built partially (that's fine as long as we remember to
                  consider cover elements only as the output of the solver for
                  co-installable packages, not as the build log). *)
               Cover_state.archive_cur_elt cover_state, true
             end
        in
        (* Avoid polluting the git history with identity commits *)
        if not has_changed then switch_state.current_timestamp
        else
          Versioned.commit_new_head ~sync:Cover_state.sync "Update cover state"
            { switch_state.current_timestamp with head = Some cover_state }
      | _ ->
        (* Start over with a fresh [current_timestamp] directory and fresh cover
           state. *)
        if switch_state.current_timestamp.head <> None then begin
          log "Existing cover state is for an old repo timestamp";
          (* There is an existing cover_state, but its timestamp does not match
             the one of the repository. Retire the current_timestamp directory
             before creating a new one *)
          retire_current_timestamp
            ~current_timestamp:switch_state.current_timestamp
            ~past_timestamps:switch_state.past_timestamps;
        end;
        log "Initialize a fresh cover state";
        (* This initializes a fresh [current_timestamp] directory; it contains
           no data at this point (i.e. [current_timestamp.head = None]) *)
        let current_timestamp =
          Versioned.load_and_clean
            ~repo:switch_state.current_timestamp.git_repo
            ~load:(fun ~dir:_ -> assert false (* there is no data to load *)) in
        (* Add some data to the directory (the initial cover state for our
           packages selection). *)
        let cover_state = Cover_state.create
            ~dir:switch_state.current_timestamp.git_repo
            ~timestamp:repo_timestamp
            ~packages:selection_packages in
        Versioned.commit_new_head ~sync:Cover_state.sync "Initial cover state"
          { current_timestamp with head = Some cover_state }
    in
    current_timestamp
  in

  (* An adequate cover_state has been fetched or created. *)

  let _current_timestamp : Cover_state.t Versioned.t =
    build_loop ~switch_name ~compiler ~universe ~universe_cycles
      current_timestamp
  in
  log "Done";
  ()

let cache_cmd () =
  ()

let run_term =
  let open Cmdliner in
  let arg_repo_url =
    let open Arg in
    required
    & pos 0 (some string) None
    & info
      ~docv:"REPO_URL"
      ~doc:"The URL or local path of the opam repository \
            which marracheck will use as reference. This should be \
            a valid git repository, and marracheck will maintain \
            its own clone of it. Changes to the repository HEAD \
            will cause marracheck to recompute the planned package cover."
      [] in
  let arg_working_dir =
    let open Arg in
    required
    & pos 1 (some dir) None
    & info
    ~docv:"WORKING_DIR"
    ~doc:"The path to the directory where marracheck will store its
          internal state and results."
    []
  in
  let arg_compiler_variant =
    let open Arg in
    required
    & pos 2 (some string) None
    & info
    ~docv:"COMPILER_VARIANT"
    ~doc:"The name of the compiler variant to use for opam testing; \
          this should be the name of a compiler package in the specified \
          opam repository, typically of the form $(i,ocaml-base-compiler.VERSION) \
          or $(i,ocaml-variants.VERSION+variant).\n
          Note in particular that $(i,4.07.1) is not a valid name, \
          you should use $(i,ocaml-base-compiler.4.07.1) instead."
    []
  in
  let arg_package_selection =
    let join_list s =
      String.concat "," s in
    let split_list s =
      String.split_on_char ',' s
      |> List.map String.trim in
    let print_package ppf package =
      Format.fprintf ppf "%s"
        (OpamPackage.to_string package) in
    let print_packages ppf packages =
      Format.fprintf ppf "%s"
        (join_list @@ List.map OpamPackage.to_string packages) in
    let printer ppf selection =
      let printf fmt = Format.fprintf ppf fmt in
      match selection with
      | `All ->
        printf "all"
      | `Packages [package] ->
        printf "package(%a)" print_package package
      | `Packages packages ->
        printf "packages(%a)" print_packages packages
      | `Revdeps packages ->
        printf "revdeps(%a)" print_packages packages
    in
    let parser input =
      Format.eprintf "parser %S\n%!" input;
      let module R = Containers.Result in
      let parse_package package_string =
        match OpamPackage.of_string_opt package_string with
        | Some package -> Ok package
        | None -> Printf.ksprintf (fun s -> Result.Error (`Msg s))
                    "Invalid package %S" package_string
      in
      let parse_packages s =
        R.map_l parse_package (split_list s) in
      Scanf.ksscanf input (fun _ _ ->
      Scanf.ksscanf input (fun _ _ ->
      Scanf.ksscanf input (fun _ _ ->
      Scanf.ksscanf input (fun _ _ ->
        R.Error (`Msg "invalid package selection")
      ) "all" (R.Ok `All)
      ) "package(%s@)" (fun s ->
         R.map (fun p -> `Packages([p])) (parse_package s))
      ) "packages(%s@)" (fun s ->
         R.map (fun ps -> `Packages(ps)) (parse_packages s))
      ) "revdeps(%s@)" (fun s ->
         R.map (fun ps -> `Revdeps(ps))  (parse_packages s))
    in
    let open Arg in
    let docv = "PACKAGE_SELECTION" in
    value
    & opt (conv ~docv (parser, printer)) `All
    & info
      ~docv
      ~doc:"the selection of packages to build with marracheck:\n
            - $(i,all) will build all installable packages for this compiler
              version in the switch\n
            - $(i,packages(foo, bar, baz)) will build the listed packages
              and their dependencies; $(i,package(foo)) for a single package.\n
            - $(i,revdeps(foo, bar, baz)) will build all reverse dependencies
              of any of the listed packages."
      ["selection"; "package-selection"] in
  Term.(const (fun
                repo_url
                working_dir
                compiler_variant
                package_selection ->
                run_cmd
                  ~repo_url
                  ~working_dir
                  ~compiler_variant
                  ~package_selection)
        $ arg_repo_url
        $ arg_working_dir
        $ arg_compiler_variant
        $ arg_package_selection),
  Term.info "run"
    ~doc:"Build the selected opam packages from the given opam repository. \
          All state of marracheck (build cache, action logs, task set...) \
          are stored persistently in the working directory.\n"

let cache_term =
  let open Cmdliner in
  Term.(const cache_cmd $ const ()),
  Term.info "cache"
    ~doc:"Maintenance operation on the working cache \
          of a marracheck working directory.\n
          (TODO Not implemented yet)"

let usage_term =
  let open Cmdliner in
  Term.(ret (const (`Help (`Auto, None)))),
  Term.info "marracheck"
    ~doc:"build many packages from an opam repository"
    ~man:[
      `S Manpage.s_description;
      `P "The marracheck tool builds all packages of a given opam repository, \
          for testing purposes -- testing the packages, or testing the compiler \
          used to build them.";
      `P "marracheck strives to work quickly on a single machine, by maximizing \
          parallelism and incrementality.";

      `S Manpage.s_examples;
      `Pre "marracheck run tmp/opam-repository tmp/working-dir 4.07.1";
      `Pre "marracheck cache clean # TODO";
    ]
    ~exits:Term.default_exits (* TODO *)

let () =
  let open Cmdliner in
  Term.(exit @@ eval_choice usage_term [run_term; cache_term])
