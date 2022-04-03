open Marracheck_lib
open OpamTypes
module ST = OpamStateTypes
open Utils
module File = OpamFilename
module Dir = OpamFilename.Dir
module PkgSet = OpamPackage.Set
module Cover_elt_plan = Lib.Cover_elt_plan

type package_selection = [
  | `All (* all installable packages for a given compiler *)
  | `Revdeps of package list
  | `Packages of package list
]

(* neutralize the environment variable that make our life miserable *)
let cleanup_system_environment () =
  [
    "OPAMSWITCH";
    "OPAM_SWITCH_PREFIX";
  ]
  |> List.iter (fun var -> Unix.putenv var "")

let () =
  cleanup_system_environment ()

let init_opam_root ~workdir ~opamroot ~repo_url =
  let init_config = OpamInitDefaults.init_config ~sandboxing:true () in
  (* Setup the hooks for the binary cache script *)
  let s x = CString x, None in
  let i x = CIdent x, None in
  let script = s "%{hooks}%/opam-bin-cache.sh" in
  (* This should reflect the "Use as" instructions at the end of the binary
     cache script (see cache_script.ml) *)
  let build_id_isdef =
    FDefined (FIdent ([], OpamVariable.of_string "build-id", None)) in
  let error_code_iszero =
    FOp (FIdent ([], OpamVariable.of_string "error-code", None),
         `Eq, FString "0") in
  let w = OpamFile.InitConfig.wrappers init_config in
  let w =
    let open OpamFile.Wrappers in
    { w with
      pre_install =
        ([ script; s "restore"; i "build-id"; i "name" ],
           Some build_id_isdef) ::
        w.pre_install;
      wrap_build =
        ([ script; s "wrap"; i "build-id" ], Some build_id_isdef) ::
        w.wrap_build;
      wrap_install =
        ([ script; s "wrap"; i "build-id" ], Some build_id_isdef) ::
        w.wrap_install;
      post_install =
        ([ script; s "store"; i "build-id"; i "installed-files" ],
         Some (FAnd (build_id_isdef, error_code_iszero))) ::
        w.post_install;
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
  let base_deps =
    OpamSolver.dependencies ~depopts:true ~build:true ~post:true
      ~installed:true u u.u_base
  in
  let base = PkgSet.union u.u_base base_deps in
  { u with u_installed = base;
           u_installed_roots =
             PkgSet.inter base u.u_installed_roots;
           u_pinned = PkgSet.empty;
  }

(* Used to discard packages from the universe that we know are broken
   (e.g. do not build). *)
let remove_from_universe (u: universe) (to_remove : PkgSet.t): universe =
  (* Hopefully it is enough to filter [u_available]?... *)
  { u with
    u_available = PkgSet.diff u.u_available to_remove;
  }

(* Assumes a clean universe *)
let compute_package_selection (u: universe)
  : package_selection -> PkgSet.t
  =
  function
  | `All ->
    log "Computing the set of all installable packages...";
  (* NB: this is somewhat expensive to compute *)
    let allpkgs = OpamSolver.installable u in
    log "Done (there are %d installable packages)" (PkgSet.cardinal allpkgs);
    allpkgs

  | `Packages pkgs ->
    OpamPackage.Set.of_list pkgs

  | `Revdeps pkgs ->
    log "Computing the set of transitively installable reverse dependencies...";
    let pkgs =
      OpamSolver.reverse_dependencies
        ~depopts:false ~build:true ~post:true ~installed:false u
        (PkgSet.of_list pkgs)
    in
    log "Done";
    pkgs

let universe ~sw =
  OpamSwitchState.universe sw ~requested:OpamPackage.Set.empty
    OpamTypes.Query (* for historical reasons; should not matter *)

let switch_universe () =
  OpamGlobalState.with_ `Lock_read @@ fun gt ->
  OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
  universe ~sw

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

let create_new_switch gt ~switch ~compiler =
  log "Creating a new switch %s..." (OpamSwitch.to_string switch);
  let ((), sw) =
    OpamRepositoryState.with_ `Lock_none gt @@ begin fun rt ->
      (* setup a new switch with [compiler_variant] as compiler *)
      OpamSwitchCommand.create gt ~rt
        ~update_config:true
        ~invariant:(OpamFormula.of_conjunction [compiler.name, Some (`Eq, compiler.version)])
        switch
        (fun sw -> (), OpamSwitchCommand.install_compiler sw)
    end in
  log "New switch successfully created";
  (* [sw.switch global] is the new [gt]; we need to return it, it has been
     updated! (to contain the newly created switch) *)
  sw, sw.switch_global

let remove_switch gt ~switch =
  OpamSwitchCommand.remove gt ~confirm:false switch

let recreate_switch gt ~switch ~compiler =
  let gt = remove_switch gt ~switch in
  let sw, gt = create_new_switch gt ~switch ~compiler in
  OpamSwitchState.unlock sw, gt

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

let process_action_result
    (action : package action)
    (action_result : (PkgSet.t * PkgSet.t, OpamSolver.Action.Set.t) action_result)
  : Data.Package_report.t list
  =
  (* Because of fetch actions, the result of one action might be relevant for
     several packages.

     (one fetch can be used for several packages, hence if it fails then this
     makes the build of several packages fail and we thus issue several
     corresponding packages reports.) *)
  let module R = Data.Package_report in
  let error exn cause = R.Error { log = build_log_of_exn exn; cause } in
  match action with
  | (`Change _ | `Reinstall _ | `Remove _) ->
    assert false
  | (`Build _ | `Fetch _) as action ->
    (* Build and Fetch are dependencies of Install.

       If they fail, Install will be aborted, we record them as failures.
       If they succeed, we don't know whether the package will succeed,
       so do not record a success and wait for the Install action.

       We record all aborts, potentially several for each package.
    *)
    let packages = match action with
      | `Build pkg -> [pkg]
      | `Fetch pkgs -> pkgs
    in
    begin match action_result with
      | `Aborted deps ->
        List.map (fun pkg -> (pkg, R.Aborted { deps })) packages
      | `Successful _ ->
        []
      | `Exception exn ->
        let cause = match action with
          | `Build _ -> `Build
          | `Fetch _ -> `Fetch
        in
        List.map (fun pkg -> (pkg, error exn cause)) packages
    end
  | `Install package ->
    let report = match action_result with
      | `Aborted deps ->
        R.Aborted { deps }
      | `Exception exn ->
        error exn `Install
      | `Successful (installed, _removed) ->
        let changes = (* FIXME *)
          ignore installed; R.Changes in
        let log = (* FIXME *)
          ["TODO"] in
        Success { log; changes }
    in [package, report]

let retire_cover_state ~st ~switch
  =
  let workdir = State.get_workdir st in
  let repo_path = State.(d ~workdir @@ cover_state_path ~switch) in
  let past_timestamps = State.(d ~workdir (past_timestamps_path ~switch)) in
  let timestamp_value = State.(read st (timestamp_path ~switch)) in
  let cur_basename = File.Base.to_string (File.basename_dir repo_path) in
  mv repo_path File.Op.(past_timestamps / (cur_basename ^ "_" ^ timestamp_value))

(* Recover or initialize an opam_root with an up-to-date repository *)
let recover_opam_root ~workdir ~repo_url opamroot =
  (* Does the opamroot look like a corrupted, partially initialized opamroot?
     (concretely, we check for the cache script that we normally write after
     initializing the opamroot) *)
  if OpamFile.exists (OpamPath.config opamroot) &&
     not (OpamFile.exists (
       OpamFile.make
         File.(Op.(OpamPath.hooks_dir opamroot // Cache_script.name))
     ))
  then begin
    log "The opam root looks like it has been only partially initialized. Removing it...";
    OpamSystem.remove_dir (File.Dir.to_string opamroot)
  end;

  (* Is there anything in the opamroot? *)
  match OpamFile.exists (OpamPath.config opamroot) with
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
      end |> fun ((), _rt) -> ()
    end;
    log "Updating the repository...";
    OpamGlobalState.with_ `Lock_write @@ begin fun gt ->
      let success, _changed, _rt =
        OpamClient.update gt ~repos_only:true ~dev_only:false [] in
      if not success then OpamStd.Sys.exit_because `Sync_error;
    end;
    log "Done updating the repository."

let recover_opam_switch ~compiler ~compiler_variant ~switch =
  OpamGlobalState.with_ `Lock_write @@ fun gt ->
  let _gt =
    if not (OpamGlobalState.switch_exists gt switch) then begin
      log "Creating new opam switch %s" compiler_variant;
      let sw, gt = create_new_switch gt ~switch ~compiler in
      OpamSwitchState.drop sw;
      gt
    end else begin
      log "Existing opam switch %s found" compiler_variant;
      (* Check that the switch repositories are what we expect
         (just our repository)
         (this is a sanity check; it is very most likely the case) *)
      begin
        OpamSwitchState.with_ `Lock_none gt ~switch:switch @@ fun sw ->
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
      (* TODO update/refine/fix this criterion, esp with switch invariants in
         mind? *)
      let sw_base_installed =
        OpamSwitchState.with_ `Lock_none gt ~switch
          (fun sw ->
             PkgSet.inter
               sw.compiler_packages sw.installed) in
      if not (PkgSet.mem compiler sw_base_installed) then begin
        log "Either the compiler %s is not installed or not in the \
             base packages of the switch" (OpamPackage.to_string compiler);
        log "Creating a new switch instead";
        let (sw, gt) = recreate_switch gt ~switch ~compiler in
        OpamSwitchState.drop sw;
        gt
      end else begin
        begin OpamSwitchState.with_ `Lock_none gt ~switch @@ fun sw ->
          let _sw = OpamSwitchAction.set_current_switch gt sw in ()
        end;
        OpamGlobalState.unlock gt
      end
    end
  in
  ()

(* Are the packages currently installed in the opam switch compatible with
   the current cover element? *)
let recover_opam_switch_for_plan ~switch ~universe ~compiler plan =
    OpamGlobalState.with_ `Lock_none @@ fun gt ->
    OpamSwitchState.with_ `Lock_read gt @@ fun sw ->
    let elt_installs = Cover_elt_plan.installs plan in
    (* will be installed in the end: base packages + elt_installs *)
    let elt_pkgs = PkgSet.union elt_installs universe.u_installed in
    (* TODO: we could be less conservative and check (using the solver?) that
       the plan can be executed in the current switch, instead of conservatively
       assuming that any package not in the plan but currently installed may
       conflict with the plan. *)
    let reinstall_switch =
      not (PkgSet.subset sw.installed elt_pkgs)
    in
    if reinstall_switch then begin
      log "Some packages currently installed in the switch are not part of \
           the set of packages we wish to install.";
      log "Re-creating a fresh opam switch...";
      let _sw, _gt =
        OpamGlobalState.with_write_lock gt @@ fun gt ->
        recreate_switch gt ~switch ~compiler in
      ()
    end

(* Archive the current cover element if necessary *)
let recover_cover_state ~(selection : PkgSet.t) ~st ~switch : bool (* has_changed *) =
  let already_built = State.resolved_packages st ~switch in
  let selection_already_built =
    PkgSet.inter already_built selection in
  let selection_remaining =
    PkgSet.diff selection already_built in

  log "%d packages from the selection have already been built; \
       remaining: %d"
    (PkgSet.cardinal selection_already_built)
    (PkgSet.cardinal selection_remaining);

  match State.(read st @@ cur_plan_path ~switch) with
  | None -> false
  | Some plan ->
    (* A cover element was already being built.

       We keep this cover element if this does not result
       in useless work: the packages remaining to build
       are all part of the user selection.
    *)
    let current_todo = State.nonbuilt_useful_packages st ~switch plan in
    if PkgSet.subset current_todo selection
    then begin
      log "Reusing the current cover element";
      false
    end else begin
      log "Archiving the current cover element";
      (* NB: here we archive the current element even though it has only
         been built partially (that's fine as long as we remember to
         consider cover elements only as the output of the solver for
         co-installable packages, not as the build log). *)
      let _elt = State.archive_cur_elt st ~switch in
      true
    end

let recover_switch_state
    ~(repo_url: OpamUrl.t)
    ~(selection: PkgSet.t)
    ~st ~switch
  =
  let repo_timestamp = get_repo_timestamp repo_url in
  if State.(exists st @@ cover_state_path ~switch)
  && State.(read st @@ timestamp_path ~switch) = repo_timestamp
  then begin
    log "Found an existing cover state for the current timestamp";
    let has_changed =
      recover_cover_state ~selection ~st ~switch in
    if not has_changed then
      (* Avoid polluting the git history with identity commits *)
      ()
    else
      State.(commit st (cover_state_path ~switch) ~msg:"Update cover state")
  end else begin
    if State.(exists st @@ cover_state_path ~switch) then begin
      (* There is an existing cover_state, but its timestamp does not match
         the one of the repository. Retire the cover_state and recreate it. *)
      log "Existing cover state is for an old repo timestamp.";
      log "Create a fresh cover state for the current timestamp.";
      State.recreate st (State.cover_state_path ~switch)
        ~finalize:(fun () -> retire_cover_state ~st ~switch)
        ~init:(fun () ->
        (* Initialize the directory. We need to provide the timestamp
           explicitly; the other files will be generated automatically from
           their default value following the schema. *)
          State.(write st @@ timestamp_path ~switch) repo_timestamp)
    end else begin
      (* Create a fresh cover_state repository *)
      log "Initialize a fresh cover state";
      State.mkdir st (State.cover_state_path ~switch) ~init:(fun () ->
        (* other files will be generated automatically from default values *)
        State.(write st @@ timestamp_path ~switch) repo_timestamp)
    end;
    State.(commit st ~msg:"Initial cover state" @@ cover_state_path ~switch)
  end

let build
    ~(st : State.t)
    ~(switch : switch)
    ~(compiler : package)
    ~(universe : universe)
    ~(universe_cycles : PkgSet.t list list)
    ~(to_install: PkgSet.t)
  =
  let switch_o = switch in
  let switch = OpamSwitch.to_string switch_o in
  (* Build loop:
     - compute the next element of the cover (if needed);
     - build all packages of this element;
     - archive it, and update the set of remaining packages;

     Loop invariant:
     - everything other than the opam switch and the cover state stays the
       same (in particular, the opam-repository timestamp stays the same)
     - the opam switch can contain some already installed packages
     - we maintain and update a valid cover_state which contains the current
       element, the old elements, the report of built packages
     - the universe is "clean", no extra installed packages: it corresponds to
       an empty switch with only the base packages installed.
     - the installable packages of the universe do not contain packages
       whose build errored.
  *)
  let rec start_build
      ~(universe : universe)
      ~(to_install: PkgSet.t)
    =
    match State.(read st @@ cur_plan_path ~switch) with
    | None ->
       build_next_cover_element ~universe ~to_install
    | Some cover_elt ->
       build_cover_element ~universe ~to_install cover_elt

  and finish_build () =
    let uninst = State.(read st @@ uninst_path ~switch) in
    log "Finished building all packages of the selection \
         (%d uninstallable packages)"
      (PkgSet.cardinal uninst);
    ()

  and build_next_cover_element
    ~(universe : universe)
    ~(to_install: PkgSet.t)
  =
    if PkgSet.is_empty to_install then
      finish_build ()
    else begin
      log "Computing the next element...";
      let elt, remaining =
        Cover_elt_plan.compute
          ~make_request:(Lib.make_request_maxsat ~cycles:universe_cycles)
          ~universe ~to_install in
      if PkgSet.is_empty elt.useful then begin
        assert (PkgSet.equal to_install remaining);
        State.(write st @@ uninst_path ~switch) remaining;
        State.(commit ~msg:"No new element; build loop done" st @@ cover_state_path ~switch);
        finish_build ()
      end else begin
        State.(write st @@ cur_plan_path ~switch) (Some elt);
        State.(commit ~msg:"New element computed" st @@ cover_state_path ~switch);
        build_cover_element ~universe ~to_install elt
      end
    end

  and build_cover_element
    ~(universe : universe)
    ~(to_install: PkgSet.t)
    (plan : Cover_elt_plan.t)
    =
    (* Note: we recover after a program restart,
       but also when starting a new cover element. *)
    recover_opam_switch_for_plan ~switch:switch_o ~universe ~compiler plan;

    (* The cover element can now be built in the current opam switch *)
    let update_cover_state package_action action_result =
      match process_action_result package_action action_result with
      | [] -> ()
      | report_items ->
        List.iter State.(append st @@ cur_report_path ~switch) report_items;
        let msg =
          Printf.sprintf "New report for package%s %s"
            (if List.length report_items > 1 then "s" else "")
            (String.concat ", "
               (List.map (fun (p, _) -> OpamPackage.to_string p) report_items))
        in
        State.(commit st ~msg @@ cover_state_path ~switch);
    in

    begin
      OpamGlobalState.with_ `Lock_none @@ fun gs ->
      OpamSwitchState.with_ `Lock_write gs @@ fun sw ->
      let (_sw, res) =
        OpamSolution.apply sw
          ~ask:false
          ~requested:OpamPackage.Set.empty
          ~assume_built:false
          ~report_action_result:update_cover_state
          plan.Cover_elt_plan.solution
      in
      ignore res; (* we updated the cover state report incrementally *)
    end;

    (* Update the cover state *)
    let cover_elt = State.archive_cur_elt st ~switch in
    let cover_elt = CCOption.get_exn_or "?" cover_elt in
    State.(commit st ~msg:"Built the current cover element" @@ cover_state_path ~switch);
    log "Finished building the current cover element";

    let universe, to_install =
      let successes, errors =
        let (_plan, report) = cover_elt in
        PkgMap.fold (fun package result (suc, err) ->
          match result with
          | Data.Package_report.Success _ -> (PkgSet.add package suc, err)
          | Error _ -> (suc, PkgSet.add package err)
          | Aborted _ -> (suc, err)
        ) report (PkgSet.empty, PkgSet.empty)
      in
      remove_from_universe universe errors,
      PkgSet.Op.(to_install -- successes -- errors)
    in
    build_next_cover_element ~universe ~to_install

  in
  start_build ~universe ~to_install

let run_cmd ~repo_url ~working_dir ~compiler_variant ~package_selection =
  (* resolve workdir as an absolute path *)
  let workdir =
    if Filename.is_relative working_dir then
      Filename.concat (Sys.getcwd ()) working_dir
    else
      working_dir
  in

  (* create [working_dir] if it does not exist *)
  mkdir (Dir.of_string working_dir);

  (* Create a state handle for the workdir, checking that it conforms to the
     expected schema, creating directories/files with default values if needed
     *)
  let st = State.load ~workdir in

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
    let root_dir = State.(d ~workdir opamroot_path) in
    OpamStateConfig.opamroot ~root_dir ()
  in

  (* Setup the opam-related global state *)
  OpamArg.init_opam_env_variabes OpamCLIVersion.Sourced.current;
  OpamClientConfig.opam_init
    ~best_effort:false
    ~root_dir:opamroot
    ~no_env_notice:true
    ();

  recover_opam_root ~workdir ~repo_url opamroot;
  (* We now have an initialized opamroot with an updated repository *)

  let switch_o = OpamSwitch.of_string compiler_variant in
  let switch = compiler_variant in
  recover_opam_switch ~compiler ~compiler_variant ~switch:switch_o;

  OpamStateConfig.update ~current_switch:switch_o ();
  log "Using opam switch %s" switch;

  (* We have a switch of the right name, attached to the right repository.
     Installed packages in the switch include at least our compiler and
     related base packages (there can also be other packages). *)

  (* TODO: do we want to filter the universe to exclude packages that we know
     are broken? *)
  let universe = switch_universe () |> prepare_universe in
  let universe_cycles =
      log "Computing cycles in the packages universe...";
      let universe_cycles = Lib.compute_universe_cycles universe in
      log "Done";
      universe_cycles
    )
  in

  let selection_packages = compute_package_selection universe package_selection in
  log "User package selection includes %d packages"
    (PkgSet.cardinal selection_packages);

  recover_switch_state ~st ~repo_url ~selection:selection_packages ~switch;

  let universe, to_install =
    let broken = State.broken_packages st ~switch in
    let resolved = State.resolved_packages st ~switch in
    remove_from_universe universe broken,
    PkgSet.diff selection_packages resolved
  in

  build ~st ~switch:switch_o ~compiler ~universe ~universe_cycles ~to_install;
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
    & pos 1 (some string) None
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
      ~doc:"The selection of packages to build with marracheck:\n
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
        $ arg_package_selection)
  |> Cmd.v
       (Cmd.info "run"
          ~doc:"Build the selected opam packages from the given opam repository. \
                All state of marracheck (build cache, action logs, task set...) \
                are stored persistently in the working directory.\n")

let cache_term =
  let open Cmdliner in
  Term.(const cache_cmd $ const ())
  |> Cmd.v
       (Cmd.info "cache"
          ~doc:"Maintenance operation on the working cache \
                of a marracheck working directory.\n
                (TODO Not implemented yet)")

let usage_term =
  let open Cmdliner in
  Cmd.info "marracheck"
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
    ~exits:Cmd.Exit.defaults (* TODO *)

let () =
  let open Cmdliner in
  exit @@ Cmd.eval @@ Cmd.group usage_term [run_term; cache_term]
