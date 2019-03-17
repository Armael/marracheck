open Marracheck_lib
open OpamTypes
module ST = OpamStateTypes
module Json = Ezjsonm

let log fmt = Printf.fprintf stderr ("LOG: " ^^ fmt ^^ "\n%!")
let fatal fmt =
  Printf.kfprintf (fun _ -> exit 1) stderr ("ERROR: " ^^ fmt ^^ "\n%!")

(* Relative to the workdir *)
let cache_path = "cache"
let switches_path = "switches"
let opamroot_path = "opamroot"

(* Relative to a switch_state directory *)
let log_path = "log"
let current_timestamp_path = "current_timestamp.git"
let past_timestamps_path = "past_timestamps"

(* Relative to a current_timestamp.git directory *)
let timestamp_path = "timestamp"
let cover_path = "cover.json"
let report_path = "report.json"
let cover_element_id_path = "cover_element_build.id"

let mkdir dir =
  let dir = OpamFilename.Dir.to_string dir in
  if Sys.file_exists dir && not (Sys.is_directory dir) then
    fatal "Error: %s already exists but is not a directory" dir
  else OpamSystem.mkdir dir

let read_json (file: filename): Json.t =
  let cin = open_in (OpamFilename.to_string file) in
  let res =
    try Ok (Json.from_channel cin) with
      Json.Parse_error (_,_) -> Error () in
  close_in cin;
  match res with
  | Ok j -> j
  | Error () -> fatal "In %s: parsing error" (OpamFilename.prettify file)

let must_succeed cmd res =
  if OpamProcess.is_failure res then
    fatal "Running '%s' failed:\n %s\n%!"
      (OpamProcess.string_of_command cmd)
      (OpamProcess.string_of_result res)

module Versioned = struct
  type 'a t = {
    head : 'a option;
    git_repo : dirname;
  }

  let load_and_clean
      ~(repo : dirname)
      ~(load : dir:dirname -> 'a)
    : 'a t =
    let open OpamProcess in
    let repo_s = OpamFilename.Dir.to_string repo in
    mkdir repo;
    if not (OpamGit.VCS.exists repo) then begin
      let cmd = command ~dir:repo_s "git" [ "init" ] in
      run cmd |> must_succeed cmd
    end;
    (* cleanup uncommited modifications *)
    if Job.run (OpamGit.VCS.is_dirty repo) then begin
      Job.of_list [
        command ~dir:repo_s "git" [ "reset"; "--hard"; "HEAD" ];
        command ~dir:repo_s "git" [ "clean"; "-xfd" ];
      ] |> Job.run
      |> OpamStd.Option.iter (fun (cmd, res) -> must_succeed cmd res)
    end;
    match Job.run (OpamGit.VCS.revision repo) with
    | None ->
      (* No commits recorded *)
      { head = None; git_repo = repo }
    | Some _ ->
      { head = Some (load ~dir:repo); git_repo = repo }

  let commit_new_head (st: 'a t) ~(sync : 'a -> unit) msg : unit =
    let open OpamProcess in
    let repo_s = OpamFilename.Dir.to_string st.git_repo in
    match st.head with
    | None -> assert false
    | Some data ->
      sync data;
      let msg = if msg = "" then "-" else msg in
      Job.of_list [
        command ~dir:repo_s "git" [ "add"; "*"; ];
        command ~dir:repo_s "git" [ "commit"; "-a"; "-m"; msg ];
      ] |> Job.run
      |> OpamStd.Option.iter (fun (cmd, res) -> must_succeed cmd res)
end

module Serialized = struct
  type 'a t = {
    data : 'a;
    path : filename;
  }

  let load_raw ~file : string t =
    { data = OpamSystem.read (OpamFilename.to_string file);
      path = file }

  let sync_raw (s : string t) =
    OpamSystem.write (OpamFilename.to_string s.path) s.data

  let load_json ~file (of_json : Json.t -> 'a) : 'a t =
    let j = read_json file in
    { data = of_json j; path = file }

  let sync_json (s : 'a t) (to_json : 'a -> Json.t) =
    let cout = open_out (OpamFilename.to_string s.path) in
    Json.to_channel ~minify:false cout (to_json s.data);
    close_out cout
end

type timestamp = string (* git hash *)
type build_log = string
type changes = Changes (* fixme *)
type error_cause = [ `Fetch | `Build | `Install ]

type package_report =
  | Success of { log : build_log; changes : changes }
  | Error of { log : build_log; cause : error_cause }
  | Aborted of { deps : OpamPackage.Set.t }

let package_report_of_json (j: Json.value): package_report =
  let l = Json.get_dict j in
  try
    begin match Json.get_string (List.assoc "status" l) with
    | "success" ->
      Success { log = Json.get_string (List.assoc "log" l);
                changes = Changes (* TODO *) }
    | "error" ->
      let cause = match Json.get_string (List.assoc "cause" l) with
        | "fetch" -> `Fetch
        | "build" -> `Build
        | "install" -> `Install
        | _ -> raise Not_found
      in
      Error { log = Json.get_string (List.assoc "log" l); cause }
    | "aborted" ->
      let deps = match OpamPackage.Set.of_json (List.assoc "deps" l) with
        | Some deps -> deps
        | None -> raise Not_found
      in
      Aborted { deps }
    | _ -> raise Not_found
    end
  with Not_found -> Json.parse_error `Null "" (* XX *)

let package_report_to_json = function
  | Success { log; changes = Changes (* TODO *) } ->
    `O [ ("status", `String "success");
         ("log", `String log) ]
  | Error { log; cause } ->
    let cause_s = match cause with
      | `Fetch -> "fetch"
      | `Build -> "build"
      | `Install -> "install"
    in
    `O [ ("status", `String "error");
         ("log", `String log);
         ("cause", `String cause_s) ]
  | Aborted { deps } ->
    `O [ ("status", `String "aborted");
         ("deps", OpamPackage.Set.to_json deps) ]

module Cover = struct
  type t = Lib.cover_elt list

  let compute (u : universe) (selection : OpamPackage.Set.t) : t =
    let (cover, remain) = Lib.compute_cover u selection in
    assert (remain = Ok ());
    (* [selection] must only contain installable packages (see
       [compute_package_selection]). *)
    cover

  let solution_of_json (_: Json.value): OpamSolver.solution option =
    assert false (* TODO! *)

  let solution_to_json (_: OpamSolver.solution): Json.value =
    assert false (* TODO! *)

  let of_json (j: Json.t): t =
    let cover_elt_of_json j =
      try
        let l = Json.get_dict j in
        let get_opt = function Some x -> x | None -> raise Not_found in
        Lib.{ solution =
                solution_of_json (List.assoc "solution" l) |> get_opt;
              useful =
                OpamPackage.Set.of_json (List.assoc "useful" l) |> get_opt; }
      with Not_found -> Json.parse_error `Null "" (* XX *)
    in
    Json.get_list cover_elt_of_json (Json.value j)

  let to_json (cover: t): Json.t =
    let cover_elt_to_json elt =
      `O [ ("solution", solution_to_json elt.Lib.solution);
           ("useful", OpamPackage.Set.to_json elt.Lib.useful) ]
    in
    Json.list cover_elt_to_json cover
end

module Cover_state = struct
  type element_report = (OpamPackage.t * package_report) list
  type report = element_report list
  type element_id = int

  type t = {
    timestamp : timestamp Serialized.t;
    cover : Cover.t Serialized.t;
    report : report Serialized.t;
    cover_element_id : element_id Serialized.t;
  }

  (* This assumes that the files already exist on the filesystem in a valid
     state *)
  let load ~dir : t =
    let open OpamFilename in
    let assoc k l =
      try List.assoc k l with Not_found ->
        Json.parse_error `Null "" (* XX *) in
    let get_opt =
      function Some x -> x | None -> Json.parse_error `Null "" (* XX *) in
    let pkg_report_of_json j =
      let l = Json.get_dict j in
      let pkg = assoc "package" l in
      let pkg_report = assoc "report" l in
      (get_opt (OpamPackage.of_json pkg),
       package_report_of_json pkg_report)
    in
    let element_report_of_json = Json.get_list pkg_report_of_json in
    let report_of_json j =
      try Json.get_list element_report_of_json (Json.value j) with
        Json.Parse_error (_,_) ->
        fatal "In %s: invalid format"
          OpamFilename.(prettify Op.(dir // report_path)) in
    let cover_element_id_of_json = function
      | `O [ "id", `Float id ] -> truncate id
      | _ -> fatal "In %s: invalid format (expected { \"id\" : ... })"
               (OpamFilename.(prettify Op.(dir // cover_element_id_path)))
    in
    { timestamp = Serialized.load_raw ~file:Op.(dir // timestamp_path);
      cover = Serialized.load_json ~file:Op.(dir // cover_path) Cover.of_json;
      report =
        Serialized.load_json ~file:Op.(dir // report_path) report_of_json;
      cover_element_id =
        Serialized.load_json ~file:Op.(dir // cover_element_id_path)
          cover_element_id_of_json; }

  let sync state : unit =
    let report_to_json r =
      Json.list (fun elt ->
        Json.list (fun (pkg, pkg_report) ->
          `O [ ("package", OpamPackage.to_json pkg);
               ("report", package_report_to_json pkg_report) ]
        ) elt
      ) r
    in
    let cover_element_id_to_json id =
      `O [ "id", `Float (float id) ]
    in
    Serialized.sync_raw state.timestamp;
    Serialized.sync_json state.cover Cover.to_json;
    Serialized.sync_json state.report report_to_json;
    Serialized.sync_json state.cover_element_id cover_element_id_to_json
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
      let open OpamFilename in
      let switch_dir =
        Op.(workdir / switches_path / OpamPackage.to_string compiler) in
      mkdir switch_dir;
      mkdir Op.(switch_dir / current_timestamp_path);
      mkdir Op.(switch_dir / past_timestamps_path);
      let current_timestamp =
        Versioned.load_and_clean
          ~repo:Op.(switch_dir / current_timestamp_path)
          ~load:Cover_state.load
      in
      { path = switch_dir;
        log = Op.(switch_dir // log_path);
        current_timestamp;
        past_timestamps = Op.(switch_dir / past_timestamps_path); }

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
    let open OpamFilename in
    mkdir workdir;
    (* workdir / opamroot_path will be created automatically by the opam
       initialization code. *)
    mkdir Op.(workdir / cache_path);
    mkdir Op.(workdir / switches_path);
    { opamroot = Op.(workdir / opamroot_path);
      cache = Op.(workdir / cache_path);
      switches = Op.(workdir / switches_path);
      view = view ~workdir }

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

let validate_repo_url url_s =
  let url = OpamUrl.of_string url_s in
  match url.transport with
  | "file" ->
    let dir = OpamFilename.Dir.of_string url.path in
    if Sys.file_exists url.path
    && Sys.is_directory url.path
    && OpamGit.VCS.exists dir
    && OpamProcess.Job.run (OpamGit.VCS.revision dir) <> None
    then
      Some url
    else None
  | _ -> None

let get_repo_timestamp (repo_url: OpamUrl.t) =
  let dir = OpamFilename.Dir.of_string repo_url.path in
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

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "run" ::
    "--repo" :: repo_url ::
    (* "-k/--kind" :: repo_kind *)
    (* "--packages" :: packages_selection :: *)
    working_dir ::
    compiler_variant ::
    [] ->

    let workdir = OpamFilename.Dir.of_string working_dir in
    mkdir workdir;
    if not (OpamSystem.dir_is_empty working_dir) &&
       not (OpamFilename.exists_dir OpamFilename.Op.(workdir / opamroot_path))
    then
      fatal "%s is not empty but does not contain an %s"
        working_dir opamroot_path;

    let pkgs_selection = `All in

    let compiler = match validate_compiler_variant compiler_variant with
      | None ->
        fatal "Invalid compiler variant: %s\n" compiler_variant
        (* TODO: expand on what is accepted: ocaml-base-compiler.XXX or
           ocaml-variants.XXX *)
      | Some pkg ->
        pkg
    in
    assert (OpamPackage.to_string compiler = compiler_variant);

    let repo_url = match validate_repo_url repo_url with
      | None ->
        fatal "Repo url must be a local git clone of an opam-repository"
      | Some url -> url
    in

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
          let repo_root =
            OpamRepositoryPath.create opamroot repo_name in
          { repo_root; repo_name; repo_url; repo_trust = None }
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
      let (gt, sw) =
        if OpamGlobalState.switch_exists gt switch_name then begin
          log "Existing opam switch %s found" compiler_variant;
          (* TODO: check that the switch repositories are what we expect
             (just our repository) (unlikely to not be the case) *)
          let sw =
            OpamSwitchAction.set_current_switch `Lock_none gt switch_name in
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
      ignore (gt : ST.unlocked ST.global_state);
      ignore (sw : ST.unlocked ST.switch_state);
    end;

    log "Using opam switch %s" (OpamSwitch.to_string switch_name);

    (* we have a switch of the right name, attached to the right repository *)

    let work_state =
      let view = Work_state.View_single.load_or_create compiler in
      Work_state.load_or_create ~view ~workdir in
    let switch_state = work_state.view in
    let cover_state, switch_state =
      match switch_state.current_timestamp.head with
      | Some c ->
        (* TODO: Is the cover_state timestamp matching the one of the repository?
           If not: need to compute a new cover. *)
        c, switch_state
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
