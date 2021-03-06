open OpamTypes
open Utils

module Cover_elt_plan = Lib.Cover_elt_plan

(* Relative to the workdir *)
let cache_path = "cache"
let switches_path = "switches"
let opamroot_path = "opamroot"

(* Relative to a switch_state directory *)
let log_path = "log"
let cover_state_repo_path = "cover_state.git"
let past_timestamps_path = "past_timestamps"

(* Relative to a cover_state.git directory *)
let timestamp_path = "timestamp"
let past_elts_path = "past_elts.json"
let cur_plan_path = "cur_plan.json"
let cur_report_path = "cur_report.json"
let uninst_path = "uninst.json"

module Repo = struct
  type 'a t = {
    head : 'a option;
    path : dirname;
  }

  let load_and_clean
      ~(path : dirname)
      ~(load : dir:dirname -> 'a)
    : 'a t =
    let open OpamProcess in
    let path_s = OpamFilename.Dir.to_string path in
    mkdir path;
    if not (OpamGit.VCS.exists path) then begin
      let cmd = command ~dir:path_s "git" [ "init" ] in
      run cmd |> must_succeed cmd
    end;
    (* cleanup uncommited modifications *)
    if Job.run (OpamGit.VCS.is_dirty path) then begin
      Job.of_list [
        command ~dir:path_s "git" [ "reset"; "--hard"; "HEAD" ];
        command ~dir:path_s "git" [ "clean"; "-xfd" ];
      ] |> Job.run
      |> OpamStd.Option.iter (fun (cmd, res) -> must_succeed cmd res)
    end;
    match Job.run (OpamGit.VCS.revision path) with
    | None ->
      (* No commits recorded *)
      { path; head = None }
    | Some _ ->
      { path; head = Some (load ~dir:path) }

  let commit_new_head ~(sync : 'a -> 'a) msg (st: 'a t) : 'a t =
    let open OpamProcess in
    let path_s = OpamFilename.Dir.to_string st.path in
    match st.head with
    | None -> assert false
    | Some data ->
      let data = sync data in
      let msg = if msg = "" then "-" else msg in
      let () =
        Job.of_list [
          command ~dir:path_s "git" [ "add"; "*"; ];
          command ~dir:path_s "git" [ "commit"; "-a"; "--allow-empty"; "-m"; msg ];
        ]
        |> Job.run
        |> OpamStd.Option.iter (fun (cmd, res) -> must_succeed cmd res)
      in
      { st with head = Some data }
end

module Serialized = struct
  type 'a t = {
    data : 'a;
    path : filename;
  }

  let load_raw ~file : string t =
    { data = OpamSystem.read (OpamFilename.to_string file);
      path = file }

  let sync_raw (s : string t) : string t =
    OpamSystem.write (OpamFilename.to_string s.path) s.data;
    s

  let load_json ~file (of_json : Json.value -> 'a) : 'a t =
    let j = read_json file in
    let j = Json.value j in
    { data = of_json j; path = file }

  let sync_json (s : 'a t) (to_json : 'a -> Json.t) : 'a t =
    let cout = open_out (OpamFilename.to_string s.path) in
    Json.to_channel ~minify:false cout (to_json s.data);
    close_out cout;
    s
end

module SerializedLog = struct
  type 'a t = {
    old_data : 'a list;
    new_data : 'a list;
    path : filename;
  }

  let items t =
    List.rev_append t.new_data t.old_data

  let add_item item t =
    { t with new_data = item :: t.new_data }

  let add_items items t =
    { t with new_data = List.rev_append items t.new_data }

  let load_json ~file (of_json: Json.t -> 'a): 'a t =
    let data =
      let entries = Yojson.Basic.stream_from_file (OpamFilename.to_string file) in
      let r = ref [] in
      let of_yojson x =
        match value_of_yojson x with
          | #Json.t as x -> of_json x
          | not_toplevel ->
             fatal "Invalid toplevel JSON value in %S:\n%s"
               (OpamFilename.to_string file)
               (Json.value_to_string not_toplevel)
      in
      Stream.iter (fun x -> r := of_yojson x :: !r) entries;
      List.rev !r
    in
    {
      old_data = data;
      new_data = [];
      path = file;
    }

  let sync_json (type a) (t : a t) (to_json: a -> Json.t): a t =
    CCIO.with_out_a (OpamFilename.to_string t.path) (fun chan ->
      let ppf = Format.formatter_of_out_channel chan in
      List.iter (fun item ->
          Format.fprintf ppf "%a@."
            (Yojson.Basic.pretty_print ?std:None)
            (yojson_of_value (to_json item :> Json.value)))
        (List.rev t.new_data));
    {
      old_data = items t;
      new_data = [];
      path = t.path;
    }
end

type timestamp = string (* git hash *)
type build_log = string list
type changes = Changes (* fixme *)
type error_cause = [ `Fetch | `Build | `Install ]

type package_report =
  | Success of { log : build_log; changes : changes }
  | Error of { log : build_log; cause : error_cause }
  | Aborted of { deps : Action.Set.t }
  (* An [Aborted] status means that the package could not be built
     because _for all possible ways of building the package_
     at least one of its dependencies fails to build. *)

let package_report_of_json (j: Json.value): package_report =
  let l = Json.get_dict j in
  try
    begin match Json.get_string (List.assoc "status" l) with
    | "success" ->
      Success { log = Json.get_list Json.get_string (List.assoc "log" l);
                changes = Changes (* TODO *) }
    | "error" ->
      let cause = match Json.get_string (List.assoc "cause" l) with
        | "fetch" -> `Fetch
        | "build" -> `Build
        | "install" -> `Install
        | _ -> raise Not_found
      in
      Error { log = Json.get_list Json.get_string (List.assoc "log" l); cause }
    | "aborted" ->
      let deps = match Action.Set.of_json (List.assoc "deps" l) with
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
         ("log", Json.strings log) ]
  | Error { log; cause } ->
    let cause_s = match cause with
      | `Fetch -> "fetch"
      | `Build -> "build"
      | `Install -> "install"
    in
    `O [ ("status", `String "error");
         ("log", Json.strings log);
         ("cause", `String cause_s) ]
  | Aborted { deps } ->
    `O [ ("status", `String "aborted");
         ("deps", Action.Set.to_json deps) ]

module Cover = struct
  type elt_report = package_report OpamPackage.Map.t
  type elt = Cover_elt_plan.t * elt_report
  type t = elt list

  let cover_elt_plan_of_json (j: Json.value): Cover_elt_plan.t =
    try
      let l = Json.get_dict j in
      let get_opt = function Some x -> x | None -> raise Not_found in
      { solution =
          OpamSolver.solution_of_json (List.assoc "solution" l)
          |> get_opt;
        useful =
          PkgSet.of_json (List.assoc "useful" l) |> get_opt; }
    with Not_found -> Json.parse_error j "invalid cover element"

  let cover_elt_plan_to_json plan =
    `O [ ("solution", OpamSolver.solution_to_json plan.Cover_elt_plan.solution);
         ("useful", PkgSet.to_json plan.Cover_elt_plan.useful) ]

  let cover_elt_report_of_json (j : Json.value): elt_report =
    let report_of_json j = Some (package_report_of_json j) in
    match OpamPackage.Map.of_json report_of_json j with
    | Some r -> r
    | None -> Json.parse_error j "invalid cover report"

  let cover_elt_report_to_json (report: elt_report) =
    OpamPackage.Map.to_json package_report_to_json report

  let elt_of_json (j: Json.value): elt =
    Json.get_pair cover_elt_plan_of_json cover_elt_report_of_json j

  let elt_to_json (elt: elt): Json.value =
    Json.pair cover_elt_plan_to_json cover_elt_report_to_json elt

  let of_json (j: Json.value): t =
    Json.get_list elt_of_json j

  let to_json (cover : t): Json.t =
    Json.list elt_to_json cover
end

module Cover_state = struct
  type report_item = OpamPackage.t * package_report

  type t = {
    timestamp : timestamp Serialized.t;
    past_elts : Cover.t Serialized.t;
    cur_plan : Cover_elt_plan.t option Serialized.t;
    cur_report : report_item SerializedLog.t;
    uninst: OpamPackage.Set.t Serialized.t;
  }

  let nonbuilt_useful_packages plan report =
    List.fold_left
      (fun set (package, _) -> PkgSet.remove package set)
      plan.Cover_elt_plan.useful
      report

  let broken_packages st =
    let select_broken (pkg, report) =
      match report with
      | Error _ -> Some pkg
      | Success _ | Aborted _ -> None in
    let elt_broken (_sol, report) =
      PkgMap.to_seq report
      |> Seq.filter_map select_broken
      |> PkgSet.of_seq
    in
    let cur_broken =
      CCList.filter_map select_broken (SerializedLog.items st.cur_report)
      |> PkgSet.of_list in
    List.fold_left
      (fun set elt -> PkgSet.union set (elt_broken elt))
      cur_broken st.past_elts.data

  (* we want the *resolved* packages: succeeded, failed, and uninstallable *)
  let select_resolved (pkg, report) =
    match report with
      | Success _ | Error _ -> Some pkg
      | Aborted _ -> None

  let past_resolved_packages st =
    let resolved report =
      PkgMap.to_seq report
      |> Seq.filter_map select_resolved
      |> PkgSet.of_seq
    in
    List.fold_left
      (fun set (_elt, report) -> PkgSet.union set (resolved report))
      st.uninst.data
      st.past_elts.data

  let resolved_packages st =
    let cur_resolved =
      SerializedLog.items st.cur_report
      |> List.to_seq
      |> Seq.filter_map select_resolved
      |> PkgSet.of_seq
    in
    PkgSet.union cur_resolved (past_resolved_packages st)

  let create ~dir ~timestamp =
    let open OpamFilename in
    { timestamp = { data = timestamp; path = Op.(dir // timestamp_path) };
      past_elts = { data = []; path = Op.(dir // past_elts_path) };
      cur_plan = { data = None; path = Op.(dir // cur_plan_path) };
      cur_report = { old_data = []; new_data = []; path = Op.(dir // cur_report_path) };
      uninst = { data = PkgSet.empty; path = Op.(dir // uninst_path) };
    }

  let archive_report report =
    List.fold_left (fun archive (package, package_report) ->
        PkgMap.add package package_report archive
      ) PkgMap.empty report

  let archive_cur_elt (st: t): Cover.elt * t =
    match st.cur_plan.data with
    | None -> assert false
    | Some plan ->
      let report = archive_report (SerializedLog.items st.cur_report) in
      let elt = (plan, report) in
      elt,
      { st with
        past_elts = { st.past_elts with data = elt :: st.past_elts.data };
        cur_plan = { data = None; path = st.cur_plan.path };
        cur_report = { old_data = []; new_data = []; path = st.cur_report.path };
      }

  let add_item_to_report item (st: t): t =
    { st with cur_report = SerializedLog.add_item item st.cur_report }

  let add_items_to_report items (st: t): t =
    { st with cur_report = SerializedLog.add_items items st.cur_report }

  (* This assumes that the files already exist on the filesystem in a valid
     state *)
  let load ~dir : t =
    let open OpamFilename in
    let assoc k l =
      try List.assoc k l with Not_found ->
        Json.parse_error `Null "" (* XX *) in
    let get_opt =
      function Some x -> x | None -> Json.parse_error `Null "" (* XX *) in
    let report_item_of_json j =
      let j = Json.value j in
      let l = Json.get_dict j in
      let pkg = assoc "package" l in
      let pkg_report = assoc "report" l in
      (get_opt (OpamPackage.of_json pkg),
       package_report_of_json pkg_report)
    in
    let cur_elt_plan_of_json j =
      match j with
      | `A [] -> None
      | `A [ j' ] -> Some (Cover.cover_elt_plan_of_json j')
      | _ ->
        fatal "In %s: invalid format"
          OpamFilename.(prettify Op.(dir // cur_plan_path))
    in
    let uninst_of_json j =
      get_opt (PkgSet.of_json (Json.get_dict j |> assoc "uninst")) in
    {
      timestamp = Serialized.load_raw ~file:Op.(dir // timestamp_path);
      past_elts = Serialized.load_json ~file:Op.(dir // past_elts_path) Cover.of_json;
      cur_plan =
        Serialized.load_json ~file:Op.(dir // cur_plan_path) cur_elt_plan_of_json;
      cur_report =
        SerializedLog.load_json ~file:Op.(dir // cur_report_path) report_item_of_json;
      uninst = Serialized.load_json ~file:Op.(dir // uninst_path) uninst_of_json;
    }

  let sync state : t =
    let report_item_to_json (pkg, pkg_report) =
      `O [ ("package", OpamPackage.to_json pkg);
           ("report", package_report_to_json pkg_report) ]
    in
    let cur_plan_to_json = function
      | None -> `A []
      | Some elt -> `A [ Cover.cover_elt_plan_to_json elt ] in
    let uninst_to_json set = `O [ "uninst", PkgSet.to_json set ] in

    let timestamp = Serialized.sync_raw state.timestamp in
    let past_elts = Serialized.sync_json state.past_elts Cover.to_json in
    let cur_plan = Serialized.sync_json state.cur_plan cur_plan_to_json in
    let cur_report = SerializedLog.sync_json state.cur_report report_item_to_json in
    let uninst = Serialized.sync_json state.uninst uninst_to_json in
    { timestamp; past_elts; cur_plan; cur_report; uninst }
end

module Switch_state = struct
  type t = {
    path : dirname;
    log : filename;
    cover_state_repo : Cover_state.t Repo.t;
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
      mkdir Op.(switch_dir / cover_state_repo_path);
      mkdir Op.(switch_dir / past_timestamps_path);
      let cover_state_repo =
        Repo.load_and_clean
          ~path:Op.(switch_dir / cover_state_repo_path)
          ~load:Cover_state.load
      in
      { path = switch_dir;
        log = Op.(switch_dir // log_path);
        cover_state_repo;
        past_timestamps = Op.(switch_dir / past_timestamps_path); }

    let sync ~workdir state =
      let _ = workdir, state in assert false
  end

  module View_all = struct
    type t = Switch_state.t PkgMap.t
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
