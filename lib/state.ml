open Utils

(* Definition of the schema *)

module Fmt = struct
  let raw : string Fs.data_format = Fs.mk_data_format ()
  let cover : Data.Cover.t Fs.data_format = Fs.mk_data_format ()
  let cover_elt_plan_opt : Lib.Cover_elt_plan.t option Fs.data_format = Fs.mk_data_format ()
  let package_report : Data.Package_report.t Fs.data_format = Fs.mk_data_format ()
  let pkg_set : PkgSet.t Fs.data_format = Fs.mk_data_format ()
end

(* NOTE: this is not a recursive definition; the 'rec'/'and' are used to write
   declarations in reverse order (which is more natural here). *)
let rec fs =
  Fs.StaticDir {
    cap = { git = false };
    contents = [
      "cache", ExtDir;
      "switches", DynDir {
        cap = { git = false };
        schema = switch_state;
      };
      "opamroot", ExtDir;
    ]
  }

and switch_state =
  Fs.StaticDir {
    cap = { git = false };
    contents = [
      "cover_state.git", cover_state;
      "past_timestamps", ExtDir;
    ]
  }

and cover_state =
  Fs.StaticDir {
    cap = { git = true };
    contents = [
      "timestamp",
      Fs.file_raw ~format:Fmt.raw ~default:None
      ;
      "past_elts.json",
      Fs.file_json ~format:Fmt.cover
        ~default:(Some [])
        ~of_json:Data.Cover.of_json
        ~to_json:Data.Cover.to_json
      ;
      "cur_plan.json",
      Fs.file_json ~format:Fmt.cover_elt_plan_opt
        ~default:(Some None)
        ~of_json:Data.Cover_elt_plan.opt_of_json
        ~to_json:Data.Cover_elt_plan.opt_to_json
      ;
      "cur_report.json",
      Fs.appendfile_json ~format:Fmt.package_report
        ~of_json:Data.Package_report.of_json
        ~to_json:Data.Package_report.to_json
      ;
      "uninst.json",
      Fs.file_json ~format:Fmt.pkg_set
        ~default:(Some PkgSet.empty)
        ~of_json:Data.Uninst.of_json
        ~to_json:Data.Uninst.to_json
      ;
    ]
  }

(* paths *)

module Db = Fs.Make(struct let schema = fs end)

type t = Db.t
type plain = Fs.plain
type 'a append = 'a Fs.append
type ('a, 'k) file = ('a, 'k) Fs.file
type git = Fs.git
type 'k dir = 'k Fs.dir
type 'k path = 'k Db.path

let load ~workdir = Db.load ~root:workdir
let get_workdir = Db.get_root
let read = Db.read
let write = Db.write
let append = Db.append
let commit = Db.commit
let recreate = Db.recreate
let mkdir = Db.mkdir
let remove = Db.remove
let exists = Db.exists

let p ~workdir path =
  List.fold_left Filename.concat workdir @@
  Db.path_get_raw path

let d ~workdir path =
  OpamFilename.Dir.of_string (p ~workdir path)

let f ~workdir path =
  OpamFilename.of_string (p ~workdir path)

let cache_path =
  Db.path DirPath ["cache"]

let opamroot_path =
  Db.path DirPath ["opamroot"]

let past_timestamps_path ~switch =
  Db.path DirPath ["switches"; switch; "past_timestamps"]

let cover_state_path ~switch =
  Db.path GitDirPath ["switches"; switch; "cover_state.git"]

let timestamp_path ~switch =
  Db.path (FilePath Fmt.raw)
    ["switches"; switch; "cover_state.git"; "timestamp"]

let past_elts_path ~switch =
  Db.path (FilePath Fmt.cover)
    ["switches"; switch; "cover_state.git"; "past_elts.json"]

let cur_plan_path ~switch =
  Db.path (FilePath Fmt.cover_elt_plan_opt)
    ["switches"; switch; "cover_state.git"; "cur_plan.json"]

let cur_report_path ~switch =
  Db.path (AppendFilePath Fmt.package_report)
    ["switches"; switch; "cover_state.git"; "cur_report.json"]

let uninst_path ~switch =
  Db.path (FilePath Fmt.pkg_set)
    ["switches"; switch; "cover_state.git"; "uninst.json"]

(* higher-level queries *)

let nonbuilt_useful_packages db ~switch plan =
  List.fold_left
    (fun set (package, _) -> PkgSet.remove package set)
    plan.Lib.Cover_elt_plan.useful
    (Db.read db @@ cur_report_path ~switch)

(* we want the *resolved* packages: succeeded, failed, and uninstallable *)
let select_resolved (pkg, report) =
  match report with
  | Data.Package_report.Success _ | Error _ -> Some pkg
  | Aborted _ -> None

let past_resolved_packages db ~switch =
  let resolved report =
    PkgMap.to_seq report
    |> Seq.filter_map select_resolved
    |> PkgSet.of_seq
  in
  List.fold_left (fun set (_, report) -> PkgSet.union set (resolved report))
    (Db.read db (uninst_path ~switch))
    (Db.read db (past_elts_path ~switch))

let resolved_packages db ~switch =
  let cur_resolved =
    Db.read db (cur_report_path ~switch)
    |> CCList.filter_map select_resolved
    |> PkgSet.of_list
  in
  PkgSet.union cur_resolved (past_resolved_packages db ~switch)

let broken_packages db ~switch =
  let select_broken (pkg, report) =
    match report with
    | Data.Package_report.Error _ -> Some pkg
    | Success _ | Aborted _ -> None in
  let elt_broken (_sol, report) =
    PkgMap.to_seq report
    |> Seq.filter_map select_broken
    |> PkgSet.of_seq
  in
  let cur_broken =
    CCList.filter_map select_broken (Db.read db (cur_report_path ~switch))
    |> PkgSet.of_list in
  List.fold_left
    (fun set elt -> PkgSet.union set (elt_broken elt))
    cur_broken
    (Db.read db (past_elts_path ~switch))

let map_of_report report =
  List.fold_left (fun map (package, package_report) ->
    PkgMap.add package package_report map
  ) PkgMap.empty report

let archive_cur_elt db ~switch =
  match Db.read db (cur_plan_path ~switch) with
  | None -> None
  | Some plan ->
    let report = map_of_report (Db.read db (cur_report_path ~switch)) in
    let elt = (plan, report) in
    let past_elts = Db.read db (past_elts_path ~switch) in
    Db.write db (past_elts_path ~switch) (elt :: past_elts);
    Db.write db (cur_plan_path ~switch) None;
    Db.write db (cur_report_path ~switch) [];
    Some elt
