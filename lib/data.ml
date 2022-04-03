open Utils

let assoc k l =
  try List.assoc k l with Not_found ->
    Json.parse_error `Null "" (* XX *)
let get_opt =
  function Some x -> x | None -> Json.parse_error `Null "" (* XX *)

module Package_report = struct
  type build_log = string list
  type changes = Changes (* fixme *)
  type error_cause = [ `Fetch | `Build | `Install ]
  type status =
    | Success of { log : build_log; changes : changes }
    | Error of { log : build_log; cause : error_cause }
    | Aborted of { deps : Action.Set.t }
    (* An [Aborted] status means that the package could not be built
       because _for all possible ways of building the package_
       at least one of its dependencies fails to build. *)
  type t = OpamPackage.t * status

  let status_of_json (j: Json.value): status =
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

  let status_to_json = function
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

  let of_json (j: Json.t) : t =
    let j = Json.value j in
    let l = Json.get_dict j in
    let pkg = assoc "package" l in
    let pkg_report = assoc "report" l in
    (get_opt (OpamPackage.of_json pkg),
     status_of_json pkg_report)

  let to_json (pkg, pkg_report) =
    `O [ ("package", OpamPackage.to_json pkg);
         ("report", status_to_json pkg_report) ]
end

module Cover_elt_plan = struct
  type t = Lib.Cover_elt_plan.t

  let of_json (j: Json.value): t =
    try
      let l = Json.get_dict j in
      let get_opt = function Some x -> x | None -> raise Not_found in
      { solution =
          OpamSolver.solution_of_json (List.assoc "solution" l)
          |> get_opt;
        useful =
          PkgSet.of_json (List.assoc "useful" l) |> get_opt; }
    with Not_found -> Json.parse_error j "invalid cover element"

  let to_json plan =
    `O [ ("solution", OpamSolver.solution_to_json plan.Lib.Cover_elt_plan.solution);
         ("useful", PkgSet.to_json plan.Lib.Cover_elt_plan.useful) ]

  let opt_of_json (j: Json.t) : t option =
    match j with
    | `A [] -> None
    | `A [ j' ] -> Some (of_json j')
    | _ -> Json.parse_error (Json.value j)
             "cover element plan: invalid format"

  let opt_to_json = function
    | None -> `A []
    | Some elt -> `A [ to_json elt ]
end

module Cover = struct
  type elt_report = Package_report.status OpamPackage.Map.t
  type elt = Cover_elt_plan.t * elt_report
  type t = elt list

  let elt_report_of_json (j : Json.value): elt_report =
    let report_of_json j = Some (Package_report.status_of_json j) in
    match OpamPackage.Map.of_json report_of_json j with
    | Some r -> r
    | None -> Json.parse_error j "invalid cover report"

  let elt_report_to_json (report: elt_report) =
    OpamPackage.Map.to_json Package_report.status_to_json report

  let elt_of_json (j: Json.value): elt =
    Json.get_pair Cover_elt_plan.of_json elt_report_of_json j

  let elt_to_json (elt: elt): Json.value =
    Json.pair Cover_elt_plan.to_json elt_report_to_json elt

  let of_json (j: Json.t): t =
    Json.get_list elt_of_json (Json.value j)

  let to_json (cover : t): Json.t =
    Json.list elt_to_json cover
end

module Uninst = struct
  let of_json j =
    let j = Json.value j in
    get_opt (PkgSet.of_json (Json.get_dict j |> assoc "uninst"))

  let to_json set = `O [ "uninst", PkgSet.to_json set ]
end

module Allpkgs = struct
  let of_json = function
    | `O [] -> None
    | `O [("allpkgs", j)] -> Some (get_opt (PkgSet.of_json j))
    | j -> Json.parse_error (Json.value j) "Allpkgs.of_json: invalid format"

  let to_json = function
    | None -> `O []
    | Some set -> `O [("allpkgs", PkgSet.to_json set)]
end
