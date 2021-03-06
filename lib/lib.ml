open Utils

(*
pour setup from scratch:

- OpamStateConfig.update pour mettre l'opamroot qu'on veut
- OpamClient.init qui va populate l'opamroot et retourner le globalstate
  (et on peut lui donner en paramètre le repo)

installer une solution:
OpamSolution.apply

----

concernant les fichiers changes:

- il faut les générer en ayant fait OpamCoreConfig.update ~precise_tracking:true

  (pour calculer des hashes pour les fichiers, sinon ça calcule des timestamp
  parce qu'on veut juste savoir si des fichiers ont changé)

- pour les récupérer :
  OpamPath.Switch.changes root switch name
  |> OpamFile.Changes.read
*)

let card = OpamPackage.Set.cardinal

let compute_universe_cycles (u: OpamTypes.universe): OpamPackage.Set.t list list =
  let pkgs, cycles = OpamAdminCheck.cycle_check u in
  List.map (fun cycle ->
    List.map (fun f -> OpamFormula.packages pkgs f) cycle
  ) cycles

(********)

module Cover_elt_plan = struct
  type t = {
    solution: OpamSolver.solution;
    useful: OpamPackage.Set.t;
  }

  let installs elt =
    OpamSolver.new_packages elt.solution

  let pp_stats fmt { solution; useful } =
    Format.fprintf fmt "{inst:%d, useful:%d}"
      (card (OpamSolver.new_packages solution))
      (card useful)

  let compute ~make_request ~universe ~to_install =
    log "compute_cover_elt: to_install size = %d" (card to_install);
    let solution = make_request ~universe ~to_install in
    log "DONE";
    let solution_installs = OpamSolver.new_packages solution in
    let useful = OpamPackage.Set.inter to_install solution_installs in
    let elt = { solution; useful } in
    log "cover elt: %a" pp_stats elt;
    (* "Remaining" is the set of packages that we could not install as part of the
       element. We have to make sure to exclude from this set packages that are
       *already installed* in the universe, because they would otherwise be
       eventually counted as uninstallable (they never occur in the "new packages"
       of a solution since they're already installed...). *)
    let remaining = OpamPackage.Set.Op.(
      (to_install -- solution_installs) -- universe.OpamTypes.u_installed
    ) in
    log "remaining: %d" (card remaining);
    elt, remaining

  let to_json (elt: t) =
    `O [
      "solution", OpamSolver.solution_to_json elt.solution;
      "useful", OpamPackage.Set.to_json elt.useful;
    ]
end

(*********)

let maxsat_time_budget_min = 1.
let maxsat_time_budget_per_pkg = 0.005

let maxsat_time_budget nb_pkgs =
  maxsat_time_budget_min +.
  maxsat_time_budget_per_pkg *. (float nb_pkgs)

(****************)
(* Copy-paste (after some simplification) from opamSolver.ml *)

let constraint_to_cudf version_map name (op, v) =
  let nv = OpamPackage.create name v in
  try Some (op, OpamPackage.Map.find nv version_map)
  with Not_found ->
    (* Hopefully this doesn't happen according to the comment in opamSolver.ml
    *)
    assert false

let name_to_cudf name =
  Common.CudfAdd.encode (OpamPackage.Name.to_string name)

let atom2cudf _universe (version_map : int OpamPackage.Map.t) (name,cstr) =
  name_to_cudf name,
  OpamStd.Option.Op.(cstr >>= constraint_to_cudf version_map name)

let map_request f r =
  let open OpamTypes in
  let f = List.rev_map f in
  { wish_install = f r.wish_install;
    wish_remove  = f r.wish_remove;
    wish_upgrade = f r.wish_upgrade;
    criteria = r.criteria;
    extra_attributes = r.extra_attributes; }

let opamcudf_remove universe name constr =
  let filter p =
    p.Cudf.package <> name
    || not (Cudf.version_matches p.Cudf.version constr) in
  let packages = Cudf.get_packages ~filter universe in
  Cudf.load_universe packages

(** Special package used by Dose internally, should generally be filtered out *)
let dose_dummy_request = Algo.Depsolver.dummy_request.Cudf.package

(****************)

let make_request_maxsat ~cycles ~universe ~to_install =
  let request = OpamSolver.request
      ~install:(OpamSolution.eq_atoms_of_packages to_install) () in
  let time_budget = maxsat_time_budget (card to_install) in

  (* Morally: OpamCudf.resolve ~extern:true *)
  let opamcudf_resolve ~cycles ~version_map univ req =
    (* Translate cycles to the cudf level, using the version map *)
    let cycles : Cudf.package list list list =
      CCList.filter_map (fun cycle ->
        let cudf_cycle =
          List.map (fun pkgs ->
            (* A set of pkgs represents a disjunction. It is ok to drop
               non-existent packages; if the disjunction becomes vacuous, then we
               can remove the cycle completely. *)
            OpamPackage.Set.fold (fun pkg acc ->
              match OpamPackage.Map.find_opt pkg version_map with
              | None -> acc
              | Some ver ->
                let pkgname = OpamPackage.name_to_string pkg in
                match Cudf.lookup_package univ (pkgname, ver) with
                | exception Not_found -> acc
                | cudf_pkg -> cudf_pkg :: acc
            ) pkgs []
          ) cycle in
        if List.for_all ((<>) []) cudf_cycle then Some cudf_cycle else None
      ) cycles
    in

    let get_final_universe () =
      let cudf_request = OpamCudf.to_cudf univ req in
      if Cudf.universe_size univ > 0 then
        let call_solver cudf = MaxSat.inner_call ~cycles ~time_budget cudf in
        try
          let r =
            Algo.Depsolver.check_request_using ~call_solver
              ~explain:true cudf_request in
          r
        with
        | Failure msg ->
          raise (OpamCudf.Solver_failure ("Solver failure: " ^ msg))
        | e ->
          OpamStd.Exn.fatal e;
          let msg =
            Printf.sprintf "Solver failed: %s" (Printexc.to_string e) in
          raise (OpamCudf.Solver_failure msg)
      else
        Algo.Depsolver.Sat (None, Cudf.load_universe [])
    in
    match get_final_universe () with
    | Algo.Depsolver.Sat (_,u) ->
      OpamTypes.Success (opamcudf_remove u dose_dummy_request None)
    | Algo.Depsolver.Error str -> fatal "Solver error: %s" str
    | Algo.Depsolver.Unsat _ -> assert false
  in

  (* Morally:
     [OpamSolver.resolve univers ~orphans:OpamPackage.Set.empty req] *)
  let open OpamTypes in
  let open OpamPackage.Set.Op in
  let all_packages = universe.u_available ++ universe.u_installed in
  let version_map = OpamSolver.cudf_versions_map universe all_packages in
  let univ_gen = OpamSolver.load_cudf_universe universe ~version_map all_packages in
  let simple_universe = univ_gen ~build:true ~post:true () in
  let cudf_request = map_request (atom2cudf universe version_map) request in
  let resolve u req =
    try
      let resp = opamcudf_resolve ~cycles ~version_map u req in
      OpamCudf.to_actions (fun u -> u) u resp
    with OpamCudf.Solver_failure msg ->
      OpamConsole.error_and_exit `Solver_failure "%s" msg
  in
  match resolve simple_universe cudf_request with
  | Conflicts _ -> fatal "conflicts in the solution"
  | Success actions ->
    let simple_universe = univ_gen ~depopts:true ~build:false ~post:false () in
    let complete_universe = univ_gen ~depopts:true ~build:true ~post:false () in
    try
      let s = OpamCudf.atomic_actions ~simple_universe ~complete_universe actions in
      (Obj.magic s : OpamSolver.solution) (* XXXX *)
    with OpamCudf.Cyclic_actions _cycles ->
      assert false
