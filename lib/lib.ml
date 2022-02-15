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
    (* FIXME This may happen if the user asks to install a version that does not
       exist! *)
    assert false

let name_to_cudf name =
  Dose_common.CudfAdd.encode (OpamPackage.Name.to_string name)

let atom2cudf _universe (version_map : int OpamPackage.Map.t) (name,cstr) =
  name_to_cudf name,
  OpamStd.Option.Op.(cstr >>= constraint_to_cudf version_map name)

let map_request f r =
  let open OpamTypes in
  let f = List.rev_map f in
  { wish_install = f r.wish_install;
    wish_remove  = f r.wish_remove;
    wish_upgrade = f r.wish_upgrade;
    wish_all = f r.wish_all;
    criteria = r.criteria;
    extra_attributes = r.extra_attributes; }

let opamcudf_remove universe name constr =
  let filter p =
    p.Cudf.package <> name
    || not (Cudf.version_matches p.Cudf.version constr) in
  let packages = Cudf.get_packages ~filter universe in
  Cudf.load_universe packages

(** Special package used by Dose internally, should generally be filtered out *)
let dose_dummy_request = Dose_algo.Depsolver.dummy_request.Cudf.package

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
        (* NOTE here by default opam does preprocess_cudf_request (in OpamCudf).
           (can be disabled, but enabled by default) is it only an optimisation
           we don't care about? *)
        try
          let r =
            Dose_algo.Depsolver.check_request_using ~call_solver
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
        Dose_algo.Depsolver.Sat (None, Cudf.load_universe [])
    in
    match get_final_universe () with
    | Dose_algo.Depsolver.Sat (_,u) ->
      let u = opamcudf_remove u dose_dummy_request None in
      Cudf.remove_package u OpamCudf.opam_invariant_package;
      OpamTypes.Success u
    | Dose_algo.Depsolver.Error str -> fatal "Solver error: %s" str
    | Dose_algo.Depsolver.Unsat _ -> assert false
  in

  (* OpamSolver.opam_invariant_package *)
  let opamsolver_opam_invariant_package version_map invariant =
    let depends =
      OpamFormula.to_atom_formula invariant
      |> OpamFormula.map (fun at -> Atom (atom2cudf () version_map at))
      |> OpamFormula.cnf_of_formula
      |> OpamFormula.ands_to_list
      |> List.map (OpamFormula.fold_right (fun acc x -> x::acc) [])
    in {
      Cudf.
      package = OpamCudf.opam_invariant_package_name;
      version = snd OpamCudf.opam_invariant_package;
      depends;
      conflicts = [];
      provides = [];
      installed = true;
      was_installed = true;
      keep = `Keep_version;
      pkg_extra = [
        OpamCudf.s_source, `String "SWITCH_INVARIANT";
        OpamCudf.s_source_number, `String "NULL";
      ];
    }
  in

  (* Morally:
     [OpamSolver.resolve univers ~orphans:OpamPackage.Set.empty req] *)
  let open OpamTypes in
  let open OpamPackage.Set.Op in
  let all_packages = universe.u_available ++ universe.u_installed in
  let version_map = OpamSolver.cudf_versions_map universe all_packages in
  let univ_gen = OpamSolver.load_cudf_universe universe ~version_map all_packages in
  let cudf_universe = univ_gen ~build:true ~post:true () in
  let requested_names =
    OpamPackage.Name.Set.of_list (List.map fst request.wish_all) in
  let cudf_request = map_request (atom2cudf universe version_map) request in
  let invariant_pkg =
    opamsolver_opam_invariant_package version_map universe.u_invariant
  in
  let solution =
    try
      Cudf.add_package cudf_universe invariant_pkg;
      let resp = opamcudf_resolve ~cycles ~version_map cudf_universe cudf_request in
      Cudf.remove_package cudf_universe OpamCudf.opam_invariant_package;
      OpamCudf.to_actions cudf_universe resp
    with OpamCudf.Solver_failure msg ->
      let bt = Printexc.get_raw_backtrace () in
      OpamConsole.error "%s" msg;
      Printexc.raise_with_backtrace
        OpamStd.Sys.(Exit (get_exit_code `Solver_failure))
        bt
  in
  match solution with
  | Conflicts _ -> fatal "conflicts in the solution"
  | Success actions ->
    let simple_universe = univ_gen ~depopts:true ~build:false ~post:false () in
    let complete_universe = univ_gen ~depopts:true ~build:true ~post:false () in
    try
      let s = OpamCudf.atomic_actions ~simple_universe ~complete_universe actions in
      OpamCudf.trim_actions cudf_universe requested_names s;
      (Obj.magic s : OpamSolver.solution) (* XXXX *)
    with OpamCudf.Cyclic_actions _cycles ->
      fatal "The custom solver produced a cyclic solution. \
             This should not happen, since we worked to make cycles illegal upfront."
