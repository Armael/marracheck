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

let make_request_maxsat ~universe ~to_install =
  let request = OpamSolver.request
      ~install:(OpamSolution.eq_atoms_of_packages to_install) () in
  let time_budget = maxsat_time_budget (card to_install) in

  let cfg = OpamSolverConfig.(!r) in
  OpamSolverConfig.update
    ~solver:(lazy (module MaxSat))
    ~solver_timeout:(Some time_budget)
    ~preprocess:false
    (* important! opam's preprocessing is incorrect for these kind of queries *)
    ();
  let solution = OpamSolver.resolve universe request in
  OpamSolverConfig.(r := cfg);

  match solution with
  | Success solution -> solution
  | Conflicts c ->
    fatal "Conflicts in the solution produced by the custom solver:\n%s\n\
           Make sure that the repository is cycle-free by running `opam admin check --cycles -i`"
      (OpamCudf.string_of_conflicts universe.u_packages(*?*) (fun _ -> "")(*?*) c)
