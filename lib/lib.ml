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

(* Explicitly mark cyclic solutions as conflicts in the universe.

   This is because a (e.g. boolean) solver will typically allow cyclic
   solutions, even though they will be rejected afterwards. So we compute the
   cycles upfront, and add them as conflicts to prevent them being picked by
   the solver. *)
(*
let universe_exclude_cycles (u: OpamTypes.universe): OpamTypes.universe =
  let open OpamTypes in
  let pkgs_in_cycles, cycles = OpamAdminCheck.cycle_check u in
  let cycles = List.map (fun cycle ->
    match cycle with
    | [] -> assert false
    | f :: fs ->
      let f_pkgs = OpamFormula.packages pkgs_in_cycles f in
      let fs_formula =
        List.fold_left (fun x y -> OpamFormula.And (x, y))
          (List.hd fs) (List.tl fs) in
      (f_pkgs, fs_formula)
  ) cycles in
  List.fold_left (fun u (pkgs, f) ->
    OpamPackage.Set.fold (fun pkg u ->
      { u with u_conflicts = OpamPackage.Map.add pkg f u.u_conflicts }
    ) pkgs u
  ) u cycles
*)

(******* *)

let maxsat_time_budget_min = 1.
let maxsat_time_budget_per_pkg = 0.005

(* Solver-specific hackery goes here *)
let make_request_ ~universe ~to_install =
  if !SolverWrapper.solver = MaxSat then begin
    let time_budget =
      maxsat_time_budget_min +.
      maxsat_time_budget_per_pkg *. (float (card to_install)) in
    log "time budget: %f" time_budget;
    OpamSolverConfig.update ~solver_timeout:(Some time_budget) ()
  end;
  let res =
    match !SolverWrapper.solver with
    | SolverWrapper.Z3_lp | Z3_wmax | MaxSat ->
      let req =
        OpamSolver.request
          ~install:(OpamSolution.eq_atoms_of_packages to_install)
          () in
      OpamSolver.resolve universe ~orphans:OpamPackage.Set.empty req

    | SolverWrapper.Default | Z3_default ->
      let req = OpamSolver.request () in
      let u = { universe with u_attrs = ["opam-query", to_install] } in
      let best_effort_setting = (!OpamSolverConfig.r).best_effort in
      OpamSolverConfig.update ~best_effort:true ();
      let res = OpamSolver.resolve u ~orphans:OpamPackage.Set.empty req in
      OpamSolverConfig.update ~best_effort:best_effort_setting ();
      res
  in
  match res with
  | Success solution ->
    Ok solution
  | Conflicts c ->
    (* TEMPORARY handle possible cycles in the solution *)
    let cycles = OpamCudf.conflict_cycles c in
    assert (cycles <> []);
    let cycles = List.map (fun l ->
      List.map (fun s ->
        (* FIXME: disgusting hack *)
        let i = ref 0 in
        let is_alpha c =
          Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' in
        while not (is_alpha s.[!i]) do incr i done;
        let s' = String.sub s !i (String.length s - !i) in
        OpamPackage.of_string s'
      ) l
      |> List.sort_uniq OpamPackage.compare
    ) cycles in
    Error cycles

let rec make_request ~universe ~to_install =
  (* TEMPORARY: handle cycles in the solution by turning them into conflicts
     and restarting the query *)
  match make_request_ ~universe ~to_install with
  | Ok solution -> solution
  | Error cycles ->
    log "Dependency cycle(s) in the solution";
    List.iter (fun l ->
      print_endline (
        OpamStd.Format.pretty_list ~last:","
          (List.map OpamPackage.to_string l)
      )
    ) cycles;

    let universe = List.fold_left (fun u cycle ->
      match cycle with
      | p1 :: (_ :: _ as ps) ->
        let conflicts = u.OpamTypes.u_conflicts in
        let cycle_conflict =
          OpamFormula.of_conjunction
            (List.map (fun p ->
               OpamPackage.name p, Some (`Eq, OpamPackage.version p))
               ps)
        in
        let p1_conflicts =
          try
            let f = OpamPackage.Map.find p1 conflicts in
            OpamFormula.Or (f, cycle_conflict)
          with Not_found ->
            cycle_conflict
        in
        { u with
          u_conflicts = OpamPackage.Map.add p1 p1_conflicts conflicts }
      | _ ->
        assert false
    ) universe cycles in
    log "Restarting query after adding the cycles as conflicts";
    make_request ~universe ~to_install

type cover_elt = {
  solution: OpamSolver.solution;
  useful: OpamPackage.Set.t;
}

let pp_cover_elt_stats fmt { solution; useful } =
  Format.fprintf fmt "{inst:%d, useful:%d}"
    (card (OpamSolver.new_packages solution))
    (card useful)

let compute_cover_elt ~universe ~to_install =
  log "compute_cover_elt: to_install size = %d" (card to_install);
  let solution = make_request ~universe ~to_install in
  log "DONE";
  let solution_installs = OpamSolver.new_packages solution in
  let useful = OpamPackage.Set.inter to_install solution_installs in
  let elt = { solution; useful } in
  log "cover elt: %a" pp_cover_elt_stats elt;
  (* "Remaining" is the set of packages that we could not install as part of the
     element. We have to make sure to exclude from this set packages that are
     *already installed* in the universe, because they would otherwise be
     eventually counted as uninstallable (they never occur in the "new packages"
     of a solution since they're already installed...). *)
  let remaining = OpamPackage.Set.Op.(
    (to_install -- solution_installs) -- universe.u_installed
  ) in
  log "remaining: %d" (card remaining);
  elt, remaining

(* Only used for benchmarking solver backends *)
let compute_cover_batch ~universe ~packages =
  let rec loop elts to_install =
    let elt, remaining = compute_cover_elt ~universe ~to_install in
    if OpamPackage.Set.is_empty elt.useful then begin
      (* We are done, the remaining packages are uninstallable *)
      assert (OpamPackage.Set.equal to_install remaining);
      List.rev elts, remaining
    end else
      loop (elt :: elts) remaining
  in
  loop [] packages

let dump_file = "last_run.dump"
