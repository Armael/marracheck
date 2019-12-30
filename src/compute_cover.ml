open Marracheck_lib

(* benchmark utility *)

let log_inline fmt = Printf.eprintf (fmt ^^ "%!")
let log fmt = Printf.eprintf (fmt ^^ "\n%!")

let get_universe switch =
  let u = OpamSwitchState.universe switch
      ~requested:OpamPackage.Name.Set.empty
      OpamTypes.Query (* Louis: for historical reasons, should not matter nowadays *)
  in
  (* Lib.universe_exclude_cycles *)
  { u with u_installed = u.u_base; }

let card = OpamPackage.Set.cardinal

(****************************)
(* Custom solver setup *)

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

(****************************)

let compute_cover_batch ~universe ~packages =
  let rec loop elts to_install =
    let elt, remaining =
      Lib.compute_cover_elt ~make_request ~universe ~to_install in
    if OpamPackage.Set.is_empty elt.useful then begin
      (* We are done, the remaining packages are uninstallable *)
      assert (OpamPackage.Set.equal to_install remaining);
      List.rev elts, remaining
    end else
      loop (elt :: elts) remaining
  in
  loop [] packages

(****************************)

let () =
  OpamClientConfig.opam_init
    ~solver:(lazy (module SolverWrapper))
    ();
  SolverWrapper.solver := MaxSat;
  let gs = OpamGlobalState.load `Lock_read in
  OpamSwitchState.with_ `Lock_read gs (fun switch ->
    let u = get_universe switch in
    let all_packages = OpamSolver.installable u in
    let all_names = OpamPackage.names_of_packages all_packages in
    let all_packages_last_version =
      OpamPackage.Name.Set.to_seq all_names
      |> OSeq.map (OpamPackage.max_version all_packages)
      |> OpamPackage.Set.of_seq
    in
    log "all (installable) packages: %d" (card all_packages);
    log "all (installable) packages last version: %d"
      (card all_packages_last_version);

    let (elts, uninst) =
      compute_cover_batch ~universe:u ~packages:all_packages in
    CCIO.with_out Lib.dump_file (fun cout -> output_value cout (elts, uninst));
    log_inline "\n";
    List.iter (fun (cover_elt: Lib.cover_elt) ->
      log_inline "(%d|%d) "
        (card (OpamSolver.new_packages cover_elt.solution))
        (card cover_elt.useful)
    ) elts;
    log_inline "\n";
    log "uninstallable: %d" (card uninst)
  )
