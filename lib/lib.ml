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

let make_request u packages =
  let res =
    match !SolverWrapper.solver with
    | SolverWrapper.Z3_lp | Z3_wmax | MaxSat ->
      let req =
        OpamSolver.request
          ~install:(OpamSolution.eq_atoms_of_packages packages)
          ()
      in
      OpamSolver.resolve u ~orphans:OpamPackage.Set.empty req

    | SolverWrapper.Default | Z3_default ->
      let req = OpamSolver.request () in
      let u = { u with u_attrs = ["opam-query", packages] } in
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
    let cycles = OpamCudf.conflict_cycles c in
    assert (cycles <> []);
    let cycles = List.map (fun l ->
      List.map (fun s ->
        (* disgusting hack *)
        let i = ref 0 in
        let is_alpha c =
          Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' in
        while not (is_alpha s.[!i]) do incr i done;
        let s' = String.sub s !i (String.length s - !i) in
        log "parsing: %s" s;
        log "after strip: %s" s';
        OpamPackage.of_string s'
      ) l
      |> List.sort_uniq OpamPackage.compare
    ) cycles in
    Error cycles

let card = OpamPackage.Set.cardinal

type cover_elt = {
  solution: OpamSolver.solution;
  useful: OpamPackage.Set.t;
}

let pp_cover_elt fmt { solution; useful } =
  Format.fprintf fmt "{inst:%d, useful:%d}"
    (card (OpamSolver.new_packages solution))
    (card useful)

let installable { solution; _ } =
  OpamSolver.new_packages solution

let version_weights all_packages : float OpamPackage.Map.t =
  OpamPackage.to_map all_packages
  |> OpamPackage.Name.Map.to_seq
  |> OSeq.map (fun (pkg_name, versions) ->
    let versions_l =
      OpamPackage.Version.Set.elements versions
      |> List.stable_sort OpamPackage.Version.compare in
    let incr = 1. /. float (List.length versions_l) in
    versions_l
    |> List.mapi (fun i v ->
      (OpamPackage.create pkg_name v, float (i+1) *. incr))
    |> OSeq.of_list
  )
  |> OSeq.flatten
  |> OpamPackage.Map.of_seq

let maxsat_time_budget = 30.
let maxsat_time_budget_min = 1.

let compute_cover u packages =
  let budget_per_pkg =
    (maxsat_time_budget -. maxsat_time_budget_min) /. (float (card packages))
  in
  let rec cover u acc to_install =
    Format.printf "%.2f: <to_install size: %d>\n%!"
      (time ()) (List.length to_install);
    let to_install_s = OpamPackage.Set.of_list to_install in
    if !SolverWrapper.solver = MaxSat then begin
      let time_budget =
        maxsat_time_budget_min +.
        budget_per_pkg *. (float (card to_install_s))
      in
      log "time budget: %f" time_budget;
      OpamSolverConfig.update ~solver_timeout:(Some time_budget) ()
    end;
    let solution = make_request u to_install_s in
    log "DONE";
    match solution with
    | Ok solution ->
      let installable = OpamSolver.new_packages solution in
      let useful = OpamPackage.Set.inter to_install_s installable in
      Format.printf "%a\n\n%!" pp_cover_elt { solution; useful };
      if OpamPackage.Set.is_empty useful then begin
        if acc = [] then
          (* Return the trivial cover element in that case; the cover
             must always be a non-empty list. *)
          ([{solution; useful}], Error to_install)
        else
          (List.rev acc, Error to_install)
      end else
        let acc' = { solution; useful } :: acc in
        let to_install' =
          List.filter (fun pkg -> not (OpamPackage.Set.mem pkg installable))
            to_install
        in
        if to_install' = [] then (List.rev acc', Ok ())
        else cover u acc' to_install'
    | Error cycles ->
      log "Dependency cycle(s) in the solution";
      List.iter (fun l ->
        print_endline (
          OpamStd.Format.pretty_list ~last:","
            (List.map OpamPackage.to_string l)
        )
      ) cycles;

      let u = List.fold_left (fun u cycle ->
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
      ) u cycles in
      log "Restarting query after adding the cycles as conflicts";
      cover u acc to_install
  in
  let vw = version_weights packages in
  let packages_list =
    OpamPackage.Set.elements packages
    |> List.stable_sort (fun p1 p2 ->
      Float.compare (OpamPackage.Map.find p2 vw) (OpamPackage.Map.find p1 vw))
  in
  cover u [] packages_list

let dump_file = "last_run.dump"

let repair_cover u cover broken_pkgs =
  CCList.flat_map (fun elt ->
    let elt_pkgs = installable elt in
    let elt_broken = OpamPackage.Set.inter elt_pkgs broken_pkgs in
    if OpamPackage.Set.is_empty elt_broken then
      [elt]
    else
      let elt_pkgs = OpamPackage.Set.diff elt_pkgs elt_broken in
      let (elt_new_cover, uninst) = compute_cover u elt_pkgs in
      assert (uninst = Ok ());
      assert (List.length elt_new_cover >= 1);
      Format.printf "Repaired: %a - %d broken -> %a@."
        pp_cover_elt elt
        (card elt_broken)
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           pp_cover_elt)
        elt_new_cover;
      elt_new_cover
  ) cover
