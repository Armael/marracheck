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

let get_universe switch =
  let u = OpamSwitchState.universe switch
      ~requested:OpamPackage.Name.Set.empty
      OpamTypes.Query (* Louis: for historical reasons, should not matter nowadays *)
  in
  { u with u_installed = OpamPackage.Set.empty;
           (* u_base = OpamPackage.Set.empty; *)
  }

let make_request u packages =
  let req =
    OpamSolver.request
      ~install:(OpamSolution.eq_atoms_of_packages packages)
      ()
  in
  let res = OpamSolver.resolve u ~orphans:OpamPackage.Set.empty req in
  match res with
  | Success solution ->
    Ok solution
  | Conflicts c ->
    Error c

let card = OpamPackage.Set.cardinal

let t0 = Unix.gettimeofday ()

type cover_elt = {
  solution: OpamSolver.solution;
  useful: OpamPackage.Set.t;
}

let cover_elt_is_empty { useful; _ } =
  (* The solver could produce a solution which installs spurious packages, so we
     do not check that [OpamSolver.solution_is_empty solution].

     If the set of useful packages to install is empty, it is enough to consider
     the element as empty. *)
  OpamPackage.Set.is_empty useful

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

let compute_cover u packages =
  let rec cover u acc to_install =
    Format.printf "%.2f: <to_install size: %d>\n%!"
      (Unix.gettimeofday () -. t0) (List.length to_install);
    let to_install_s = OpamPackage.Set.of_list to_install in
    let solution = make_request u to_install_s in
    Format.printf "%.2f: DONE  %!" (Unix.gettimeofday () -. t0);
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
    | Error _c ->
      failwith "Conflict"
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
