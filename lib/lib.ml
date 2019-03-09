(*
pour setup from scratch:

- OpamStateConfig.update pour mettre l'opamroot qu'on veut
- OpamClient.init qui va populate l'opamroot et retourner le globalstate


installer une solution:
OpamSolution.apply

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

let pp_cover_elt fmt { solution; useful } =
  Format.fprintf fmt "{inst:%d, useful:%d}"
    (card (OpamSolver.new_packages solution))
    (card useful)

let installable { solution; _ } =
  OpamSolver.new_packages solution

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
      if OpamPackage.Set.is_empty useful then
        (List.rev acc, Error to_install)
    else
      let acc' = { solution; useful } :: acc in
      let to_install' =
        List.filter (fun pkg -> not (OpamPackage.Set.mem pkg installable))
          to_install
      in
      if to_install' = [] then (List.rev acc, Ok ())
      else cover u acc' to_install'
  | Error _c ->
    failwith "Conflict"

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

let dump_file = "last_run.dump"

let compute_cover u packages =
    let vw = version_weights packages in
    let packages_list =
      OpamPackage.Set.elements packages
      |> List.stable_sort (fun p1 p2 ->
        Float.compare (OpamPackage.Map.find p2 vw) (OpamPackage.Map.find p1 vw))
    in
    cover u [] packages_list

let install_cover_elt switch { solution; _ } =
  let action_graph = OpamSolver.get_atomic_action_graph solution in
  OpamSolution.parallel_apply switch
    ~requested:OpamPackage.Set.empty
    ~assume_built:false
    action_graph

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
      Format.printf "Repaired: %a -> %a@."
        pp_cover_elt elt
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           pp_cover_elt)
        elt_new_cover;
      elt_new_cover
  ) cover
