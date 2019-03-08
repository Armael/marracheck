(*
 - OpamGlobalState
 - OpamSwitchState
 - universe
 - OpamSolver
 - request
 - resolve
 - atomic_actions

  best_effort:
  OpamSolver.request vide le wish install
  requested : on met ce qu'on veut installer


  simuler best effort:

  univers + patcher le champ u_attrs avec "opam-query" + liste des paquets à installer en best-effort
    dans la requête, ajouter le critère "+count[opam-query:,false]"
    en le settant avec OpamSolverConfig

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
  let req = OpamSolver.request
      ~install:(OpamSolution.eq_atoms_of_packages packages)
      ()
  in
  let res = OpamSolver.resolve u ~orphans:OpamPackage.Set.empty req in
  match res with
  | Success solution ->
    Ok (OpamSolver.new_packages solution)
  | Conflicts c ->
    Error c

let card = OpamPackage.Set.cardinal

let run () =
  OpamClientConfig.opam_init
    (* ~solver:(lazy (module OpamBuiltinZ3)) *)
    (* ~solver:(lazy (module OpamCudfSolver.Aspcud)) *)
    ~solver:(lazy (module OpamZ3))
    (* ~best_effort:true *)
    ();
  let gs = OpamGlobalState.load `Lock_read in
  OpamSwitchState.with_ `Lock_read gs (fun switch ->
    let u = get_universe switch in
    let all_packages = u.u_packages in
    let all_names = OpamPackage.names_of_packages all_packages in
    let all_packages_last_version =
      OpamPackage.Name.Set.to_seq all_names
      |> OSeq.map (OpamPackage.max_version all_packages)
      |> OpamPackage.Set.of_seq
    in
    Printf.eprintf "all packages last version: %d\n%!" (card all_packages_last_version);
    (* Printf.eprintf "all packages: %d\n%!"
     *   OpamPackage.(Name.Set.cardinal (names_of_packages all_packages)); *)

    let rec cover acc to_install =
      let solution = make_request u to_install in
      Printf.printf ".%!";
      match solution with
      | Ok installable ->
        let useful = OpamPackage.Set.inter to_install installable in
        let useful_nb = card useful in
        Printf.printf "(%d|%d)%!" useful_nb (card installable);
        if useful_nb = 0 then
          (List.rev acc, Error to_install)
        else
          let acc' = installable :: acc in
          if useful_nb = card to_install then
            (List.rev acc', Ok ())
          else
            cover acc' (OpamPackage.Set.diff to_install useful)
      | Error _c ->
        failwith "Conflict"
    in

    let (sets, uninst) = cover [] all_packages_last_version in
    Printf.printf "\n";
    List.iter (fun s -> Printf.printf "%d " (OpamPackage.Set.cardinal s)) sets;
    match uninst with
    | Ok () -> ()
    | Error uninst ->
      Printf.printf "uninstallable: %d\n%s\n"
        (OpamPackage.Set.cardinal uninst)
        (OpamPackage.Set.to_string uninst)
  )
