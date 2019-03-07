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

let run () =
  OpamClientConfig.opam_init
    (* ~solver:(lazy (module OpamBuiltinZ3)) *)
    (* ~solver:(lazy (module OpamCudfSolver.Aspcud)) *)
    ~solver:(lazy (module OpamZ3))
    ~best_effort:true
    ();
  let gs = OpamGlobalState.load `Lock_read in
  OpamSwitchState.with_ `Lock_read gs (fun switch ->
    let u = OpamSwitchState.universe switch
        ~requested:OpamPackage.Name.Set.empty
        OpamTypes.Query (* Louis: for historical reasons, should not matter nowadays *)
    in
    let u = { u with u_installed = OpamPackage.Set.empty;
                     (* u_base = OpamPackage.Set.empty; *)
            } in
    let all_packages = u.u_packages in
    let all_names = OpamPackage.names_of_packages all_packages in
    let all_packages_last_version =
      OpamPackage.Name.Set.to_seq all_names
      |> OSeq.map (OpamPackage.max_version all_packages)
      |> OpamPackage.Set.of_seq
    in
    Printf.eprintf "all packages last version: %d\n%!"
      (OpamPackage.Set.cardinal all_packages_last_version);
    Printf.eprintf "all packages: %d\n%!"
      OpamPackage.(Name.Set.cardinal (names_of_packages all_packages));

    let u0 = { u with u_attrs = ["opam-query", all_packages_last_version] } in
    (* best-effort removes the install/upgrade request info *)
    let req = OpamSolver.request () in
    let res = OpamSolver.resolve u0 ~orphans:OpamPackage.Set.empty req in
    match res with
    | Success solution ->
      Printf.eprintf "%s\n" (OpamSolver.(string_of_stats (stats solution)))
    | Conflicts _ ->
      Printf.eprintf "conflict\n"

    (* OpamSolver.dump_universe u stdout *)
  )
