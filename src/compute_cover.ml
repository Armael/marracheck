open Marracheck_lib

let () =
  OpamClientConfig.opam_init
    ~solver:(lazy (module OpamZ3))
    ();
  let gs = OpamGlobalState.load `Lock_read in
  OpamSwitchState.with_ `Lock_read gs (fun switch ->
    let u = Lib.get_universe switch in
    let all_packages = OpamSolver.installable u in
    let all_names = OpamPackage.names_of_packages all_packages in
    let all_packages_last_version =
      OpamPackage.Name.Set.to_seq all_names
      |> OSeq.map (OpamPackage.max_version all_packages)
      |> OpamPackage.Set.of_seq
    in
    Printf.eprintf "all (installable) packages: %d\n%!" (Lib.card all_packages);
    Printf.eprintf "all (installable) packages last version: %d\n%!"
      (Lib.card all_packages_last_version);

    let (elts, uninst) = Lib.compute_cover u all_packages in
    CCIO.with_out Lib.dump_file (fun cout -> output_value cout (elts, uninst));
    Printf.printf "\n";
    List.iter (fun cover_elt ->
      Printf.printf "(%d|%d) "
        (Lib.card (Lib.installable cover_elt))
        (Lib.card cover_elt.useful)
    ) elts;
    Printf.printf "\n";
    match uninst with
    | Ok () -> ()
    | Error uninst ->
      Printf.printf "uninstallable: %d\n" (List.length uninst)
  )
