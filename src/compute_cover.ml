open Marracheck_lib

let log_inline fmt = Printf.eprintf (fmt ^^ "%!")
let log fmt = Printf.eprintf (fmt ^^ "\n%!")

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
    log "all (installable) packages: %d" (Lib.card all_packages);
    log "all (installable) packages last version: %d"
      (Lib.card all_packages_last_version);

    let (elts, uninst) = Lib.compute_cover u all_packages in
    CCIO.with_out Lib.dump_file (fun cout -> output_value cout (elts, uninst));
    log_inline "\n";
    List.iter (fun cover_elt ->
      log_inline "(%d|%d) "
        (Lib.card (Lib.installable cover_elt))
        (Lib.card cover_elt.useful)
    ) elts;
    log_inline "\n";
    match uninst with
    | Ok () -> ()
    | Error uninst ->
      log "uninstallable: %d" (List.length uninst)
  )
