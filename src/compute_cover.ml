open Marracheck_lib

(* benchmark utility *)

let log_inline fmt = Printf.eprintf (fmt ^^ "%!")
let log fmt = Printf.eprintf (fmt ^^ "\n%!")

let get_universe switch =
  let u = OpamSwitchState.universe switch
      ~requested:OpamPackage.Name.Set.empty
      OpamTypes.Query (* Louis: for historical reasons, should not matter nowadays *)
  in
  { u with u_installed = u.u_base;
  }

let card = OpamPackage.Set.cardinal

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
      Lib.compute_cover_batch ~universe:u ~packages:all_packages in
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
