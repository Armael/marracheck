open Opamcheck2_lib

let () =
  OpamClientConfig.opam_init
    ~solver:(lazy (module OpamZ3))
    ();
  let gs = OpamGlobalState.load `Lock_read in
  OpamSwitchState.with_ `Lock_read gs (fun switch ->
    let (elts, uninst) = Lib.compute_cover switch in
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
