open Marracheck_lib
open Lib

(* benchmark utility *)

let log_inline fmt = Printf.eprintf (fmt ^^ "%!")
let log fmt = Printf.eprintf (fmt ^^ "\n%!")

let get_universe switch =
  let u = OpamSwitchState.universe switch
      ~requested:OpamPackage.Set.empty
      OpamTypes.Query (* Louis: for historical reasons, should not matter nowadays *)
  in
  { u with u_installed = u.u_base; }

let card = OpamPackage.Set.cardinal

(* elts_bound = -1 || elts_bound > 0 *)
let compute_cover_batch ~cycles ~universe ~packages ~elts_bound =
  let rec loop elts to_install elts_bound =
    let elt, remaining =
      Cover_elt_plan.compute
        ~make_request:(make_request_maxsat ~cycles)
        ~universe ~to_install
    in
    if OpamPackage.Set.is_empty elt.Cover_elt_plan.useful || elts_bound = 1 then begin
      (* We are done, the remaining packages are uninstallable *)
      assert (OpamPackage.Set.equal to_install remaining);
      List.rev elts, remaining
    end else
      loop (elt :: elts) remaining (elts_bound-1)
  in
  loop [] packages elts_bound

(****************************)

let json_of_cover (elts, uninst) =
  `O [
    "elts", `A (List.map Cover_elt_plan.to_json elts);
    "uninst", OpamPackage.Set.to_json uninst;
   ]

let out_file = ref None
let elts_bound = ref (-1)

let () =
  let usage = Printf.sprintf "usage: %s [--max-elts n] out_file" Sys.argv.(0) in
  Arg.parse [
    "--max-elts", Set_int elts_bound, "Maximum number of elements to compute (default: no bound)";
  ] (fun s -> match !out_file with
              | None -> out_file := Some s
              | Some _ -> raise Exit)
  usage;
  let out_file =
    match !out_file with
    | None -> Printf.eprintf "%s\n" usage; exit 1
    | Some s -> s
  in
  if !elts_bound = 0 then (
    Printf.eprintf "Nothing to do.\n"; exit 1
  );

  OpamClientConfig.opam_init ();
  let gs = OpamGlobalState.load `Lock_read in
  OpamSwitchState.with_ `Lock_read gs (fun switch ->
    let u = get_universe switch in
    log_inline "computing installable packages...";
    let all_packages = OpamSolver.installable u in
    log_inline " Done\n";
    let all_names = OpamPackage.names_of_packages all_packages in
    let all_packages_last_version =
      (* only used for info/debugging *)
      OpamPackage.Name.Set.to_seq all_names
      |> OSeq.map (OpamPackage.max_version all_packages)
      |> OpamPackage.Set.of_seq
    in
    log "all (installable) packages: %d" (card all_packages);
    log "all (installable) packages last version: %d"
      (card all_packages_last_version);

    let universe_cycles = compute_universe_cycles u in
    let (elts, uninst) =
      compute_cover_batch ~cycles:universe_cycles ~universe:u
        ~packages:all_packages ~elts_bound:!elts_bound
    in

    CCIO.with_out out_file (fun cout ->
      CCIO.write_line cout
        (OpamJson.to_string (json_of_cover (elts, uninst)))
    );

    log_inline "\n";
    List.iter (fun (elt: Cover_elt_plan.t) ->
      log_inline "(%d|%d) "
        (card (Cover_elt_plan.installs elt))
        (card elt.Cover_elt_plan.useful)
    ) elts;
    log_inline "\n";
    log "uninstallable: %d" (card uninst)
  )
