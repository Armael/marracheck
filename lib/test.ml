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

let t0 = Unix.gettimeofday ()

let rec cover ~slice_size u acc acc_uninst to_install =
  let slice, to_install_rest = CCList.take_drop slice_size to_install in
  let slice_s = OpamPackage.Set.of_list slice in
  Printf.printf "< %.2f \n%!" (Unix.gettimeofday () -. t0);
  let solution = make_request u slice_s in
  Printf.printf "> %.2f \n%!" (Unix.gettimeofday () -. t0);
  match solution with
  | Ok installable ->
    let useful = OpamPackage.Set.inter slice_s installable in
    let useful_nb = card useful in
    let useful_for_later =
      List.filter (fun pkg -> OpamPackage.Set.mem pkg installable)
        to_install_rest in
    Printf.printf "(%d|%d|%d) %d\n%!"
      useful_nb (card installable) (List.length useful_for_later)
      (List.fold_left (+) 0 @@ List.map snd acc);
    let acc', acc_uninst', to_install_rest' =
      if useful_nb = 0 then acc, slice_s :: acc_uninst, to_install_rest
      else
        let acc' = (installable, card useful + List.length useful_for_later) :: acc in
        let slice_rest =
          OpamPackage.Set.diff slice_s useful
          |> OpamPackage.Set.to_seq
          |> OSeq.to_list in
        let to_install_rest' =
          List.filter (fun pkg -> not (OpamPackage.Set.mem pkg installable))
            to_install_rest in
        acc', acc_uninst, slice_rest @ to_install_rest'
    in
    if to_install_rest' = [] then
      (List.rev acc', List.rev acc_uninst')
    else
      cover ~slice_size u acc' acc_uninst' to_install_rest'
  | Error _c ->
    failwith "Conflict"

let version_weights all_packages : float OpamPackage.Map.t =
  OpamPackage.to_map all_packages
  |> OpamPackage.Name.Map.to_seq
  |> OSeq.map (fun (pkg_name, versions) ->
    let versions_l =
      OpamPackage.Version.Set.to_seq versions |> OSeq.to_list
      |> List.stable_sort OpamPackage.Version.compare in
    let incr = 1. /. float (List.length versions_l) in
    versions_l
    |> List.mapi (fun i v ->
      (OpamPackage.create pkg_name v, float (i+1) *. incr))
    |> OSeq.of_list
  )
  |> OSeq.flatten
  |> OpamPackage.Map.of_seq

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
    (* let all_packages = u.u_packages in *)
    let all_packages = OpamSolver.installable u in
    let all_names = OpamPackage.names_of_packages all_packages in
    let all_packages_last_version =
      OpamPackage.Name.Set.to_seq all_names
      |> OSeq.map (OpamPackage.max_version all_packages)
      |> OpamPackage.Set.of_seq
    in
    Printf.eprintf "all packages: %d\n%!" (card all_packages);
    (* Printf.eprintf "all installable packages: %d\n%!" (card (OpamSolver.installable u)); *)
    Printf.eprintf "all packages last version: %d\n%!" (card all_packages_last_version);
    (* Printf.eprintf "all packages: %d\n%!"
     *   OpamPackage.(Name.Set.cardinal (names_of_packages all_packages)); *)

    let vw = version_weights all_packages in
    let all_packages_list =
      OpamPackage.Set.to_seq all_packages
      |> OSeq.to_list
      |> List.stable_sort (fun p1 p2 ->
        Float.compare (OpamPackage.Map.find p2 vw) (OpamPackage.Map.find p1 vw))
    in

    let (sets, uninst) =
      cover ~slice_size:(card all_packages_last_version)
        u [] [] all_packages_list
    in
    Printf.printf "\n";
    List.iter (fun (s, su) -> Printf.printf "(%d|%d) "
                  (OpamPackage.Set.cardinal s) su
              ) sets;
    Printf.printf "\nuninstallable:\n";
    List.iter (fun s -> Printf.printf "%d " (OpamPackage.Set.cardinal s)) uninst;
    Printf.printf "\n"
  )
