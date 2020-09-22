open OpamCudfSolverSig

let log f = OpamConsole.log "MaxSat" f

let name = "maxsat"

let ext : string option ref = ref None

let is_present () = true

let command_name = None

(* https://github.com/sbjoshi/Open-WBO-Inc *)
let solver_path = ref "open-wbo-inc_release"

let default_criteria = {
  (* ignored *)
  crit_default = "";
  crit_upgrade = "";
  crit_fixup = "";
  crit_best_effort_prefix = None;
}

module Vec = CCVector

let ( @^ ) opt l = match opt with
  | None -> l
  | Some x -> x :: l

let xrmap f l =
  match List.fold_left (fun acc x -> f x @^ acc) [] l with
  | [] -> None
  | l -> Some l

open OpamStd.Option.Op

let def_packages
    (cycles : Cudf.package list list list)
    (_preamble, universe, _request) :
  (int list, [`RW]) Vec.t *
  (Cudf.package -> int option) *
  (int -> Cudf.package) *
  int
  =
  let pkg_table : (Cudf.package, int) Hashtbl.t = Hashtbl.create 4000 in
  let pkg_rev_table : (int, Cudf.package) Hashtbl.t = Hashtbl.create 4000 in
  let pkg_id p = Hashtbl.find_opt pkg_table p in
  let pkg_of_id id = Hashtbl.find pkg_rev_table id in
  let pkg_id_exn p = match pkg_id p with Some x -> x | None -> raise Not_found in
  let fresh_id = ref 1 in
  let clauses = Vec.create () in

  Cudf.iter_packages (fun pkg ->
    Hashtbl.add pkg_table pkg !fresh_id;
    Hashtbl.add pkg_rev_table !fresh_id pkg;
    incr fresh_id
  ) universe;

  (* "keep" flags *)
  Cudf.iter_packages_by_name (fun _name pkgs ->
    let keep =
      match List.find (fun p -> p.Cudf.keep = `Keep_version) pkgs with
      | p -> pkg_id p >>| fun x -> [x]
      | exception Not_found ->
        if List.exists (fun p -> p.Cudf.keep = `Keep_package) pkgs then
          xrmap pkg_id pkgs
        else None
    in
    OpamStd.Option.iter (Vec.push clauses) keep
  ) universe;

  let expand_constraint pkg (name, constr) =
    xrmap (fun p -> if Cudf.( =% ) pkg p then None else pkg_id p)
      (Cudf.lookup_packages universe ~filter:constr name)
  in

  let implies x l = -x :: l in

  Cudf.iter_packages (fun pkg ->
    let pid = pkg_id_exn pkg in
    (* depends *)
    List.iter (fun disj ->
      CCList.filter_map (expand_constraint pkg) disj |> List.flatten
      |> (fun l -> implies pid l)
      |> Vec.push clauses
    ) pkg.Cudf.depends;

    (* conflicts *)
    List.iter (fun vpkg ->
      expand_constraint pkg vpkg |> OpamStd.Option.iter (fun l ->
      List.iter (fun conflicting ->
        Vec.push clauses (implies pid [- conflicting])
        ) l
      )
    ) pkg.Cudf.conflicts;
  ) universe;

  (* exclude cycles *)
  (* the boolean formula for excluding a cycle is:

     ¬ (P1 /\ P2 /\ .. /\ Pn)
     <=>
     (¬P1 \/ ¬P2 \/ .. \/ ¬Pn)

     where P1..Pn are in fact sets of packages, so disjunctions:

     (¬(P11 \/ P12 \/ .. \/ P1m) \/ ... \/ ¬(Pn1 \/ .. \/ Pnm)
     <=>
     ((¬P11 /\ ¬P12 /\ .. /\ ¬P1m) \/ .. \/ (¬Pn1 /\ .. /\ ¬Pnm))

     i.e. a formula in disjunctive normal form (DNF).

     We therefore need to turn it into CNF...
  *)
  let rec cnf = function
    | [] -> [[]]
    | cycle :: cycles ->
      let cycles' = cnf cycles in
      CCList.flat_map (fun pkg ->
        List.map (fun cl -> pkg :: cl) cycles'
      ) cycle
  in
  List.iter (fun cycle ->
    List.iter (fun cl ->
      Vec.push clauses (List.map (fun pkg -> - (pkg_id_exn pkg)) cl)
    ) (cnf cycle);
  ) cycles;

  clauses, pkg_id, pkg_of_id, (!fresh_id - 1)

let read_pkg_property_value preamble property p =
  match property with
  | None -> 1
  | Some prop ->
    match Cudf.lookup_typed_package_property p prop with
    | `Int n | `Nat n -> n
    | `Bool true -> 1
    | `Bool false -> 0
    | _ -> 0
    | exception Not_found ->
      match List.assoc prop preamble.Cudf.property with
      | `Int (Some n) | `Nat (Some n) -> n
      | `Bool (Some true) -> 1
      | `Bool (Some false) -> 0
      | _ -> 0
      | exception Not_found ->
        failwith ("Undefined CUDF property: "^prop)

let output_wcnf
    (cout: out_channel)
    (hard_clauses: (int list, [`RW]) Vec.t)
    (soft_clauses: (int list * [`Normal of int | `Inverted of int], [`RW]) Vec.t)
  =
  let max_inverted_weight = Vec.fold (fun acc (_, w) ->
    match w with `Normal _ -> acc | `Inverted w -> max acc w) 0 soft_clauses in
  let weight = function
    | `Normal x -> x
    | `Inverted x -> max_inverted_weight + 2 - x
  in
  let weights_sum = Vec.fold (fun acc (_, w) -> acc + weight w) 0 soft_clauses in
  let clause_max_lit = List.fold_left (fun acc x -> max acc (abs x)) 0 in
  let max_lit =
    max
      (Vec.fold (fun acc cl -> max acc (clause_max_lit cl)) 0 hard_clauses)
      (Vec.fold (fun acc (cl, _) -> max acc (clause_max_lit cl)) 0 soft_clauses) in
  let top = weights_sum + 1 in

  Printf.fprintf cout "p wcnf %d %d %d\n" max_lit
    (Vec.size hard_clauses + Vec.size soft_clauses) top;
  Vec.iter (fun cl ->
    List.iter (Printf.fprintf cout "%d ") (top :: cl);
    Printf.fprintf cout "0\n"
  ) hard_clauses;
  Vec.iter (fun (cl, w) ->
    List.iter (Printf.fprintf cout "%d ") (weight w :: cl);
    Printf.fprintf cout "0\n"
  ) soft_clauses

let spawn_lwt ~timeout (cmd: string) =
  let open Lwt.Infix in
  let read_stderr, write_stderr = Lwt_unix.pipe_in ()
  and read_stdout, write_stdout = Lwt_unix.pipe_in ()
  in
  let p = new Lwt_process.process_none
      ~stdout:(`FD_move write_stdout)
      ~stderr:(`FD_move write_stderr)
      (Lwt_process.shell cmd)
  in
  Lwt.on_success (Lwt_unix.sleep timeout) (fun () -> p#kill 15 (* sigterm *));
  Lwt.async (fun () ->
    Lwt_io.read_lines (Lwt_io.of_fd ~mode:Lwt_io.input read_stderr)
    |> Lwt_stream.iter_s (fun line ->
      log "stderr: %s" line;
      Lwt.return ()
    )
  );
  let stdout = Lwt_io.read (Lwt_io.of_fd ~mode:Lwt_io.input read_stdout) in
  p#status >>= (fun ret_code ->
    stdout >>= (fun stdout ->
      Lwt.return (ret_code, stdout)))

let call_solver ~time_budget pkg_of_id hard_clauses soft_clauses max_id =
  let filename, cout = Filename.open_temp_file "marracheck-maxsat" "" in

  log "MaxSat instance in: %s" filename;

  for i = 1 to max_id do
    let pkg = pkg_of_id i in
    Printf.fprintf cout "c %d -> %s.%d\n"
      i pkg.Cudf.package pkg.Cudf.version
  done;

  output_wcnf cout hard_clauses soft_clauses;
  close_out cout;

  begin match OpamStd.Env.getopt "MAXSATDEBUG" with
    | None -> ()
    | Some debug_file ->
      OpamSystem.copy_file filename (debug_file ^ ".wcnf")
  end;

  let (_ret, stdout) =
    Lwt_main.run @@
    spawn_lwt ~timeout:time_budget
      (Printf.sprintf "exec %s %s" !solver_path filename)
  in
  OpamSystem.remove_file filename;

  let answer = CCString.lines stdout |> CCList.last_opt |> CCOpt.get_exn in
  match String.split_on_char ' ' answer with
  | "v" :: cs ->
    let pkgs = CCList.filter_map (fun s ->
      if s = "" then None else
        let x = int_of_string s in
        if x > 0 then (
          let pkg = pkg_of_id x in
          Some { pkg with Cudf.was_installed = pkg.Cudf.installed;
                          Cudf.installed = true }
        ) else None
    ) cs in
    Cudf.load_universe pkgs
  | _ ->
    log "Incorrect solver output";
    exit 1

let inner_call ~time_budget ~cycles (preamble, universe, request as cudf) =
  let hard_clauses, pkg_id, pkg_of_id, max_id = def_packages cycles cudf in
  let soft_clauses = Vec.create () in

  let expand_constraint (name, constr) =
    match constr with
    | Some (`Eq, ver) ->
      let pkg = Cudf.lookup_package universe (name, ver) in
      pkg_id pkg >>| (fun id -> pkg, id)
    | _ -> assert false
  in

  let u' =
    xrmap expand_constraint request.Cudf.install >>| (fun to_install ->
      let to_install_s = List.map snd to_install |> OpamStd.IntSet.of_list in

      List.iter (fun (pkg, id) ->
        let weight = read_pkg_property_value preamble (Some "version-lag") pkg in
        Vec.push soft_clauses ([id], `Inverted weight)
      ) to_install;

      for i = 1 to max_id do
        if not (OpamStd.IntSet.mem i to_install_s) then
          Vec.push soft_clauses ([-i], `Normal 1)
      done;

      call_solver ~time_budget pkg_of_id hard_clauses soft_clauses max_id
    )
    |> CCOpt.get_lazy (fun () ->
      log "(!!) nothing to install";
      universe
    )
  in
  Some preamble, u'

(* In order to satisfy the opam solver API *)
let call ~criteria ?timeout cudf =
  ignore criteria;
  let time_budget = CCOpt.get_lazy (fun () ->
    log "MaxSat needs a time budget";
    raise Exit
  ) timeout in

  inner_call ~time_budget ~cycles:[](*!*) cudf
