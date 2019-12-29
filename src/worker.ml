open Marracheck_lib

let read_cover () : Lib.cover_elt list =
  CCIO.with_in Lib.dump_file input_value |> fst

let print_cover elts =
  Format.printf "@[<v>";
  List.iteri (fun i elt ->
    Format.printf "%d: %a@," i Lib.pp_cover_elt_stats elt
  ) elts;
  Format.printf "@]"

let parse_cmdline () =
  match Sys.argv |> Array.to_list |> List.tl with
  | [] -> `Print_cover
  | ["--install"; nb] ->
    let elts = read_cover () in
    begin match int_of_string nb with
    | n when 0 <= n && n < List.length elts -> `Install n
    | n -> Format.eprintf "Invalid number %d@." n; exit 1
    | exception _ -> Format.eprintf "Not a number: %s@." nb; exit 1
    end
  | _ ->
    Format.printf "usage: %s [--install <n>]@." Sys.argv.(0);
    exit 1

let () =
  match parse_cmdline () with
  | `Print_cover ->
    print_cover (read_cover ())
  | `Install n ->
    let elt = List.nth (read_cover ()) n in
    OpamClientConfig.opam_init ();
    let gs = OpamGlobalState.load `Lock_write in
    let (_switch', _res) =
      OpamSwitchState.with_ `Lock_write gs (fun switch ->
        OpamSolution.apply switch
          ~ask:false
          ~requested:OpamPackage.Name.Set.empty
          ~assume_built:false
          elt.Lib.solution
      ) in
    ()
