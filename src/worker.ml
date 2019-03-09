open Opamcheck2_lib

let read_cover () : Lib.cover_elt list =
  CCIO.with_in Lib.dump_file input_value |> fst

let print_cover elts =
  Format.printf "@[<v>";
  List.iteri (fun i elt ->
    Format.printf "%d: %a@," i Lib.pp_cover_elt elt
  ) elts;
  Format.printf "@]"

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | [] ->
    print_cover (read_cover ())
  | ["--install"; nb] ->
    let elts = read_cover () in
    let n =
      match int_of_string nb with
      | n when 0 <= n && n < List.length elts -> n
      | n -> Format.eprintf "Invalid number %d@." n; exit 1
      | exception _ -> Format.eprintf "Not a number: %s@." nb; exit 1
    in
    let elt = List.nth elts n in
    OpamClientConfig.opam_init ();
    let gs = OpamGlobalState.load `Lock_write in
    let (_switch', _res) =
      OpamSwitchState.with_ `Lock_write gs (fun switch ->
        Lib.install_cover_elt switch elt
      ) in
    ()
  | _ ->
    Format.printf "usage: %s [--install <n>]@." Sys.argv.(0);
    exit 1
