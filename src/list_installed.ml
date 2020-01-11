open Marracheck_lib
module Json = Ezjsonm

let () =
  let dir = Sys.argv.(1) in
  let report = State.Cover_state.load ~dir:(OpamFilename.Dir.of_string dir) in
  report.report.data
  |> CCList.filter_map (fun (pkg, st) ->
    match st with
    | State.Success _ -> Some pkg
    | _ -> None
  )
  |> List.iter (fun p -> print_endline (OpamPackage.to_string p))
