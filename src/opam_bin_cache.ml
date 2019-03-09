open Bos
open Rresult

let value_of_result = function
  | Ok x -> x
  | Error (`Msg s) -> Printf.eprintf "%s\n" s; exit 1

let exit_of_result r = value_of_result r; exit 0

let read_env v =
  OS.Env.current () >>= fun env ->
  Astring.String.Map.find v env
  |> R.of_option
    ~none:(fun () ->
      R.error_msgf "missing %s environment variable" v) >>= fun h ->
  Fpath.of_string h

let home = value_of_result (read_env "HOME")
let opam_switch_prefix = value_of_result (read_env "OPAM_SWITCH_PREFIX")

let cache_dir = Fpath.(home / ".cache" / "opam-bin-cache")
let success_cache_dir = Fpath.(cache_dir / "success")
let success_cache build_id = Fpath.(success_cache_dir / build_id)
let failure_cache_dir = Fpath.(cache_dir / "failure")
let failure_cache build_id = Fpath.(failure_cache_dir / build_id)

let runcmd cmd =
  OS.Cmd.(run_out cmd |> out_stdout |> success)

let rec riter f = function
  | [] -> Ok ()
  | x :: xs -> f x >>= fun () -> riter f xs

let () =
  exit_of_result @@
  match Sys.argv |> Array.to_list |> List.tl with
  | "wrap" :: "" :: command ->
    (* This is a "dev" package, that doesn't have a build id. Do not attempt to
       cache this. *)
    runcmd (Cmd.of_list command)

  | ["restore"; build_id; name] ->
    (* pre-install-commands hook *)
    OS.Dir.exists (success_cache build_id) >>= fun is_cached ->
    if is_cached then
      (* Copy the build result from the cache. *)
      (* The install instructions from the package will get disabled by the
         wrap hook. *)
      OS.File.delete ~must_exist:false (Fpath.v (name ^ ".install")) >>= fun () ->
      runcmd Cmd.(v "cp" % "-aT" % p (success_cache build_id) % p opam_switch_prefix)
    else
      (* Do nothing and let the build happen *)
      Ok ()

  | "wrap" :: build_id :: command ->
    (* wrap-build-commands, wrap-install-commands hooks*)
    OS.Dir.exists (success_cache build_id) >>= fun is_cached_success ->
    OS.File.exists (failure_cache build_id) >>= fun is_cached_failure ->
    if is_cached_success then Ok ()
    else if is_cached_failure then R.error_msg "Package in failure cache"
    else (
      begin match runcmd (Cmd.of_list command) with
      | Ok () -> Ok ()
      | Error err ->
        OS.Dir.create ~path:true failure_cache_dir >>= fun _ ->
        OS.File.write (failure_cache build_id) "." >>= fun () ->
        Error err
      end)

  | "store" :: build_id :: installed_files ->
    (* post-install-commands hook *)
    OS.Dir.exists (success_cache build_id) >>= fun is_cached_success ->
    if is_cached_success then Ok ()
    else
      riter (fun file ->
        Printf.printf "STORING FILE: %s\n" file;
        OS.Dir.exists Fpath.(opam_switch_prefix / file) >>= fun file_is_dir ->
        if file_is_dir then
          OS.Dir.create ~path:true Fpath.(success_cache build_id / file) >>= fun _ ->
          Ok ()
        else (
          OS.Dir.create ~path:true
            Fpath.(parent @@ success_cache build_id / file) >>= fun _ ->
          runcmd Cmd.(v "cp" % "-aT" %
                      p Fpath.(opam_switch_prefix / file) %
                      p Fpath.(success_cache build_id / file))
        )
      ) installed_files

  | args ->
    R.error_msgf "%sValid commands:\n\
                 \    restore ID NAME\n\
                 \    wrap ID COMMAND [ARGS]...\n\
                 \    store ID [FILES]...\n"
      (match args with
       | [] -> ""
       | cmd :: _ -> Printf.sprintf "Invalid command '%s'. " cmd)
