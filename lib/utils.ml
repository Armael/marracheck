open OpamTypes

module Json = Ezjsonm

let rec value_of_yojson = function
  | `Bool b -> `Bool b
  | `Float x -> `Float x
  | `Int n -> `Float (float_of_int n)
  | `Null -> `Null
  | `String s -> `String s
  | `Assoc assoc -> `O (List.map (fun (k, v) -> (k, value_of_yojson v)) assoc)
  | `List li -> `A (List.map value_of_yojson li)

let rec yojson_of_value = function
  | `Bool b -> `Bool b
  | `Float x -> `Float x
  | `Null -> `Null
  | `String s -> `String s
  | `O assoc -> `Assoc (List.map (fun (k, v) -> (k, yojson_of_value v)) assoc)
  | `A li -> `List (List.map yojson_of_value li)

let t0 = Unix.gettimeofday ()
let time () = Unix.gettimeofday () -. t0

let timestamp fmt () =
  let time = Unix.gettimeofday () -. t0 in
  let tm = Unix.gmtime time in
  let msec = time -. (floor time) in
  Format.fprintf fmt "%.2d:%.2d.%.3d"
    (tm.Unix.tm_hour * 60 + tm.Unix.tm_min)
    tm.Unix.tm_sec
    (int_of_float (1000.0 *. msec))

let log fmt =
  Format.fprintf Format.err_formatter "LOG %a: " timestamp ();
  Format.fprintf Format.err_formatter (fmt ^^ "\n%!")

let fatal fmt =
  Format.fprintf Format.err_formatter "ERROR %a: " timestamp ();
  Format.kfprintf (fun _ -> exit 1) Format.err_formatter (fmt ^^ "\n%!")

let mkdir dir =
  let dir = OpamFilename.Dir.to_string dir in
  if Sys.file_exists dir && not (Sys.is_directory dir) then
    fatal "Error: %s already exists but is not a directory" dir
  else OpamSystem.mkdir dir

let mv dir1 dir2 =
  let dir1 = OpamFilename.Dir.to_string dir1 in
  let dir2 = OpamFilename.Dir.to_string dir2 in
  OpamSystem.mv dir1 dir2

let read_json (file: filename): Json.t =
  let cin = open_in (OpamFilename.to_string file) in
  let res =
    try Ok (Json.from_channel cin) with
      Json.Parse_error (_,_) -> Error () in
  close_in cin;
  match res with
  | Ok j -> j
  | Error () -> fatal "In %s: parsing error" (OpamFilename.prettify file)

let must_succeed cmd res =
  if OpamProcess.is_failure res then
    fatal "Running '%s' failed:\n %s\n%!"
      (OpamProcess.string_of_command cmd)
      (OpamProcess.string_of_result res)

let get_or_fatal opt fmt =
  match opt with
  | Some x -> Format.ikfprintf (fun _ -> x) Format.err_formatter fmt
  | None -> fatal fmt
