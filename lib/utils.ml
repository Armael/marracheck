open OpamTypes

module Json = Ezjsonm

let log fmt = Printf.fprintf stderr ("LOG: " ^^ fmt ^^ "\n%!")
let fatal fmt =
  Printf.kfprintf (fun _ -> exit 1) stderr ("ERROR: " ^^ fmt ^^ "\n%!")

let mkdir dir =
  let dir = OpamFilename.Dir.to_string dir in
  if Sys.file_exists dir && not (Sys.is_directory dir) then
    fatal "Error: %s already exists but is not a directory" dir
  else OpamSystem.mkdir dir

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
