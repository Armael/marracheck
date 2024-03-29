open Utils

(* data format *)
(* definition of [data_format] and [data_format_iseq] by Florian "octachron"
   Angeletti. *)

type _ data_cstr = ..

module type DATA_CSTR = sig
  type a
  type _ data_cstr += Fmt: a data_cstr
end

type 'a data_format = (module DATA_CSTR with type a = 'a)

let mk_data_format : type a. unit -> a data_format = fun () ->
  (module struct type nonrec a = a type _ data_cstr += Fmt : a data_cstr end)

type (_, _) eq = Eq : ('a, 'a) eq
let data_format_iseq : type a b. a data_format -> b data_format -> (a, b) eq option =
  fun (module M) (module N) ->
  match M.Fmt, N.Fmt with
  | M.Fmt, M.Fmt -> Some Eq
  | _ -> None

exception Cast_failure

let data_format_eqbool : type a b. a data_format -> b data_format -> bool =
  fun fmt1 fmt2 ->
  match data_format_iseq fmt1 fmt2 with
  | Some Eq -> true
  | None -> false

let cast_data : type a b. a data_format -> b data_format -> a -> b =
  fun fmt1 fmt2 x ->
  match data_format_iseq fmt1 fmt2 with
  | Some Eq -> x
  | None -> raise Cast_failure

let cast_data_list : type a b. a data_format -> b data_format -> a list -> b list =
  fun fmt1 fmt2 x ->
  match data_format_iseq fmt1 fmt2 with
  | Some Eq -> x
  | None -> raise Cast_failure

(* fstree *)

type dircap = { git : bool }

type fstree =
  | StaticDir : {
      contents : (string * fstree) list;
      cap : dircap;
    } -> fstree

  | DynDir : {
      schema : fstree;
      cap : dircap;
    } -> fstree

  | ExtDir : fstree

  | File : {
      format : 'a data_format;
      default : 'a option;
      write : filename:string -> 'a -> unit;
      read : filename:string -> ('a, string) Result.t;
    } -> fstree

  | AppendFile : {
      format : 'a data_format;
      append : filename:string -> 'a -> unit;
      read_all : filename:string -> ('a list, string) Result.t;
      write_all : filename:string -> 'a list -> unit;
    } -> fstree

(* fstree helpers *)

let file_raw ~format ~default =
  File {
    format; default;
    read = (fun ~filename ->
      CCResult.guard_str (fun () ->
        CCIO.with_in ~flags:[Open_binary;Open_rdonly] filename CCIO.read_all));
    write = (fun ~filename str ->
      CCIO.with_out ~flags:[Open_trunc;Open_wronly;Open_creat;Open_binary] filename
        (fun cout -> output_string cout str))
  }

let file_json (type a) ~format ~(default: a option) ~of_json ~to_json =
  File {
    format; default;
    read = (fun ~filename ->
      CCIO.with_in ~flags:[Open_binary;Open_rdonly] filename
        (fun cin ->
           try Json.from_channel cin |> of_json |> Result.ok
           with Json.Parse_error (_, _) ->
             Error (Printf.sprintf "In %s: json parsing error" filename)));
      write = (fun ~filename v ->
        CCIO.with_out ~flags:[Open_trunc;Open_wronly;Open_creat;Open_binary] filename
          (fun cout ->
             Json.to_channel ~minify:false cout (to_json v))
      );
  }

let appendfile_json ~format
    ~(of_json : Json.t -> 'a)
    ~(to_json : 'a -> Json.t)
  =
  AppendFile {
    format;
    append = (fun ~filename elt ->
      CCIO.with_out ~flags:[Open_binary;Open_append;Open_wronly;Open_creat] filename
        (fun cout ->
           Yojson.Basic.to_channel cout (yojson_of_value (to_json elt :> Json.value));
           output_char cout '\n')
    );
    read_all = (fun ~filename ->
      try
        let entries = Yojson.Basic.stream_from_file filename in
        let r = ref [] in
        let of_yojson x =
          match value_of_yojson x with
          | #Json.t as x -> of_json x
          | not_toplevel ->
            Json.parse_error not_toplevel "Invalid toplevel JSON value"
        in
        Stream.iter (fun x -> r := of_yojson x :: !r) entries;
        Ok (List.rev !r)
      with Json.Parse_error (_, _) | Yojson.Json_error _ ->
        Error (Printf.sprintf "In %s: json parsing error" filename)
    );
    write_all = (fun ~filename elts ->
      let entries = List.map (fun elt ->
        yojson_of_value (to_json elt :> Json.value)
      ) elts
      in
      Yojson.Basic.stream_to_file filename
        (Stream.of_list entries)
    );
  }

(* *** *)

type plain = Plain__
type 'a append = Append__
type ('a, 'k) file = File__ of 'a
type git = Git__
type 'k dir = Dir__

module type Spec = sig
  val schema : fstree
end

module Make (Fs : Spec) = struct
  type 'k path_desc =
    | FilePath : 'a data_format -> ('a, plain) file path_desc
    | AppendFilePath : 'a data_format -> ('a list, 'a append) file path_desc
    | DirPath : plain dir path_desc
    | GitDirPath : git dir path_desc

  let path_desc_eq (type k1 k2) (d1: k1 path_desc) (d2: k2 path_desc): (k1, k2) eq option =
    match d1, d2 with
    | FilePath df1, FilePath df2 ->
      begin match data_format_iseq df1 df2 with
        | Some Eq -> Some Eq
        | None -> None
      end
    | AppendFilePath df1, AppendFilePath df2 ->
      begin match data_format_iseq df1 df2 with
        | Some Eq -> Some Eq
        | None -> None
      end
    | DirPath, DirPath -> Some Eq
    | GitDirPath, GitDirPath -> Some Eq
    | _ -> None

  type 'k path = string list * 'k path_desc

  exception Illegal_path of string list
  let () =
    Printexc.register_printer (fun exn ->
      match exn with
      | Illegal_path ps ->
        Some (
          Printf.sprintf "Illegal_path(%s)"
            (List.fold_left Filename.concat "" ps)
        )
      | _ -> None
    )

  module FsCache = Dmap.Make (struct
      type 'a t = 'a path
      let compare : type a b. a path -> b path -> (a, b) Dmap.cmp =
        fun (p1, d1) (p2, d2) ->
        let cmp = List.compare String.compare p1 p2 in
        if cmp < 0 then Dmap.Lt
        else if cmp > 0 then Dmap.Gt
        else match path_desc_eq d1 d2 with
          | Some Eq -> Dmap.Eq
          | None -> assert false (* equal paths must have equal kind *)
    end)

  type t = {
    root : string;
    mutable cache : FsCache.t;
  }

  let get_root db = db.root

  let fsmkdir ?cap path =
    let path' = OpamFilename.Dir.of_string path in
    mkdir path';
    match cap with
    | Some { git = true } ->
      (* do 'git init' if needed; otherwise cleanup uncommited modifications
         (if any) *)
      let open OpamProcess in
      if not (OpamGit.VCS.exists path') then begin
        let cmd = command ~dir:path "git" [ "init" ] in
        run cmd |> must_succeed cmd
      end;
      begin match Job.run (OpamGit.VCS.revision path') with
        | None -> (* no commits recorded *) ()
        | Some _ ->
          (* cleanup uncommited modifications *)
          if Job.run (OpamGit.VCS.is_dirty path') then begin
            Job.of_list [
              command ~dir:path "git" [ "reset"; "--hard"; "HEAD" ];
              command ~dir:path "git" [ "clean"; "-xfd" ];
            ] |> Job.run
            |> OpamStd.Option.iter (fun (cmd, res) -> must_succeed cmd res)
          end
      end
    | Some { git = false } | None -> ()

  let check_and_autoinit (path: string) (tree: fstree) =
    (* check that the filesystem corresponds to the provided schema, creating
       directories/files with default values if needed. *)
    let rec loop (path: string) (tree: fstree) =
      match tree with
      | StaticDir { contents; cap } ->
        fsmkdir ~cap path;
        List.iter (fun (name, subtree) ->
          loop (Filename.concat path name) subtree
        ) contents
      | DynDir { schema; cap } ->
        fsmkdir ~cap path;
        let contents = Sys.readdir path in
        Array.iter (fun name ->
          loop (Filename.concat path name) schema
        ) contents
      | ExtDir ->
        fsmkdir path
      | File { default; write; _ } ->
        if not (Sys.file_exists path) then (
          match default with
          | Some v -> write ~filename:path v
          | None -> fatal "%s required but not found" path
        ) else if Sys.is_directory path then
          fatal "%s exists but is not a file" path
      | AppendFile { write_all; _ } ->
        if not (Sys.file_exists path) then write_all ~filename:path []
        else if Sys.is_directory path then
          fatal "%s exists but is not a file" path
    in
    loop path tree

  let load ~root =
    (* After the initial autoinit, we work under the assumption that this
       property stays true for as long as we have hold of the database handle.
       In other words, we assume that:

       - there are no concurrent changes happening to the filesystem (while
       marracheck is running);

       - the filesystem is not updated by other means (without going through the
       API) in a way that makes it inconsistent. *)
    check_and_autoinit root Fs.schema;
    { root; cache = FsCache.empty }

  let subtree path (tree: fstree) : fstree =
    let rec loop subpath tree =
      match subpath with
      | [] -> tree
      | name :: subpath ->
        match tree with
        | File _ | AppendFile _ | ExtDir -> raise (Illegal_path path)
        | StaticDir { contents; _ } ->
          begin match List.assoc name contents with
            | tree -> loop subpath tree
            | exception Not_found -> raise (Illegal_path path)
          end
        | DynDir { schema; _ } ->
          (* any name in this directory can be valid *)
          loop subpath schema
    in loop path tree

  let check_path_against_schema : type a. a path -> unit = fun path ->
    let path, path_desc = path in
    let stree = subtree path Fs.schema in
    match path_desc, stree with
    | FilePath pformat, File { format; _ } ->
      if data_format_eqbool format pformat then () else raise (Illegal_path path)
    | (AppendFilePath pformat), (AppendFile { format; _ }) ->
      if data_format_eqbool format pformat then () else raise (Illegal_path path)
    | (GitDirPath | DirPath), (StaticDir _ | DynDir _ | ExtDir) ->
      ()
    | _, _ ->
      raise (Illegal_path path)

  let with_check_path_against_schema p =
    check_path_against_schema p; p

  let path desc p =
    with_check_path_against_schema (p, desc)

  let path_get_raw (p, _) = p

  let string_of_path root (path: string list) =
    List.fold_left Filename.concat root path

  (* conservatively required when reading and appending. Writes that create the
     file are ok. *)
  let check_path_exists root (path: string list) =
    let rec loop base path =
      if not (Sys.file_exists base) then
        fatal "%s does not exist" base;
      match path with
      | [] -> ()
      | name :: path ->
        if not (Sys.is_directory base) then
          fatal "%s exists but is not a directory" base;
        loop (Filename.concat base name) path
    in
    loop root path

  let read : type a b. t -> (a, b) file path -> a =
    fun db (path, desc) ->
    let tree = subtree path Fs.schema in
    check_path_exists db.root path;
    (* check whether the file contents are in the cache *)
    match FsCache.find_opt (path, desc) db.cache with
    | Some (File__ data) -> data
    | None ->
      (* if not, read the file and cache the data *)
      let filename = string_of_path db.root path in
      assert (Sys.file_exists filename && not (Sys.is_directory filename));
      match tree, desc with
      | File { format; read; _ }, FilePath pformat ->
        let data =
          match read ~filename with
          | Result.Ok x -> cast_data format pformat x
          | Result.Error msg -> fatal "When reading %s: %s" filename msg in
        db.cache <- FsCache.add (path, desc) (File__ data) db.cache;
        data
      | AppendFile { format; read_all; _ }, AppendFilePath pformat ->
        let data =
          match read_all ~filename with
          | Result.Ok x -> cast_data_list format pformat x
          | Result.Error msg -> fatal "When reading %s: %s" filename msg in
        db.cache <- FsCache.add (path, desc) (File__ data) db.cache;
        data
      | _ ->
        raise (Illegal_path path)

  let write : type a b. t -> (a, b) file path -> a -> unit =
    fun db (path, desc) v ->
    let tree = subtree path Fs.schema in
    let filename = string_of_path db.root path in
    match tree, desc with
    | File { format; write; _ }, FilePath pformat ->
      write ~filename (cast_data pformat format v);
      db.cache <- FsCache.add (path, desc) (File__ v) db.cache
    | AppendFile { format; write_all; _ }, AppendFilePath pformat ->
      write_all ~filename (cast_data_list pformat format v);
      db.cache <- FsCache.add (path, desc) (File__ v) db.cache
    | _ ->
      raise (Illegal_path path)

  let append : type a b. t -> (a, b append) file path -> b -> unit =
    fun db (path, desc) v ->
    let tree = subtree path Fs.schema in
    check_path_exists db.root path;
    let filename = string_of_path db.root path in
    begin match tree, desc with
    | AppendFile { format; append; _ }, AppendFilePath pformat ->
      append ~filename (cast_data pformat format v)
    | _ ->
      raise (Illegal_path path)
    end;
    (* do not attempt to cache appends; invalidate any outdated cache entry *)
    db.cache <- FsCache.remove (path, desc) db.cache

  let commit db ?(msg = "-") (path, _) =
    check_path_exists db.root path;
    let dirname = string_of_path db.root path in
    let msg = if msg = "" then "-" else msg in
    let open OpamProcess in
    Job.of_list [
      command ~dir:dirname "git" [ "add"; "*" ];
      command ~dir:dirname "git" [ "commit"; "-a"; "--allow-empty"; "-m"; msg ];
    ]
    |> Job.run
    |> OpamStd.Option.iter (fun (cmd, res) -> must_succeed cmd res)

  let mkdir db ?(init = fun () -> ()) (path, _desc) =
    let tree = subtree path Fs.schema in
    let dirname = string_of_path db.root path in
    begin match tree with
    | ExtDir -> fsmkdir dirname
    | StaticDir { cap; _ } | DynDir { cap; _ } -> fsmkdir ~cap dirname
    | _ -> raise (Illegal_path path)
    end;
    init ();
    (* run [check_and_autoinit] at the root of the filesystem, as [fsmkdir]
       can be called to create several nested subdirs, and thus we may need to
       check parent directories of [path] as well *)
    check_and_autoinit db.root Fs.schema

  let rec is_removable (tree: fstree) =
    match tree with
    | StaticDir { contents; _ } ->
      List.for_all (fun (_, subtree) ->
        is_removable subtree
      ) contents
    | DynDir _ (* A [DynDir] with no entries is always valid *)
    | ExtDir
    | File { default = Some _; _ }
    | AppendFile _ ->
      true
    | File { default = None; _ } ->
      false

  (* remove cache entries for [path] and its children (if it is a directory) *)
  let invalidate_cache_at db path =
    let rec is_prefix p1 p2 =
      match p1, p2 with
      | [], _ -> true
      | _::_, [] -> false
      | x::xs, y::ys -> x = y && is_prefix xs ys
    in
    db.cache <- FsCache.filter { fun1 = (fun (type a) (p, _: a path) _ ->
      not (is_prefix path p)
    )} db.cache

  let remove db (path, _) =
    if path = [] then
      fatal "Fs.remove: cannot remove the root path";
    let parent = CCList.take (List.length path - 1) path in
    let safe_to_remove =
      is_removable (subtree path Fs.schema) ||
      match subtree parent Fs.schema with
      | DynDir _ -> true (* safe to remove an entry from a DynDir *)
      | _ -> false
    in
    let path_s = string_of_path db.root path in
    if not safe_to_remove then
      fatal "Fs.remove: cannot remove %s" path_s;
    OpamSystem.remove path_s;
    invalidate_cache_at db path

  let recreate : type d.
    t -> ?finalize:(unit -> unit) -> ?init:(unit -> unit) ->  d dir path -> unit
    =
    fun db ?(finalize = fun () -> ()) ?(init = fun () -> ()) (path, desc) ->
    let dirname = string_of_path db.root path in
    check_path_exists db.root path;
    finalize ();
    OpamSystem.remove_dir dirname;
    invalidate_cache_at db path;
    mkdir db ~init (path, desc)

  let exists db (path, _) =
    Sys.file_exists (string_of_path db.root path)
end
