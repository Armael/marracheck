open Utils

(* Generic API for accessing (possibly serialized) data stored on the filesystem.
   For the API specific to the marracheck on-disk data, see the [State] module.
*)

type _ data_format
val mk_data_format : unit -> 'a data_format

type dircap = { git : bool }

(* schema describing an in-filesystem layout *)
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
      (* if not None, used to create the file if the file does not exist. if
         None and the file does not exist, then the db is in a "broken" state
         and an error is raised *)
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

(* fstree helpers for building File/AppendFile nodes *)
val file_raw :
  format:string data_format -> default:string option -> fstree

val file_json :
  format:'a data_format -> default:'a option ->
  of_json:(Json.t -> 'a) -> to_json:('a -> Json.t) ->
  fstree

val appendfile_json :
  format:'a data_format ->
  of_json:(Json.t -> 'a) -> to_json:('a -> Json.t) ->
  fstree

(* These types are used below in [Make.path_desc] as phantom types to describe
   the destination of a path in the filesystem.

   The constructors (Plain__, etc) are never used to construct values, they are
   only useful to help with GADT typechecking. *)
type plain = Plain__
type 'a append = Append__
type ('a, 'k) file = File__
type git = Git__
type 'k dir = Dir__

module type Spec = sig
  val schema : fstree
end

module Make (Fs : Spec) : sig
  type t
  val load : root:string -> t
  val get_root : t -> string

  type _ path_desc =
    | FilePath : 'a data_format -> ('a, plain) file path_desc
    | AppendFilePath : 'a data_format -> ('a list, 'a append) file path_desc
    | DirPath : plain dir path_desc
    | GitDirPath : git dir path_desc
  type 'k path

  exception Illegal_path of string list
  val path : 'k path_desc -> string list -> 'k path
  val path_get_raw : _ path -> string list

  val read : t -> ('a, _) file path -> 'a
  val write : t -> ('a, _) file path -> 'a -> unit
  val append : t -> (_, 'b append) file path -> 'b -> unit

  val mkdir : t -> ?init:(unit -> unit) -> _ dir path -> unit
  val commit : t -> ?msg:string -> git dir path -> unit

  val exists : t -> _ path -> bool
end
