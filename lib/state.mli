open Utils

type t
val load : workdir:string -> t
val get_workdir : t -> string

type plain
type 'a append
type ('a, 'k) file
type git
type 'k dir
type 'k path

val read : t -> ('a, _) file path -> 'a
val write : t -> ('a, _) file path -> 'a -> unit
val append : t -> (_, 'b append) file path -> 'b -> unit

val mkdir : t -> ?init:(unit -> unit) -> _ dir path -> unit
val commit : t -> ?msg:string -> git dir path -> unit
val recreate : t ->
  ?finalize:(unit -> unit) ->
  ?init:(unit -> unit) ->
  _ dir path ->
  unit

val remove : t -> _ path -> unit
val exists : t -> _ path -> bool

(* converting paths to full filesystem paths *)

val p : workdir:string -> _ path -> string
val d : workdir:string -> _ dir path -> OpamTypes.dirname
val f : workdir:string -> _ file path -> OpamTypes.filename

(* path constructors *)

val cache_path : plain dir path
val opamroot_path : plain dir path
(* switch_state-local paths *)
val past_timestamps_path : switch:string -> plain dir path
val cover_state_path : switch:string -> git dir path
val timestamp_path : switch:string -> (string, plain) file path
val past_elts_path : switch:string -> (Data.Cover.t, plain) file path
val cur_plan_path : switch:string -> (Data.Cover_elt_plan.t option, plain) file path
val cur_report_path : switch:string -> (Data.Package_report.t list, Data.Package_report.t append) file path
val uninst_path : switch:string -> (PkgSet.t, plain) file path

(* higher-level queries that aggregate or further process information from the
   files *)

val resolved_packages : t -> switch:string -> PkgSet.t
val broken_packages : t -> switch:string -> PkgSet.t
val nonbuilt_useful_packages : t -> switch:string -> Data.Cover_elt_plan.t -> PkgSet.t
val archive_cur_elt : t -> switch:string -> Data.Cover.elt option
