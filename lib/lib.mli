(* val universe_exclude_cycles : OpamTypes.universe -> OpamTypes.universe *)

module Cover_elt_plan : sig
  type t = {
    solution: OpamSolver.solution;
    useful: OpamPackage.Set.t;
  }

  val installs: t -> OpamPackage.Set.t

  val pp_stats : Format.formatter -> t -> unit

  val compute :
    make_request:(
      universe:OpamTypes.universe ->
      to_install:OpamPackage.Set.t ->
      OpamSolver.solution
    ) ->
    universe:OpamTypes.universe ->
    to_install:OpamPackage.Set.t ->
    t * OpamPackage.Set.t
end

val dump_file : string

val make_request_maxsat :
  cycles:OpamPackage.Set.t list list ->
  universe:OpamTypes.universe ->
  to_install:OpamPackage.Set.t ->
  OpamSolver.solution
