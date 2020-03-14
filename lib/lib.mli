(* val universe_exclude_cycles : OpamTypes.universe -> OpamTypes.universe *)

type cover_elt = {
  solution: OpamSolver.solution;
  useful: OpamPackage.Set.t;
}

val elt_installs: cover_elt -> OpamPackage.Set.t

val pp_cover_elt_stats : Format.formatter -> cover_elt -> unit

val compute_cover_elt :
  make_request:(
    universe:OpamTypes.universe ->
    to_install:OpamPackage.Set.t ->
    OpamSolver.solution
  ) ->
  universe:OpamTypes.universe ->
  to_install:OpamPackage.Set.t ->
  cover_elt * OpamPackage.Set.t

val dump_file : string

val make_request_maxsat :
  cycles:OpamPackage.Set.t list list ->
  universe:OpamTypes.universe ->
  to_install:OpamPackage.Set.t ->
  OpamSolver.solution
