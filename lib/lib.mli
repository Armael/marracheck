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

  val to_json : t OpamJson.encoder
end

val compute_universe_cycles : OpamTypes.universe -> OpamPackage.Set.t list list

val make_request_maxsat :
  universe:OpamTypes.universe ->
  to_install:OpamPackage.Set.t ->
  OpamSolver.solution
