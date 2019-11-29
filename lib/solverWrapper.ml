open OpamCudfSolverSig

let name = "marracheck-solver-wrapper"

let ext : string option ref = ref None

let is_present () = true

let command_name = None

let default_solver =
  OpamCudfSolver.get_solver OpamCudfSolver.default_solver_selection

module Default_solver = (val default_solver)

let default_criteria = Default_solver.default_criteria

type solver =
  | Default
  | Z3_default
  | Z3_lp
  | Z3_wmax
  | MaxSat

(* TODO: remove Z3_default after upgrading to the latest opam git,
   since it is now an official backend? *)

let solver = ref Default

let call ~criteria ?timeout cudf =
  match !solver with
  | Default ->
    Default_solver.call ~criteria ?timeout cudf
  | Z3_default ->
    OpamDefaultZ3.call ~criteria:OpamDefaultZ3.default_criteria.crit_default
      ?timeout cudf
  | Z3_lp ->
    OpamZ3_lp.call ~criteria:OpamZ3_lp.default_criteria.crit_default
      ?timeout cudf
  | Z3_wmax ->
    OpamZ3_wmax.call ~criteria:OpamZ3_wmax.default_criteria.crit_default
      ?timeout cudf
  | MaxSat ->
    MaxSat.call ~criteria:MaxSat.default_criteria.crit_default ?timeout cudf
