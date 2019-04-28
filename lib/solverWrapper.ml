open OpamCudfSolverSig

let name = "marracheck-solver-wrapper"

let ext : string option ref = ref None

let is_present () = true

let command_name = None

let default_solver =
  OpamCudfSolver.get_solver OpamCudfSolver.default_solver_selection

module Default_solver = (val default_solver)

let default_criteria = Default_solver.default_criteria

let use_custom_best_effort = ref false

let call ~criteria ?timeout cudf =
  if !use_custom_best_effort then
    OpamZ3.call
      ~criteria:OpamZ3.default_criteria.crit_default
      ?timeout
      cudf
  else
    Default_solver.call ~criteria ?timeout cudf
