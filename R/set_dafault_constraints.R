#' Set default constraints for optimization
#'
#' This function sets default constraints for an optimization problem, including initial values, lower and upper bounds, and a total constraint function.
#' @param C Number of variables
#' @param total Total sum constraint
#' @return A list containing initial values (x0), lower bounds (lb), upper bounds (ub), and a total constraint function (total_constr_func).
#'
#' @export

set_dafault_constraints = function(C, total){

  total_constr_func = set_total_constraint(total)

  list(
    x0 = rep(total/C, C),
    lb = rep(0, C),
    ub = rep(total, C),
    total_constr_func = total_constr_func
  )
}
