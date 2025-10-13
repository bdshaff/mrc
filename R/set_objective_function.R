#' Create an objective function for optimization
#' @description
#' This function generates an objective function that can be used in optimization routines.
#' The objective function computes the negative sum of the outputs from a list of response functions
#' when evaluated at a given set of input parameters.
#' @param response_funs A list of functions. Each function should take a single numeric input and return a numeric output.
#' @return A function that takes a numeric vector as input and returns the negative sum of the outputs from the response functions.
#' @importFrom purrr map2_dbl
#' @export

set_objective_function = function(response_funs){
  objective_func = function(x) {
    -sum(map2_dbl(response_funs, x, ~.x(.y)))
  }

  return(objective_func)
}
