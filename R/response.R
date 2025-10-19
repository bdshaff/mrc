#' @title Response Curve Function
#' @description
#' This function computes the response curve based on the specified model type.
#' It supports the Gompertz model and the Richards model.
#'
#' @param x A numeric vector representing the input values.
#' @param params A list containing the parameters for the model. It should include:
#' \itemize{
#'  \item b: The slope parameter (numeric).
#'  \item c: The lower asymptote (numeric).
#'  \item d: The upper asymptote (numeric).
#'  \item e: The half-saturation (numeric).
#'  }
#' @param type A character string specifying the model type. It can be either "gompertz" or "richards".
#'
#' @return A numeric vector representing the response values computed by the model.
#' @export

response = function(x, params, type = "gompertz"){

  if (!is.numeric(x) || !is.vector(x)) {
    stop("x must be a numeric vector.")
  }

  if (!is.list(params) || !all(c("b", "c", "d", "e") %in% names(params))) {
    stop("params must be a list with elements 'b', 'c', 'd', 'e'.")
  }
  if (!all(sapply(params, is.numeric)) || !all(sapply(params, length) == 1)) {
    stop("Each element of params must be a numeric value of length 1.")
  }

  response_func = rm_dispatch(type)
  y = response_func(x, b = params$b, c = params$c, d = params$d, e = params$e)

  return(y)
}
