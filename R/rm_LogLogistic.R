#' Log-logistic response model
#'
#' This function computes the log-logistic response model for a given vector of x values.
#' It takes parameters b, c, d, and e to define the log-logistic function.
#'
#' @param x A numeric vector of x values.
#' @param b A numeric value representing the steepness of the curve.
#' @param c A numeric value representing the lower asymptote.
#' @param d A numeric value representing the upper asymptote.
#' @param e A numeric value representing the x-value of the sigmoid's midpoint.
#'
#' @return A numeric vector of the same length as x, representing the computed log-logistic response values.
#' @details The log-logistic function is defined as:
#' \deqn{y = c + \frac{(d - c)}{1 + e^{-b(log(x) - log(e))}}}
#' @examples
#' x_values <- seq(0, 10, by = 0.1)
#' b <- -2
#' c <- 0
#' d <- 1
#' e <- 5
#' result <- rm_LogLogistic(x_values, b, c, d, e)
#' plot(x_values, result, type = "l", main = "Log-logistic Response Model", xlab = "x", ylab = "y")
#' @export

rm_LogLogistic = function(x, b, c, d, e){

  if (!is.numeric(x) || !is.vector(x)) {
    stop("x must be a numeric vector.")
  }
  if (!is.numeric(b) || length(b) != 1) {
    stop("b must be a numeric value of length 1.")
  }
  if (!is.numeric(c) || length(c) != 1) {
    stop("c must be a numeric value of length 1.")
  }
  if (!is.numeric(d) || length(d) != 1) {
    stop("d must be a numeric value of length 1.")
  }
  if (!is.numeric(e) || length(e) != 1) {
    stop("e must be a numeric value of length 1.")
  }

  if (d <= c) {
    stop("Parameter 'd' must be greater than 'c'.")
  }

  y = c + ( (d - c) / ( 1 + exp( b * ( log(x) - log(e) ) ) ) )
  return(y)
}
