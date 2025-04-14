#' Define a response form for a nonlinear model
#'
#' This function defines a response form for a nonlinear model based on the specified type.
#' It uses the `brms` package to create a formula for the response variable.
#' @param type A character string specifying the type of response form.
#' Valid options are "logistic", "log_logistic", "gompertz", "reflected_gompertz", "weibull", and "reflected_weibull".
#' @param x A character string representing the name of the predictor variable.
#' @param y A character string representing the name of the response variable.
#'
#' @return A `brms` formula object representing the response form.
#' @details The function supports the following response forms:
#' \itemize{
#'   \item logistic: \eqn{y = c + ((d - c)/(1 + exp(b*(x - e))))}
#'   \item log_logistic: \eqn{y = c + ((d - c)/(1 + exp(b*(log(x) - log(e))))}
#'   \item gompertz: \eqn{y = c + (d - c) * exp(-exp(b * (x - e)))}
#'   \item reflected_gompertz: \eqn{y = c + (d - c) * (1 - exp(-exp(b * (-x + e))))}
#'   \item weibull: \eqn{y = c + (d - c) * exp(-exp(b * (log(x) - log(e))))}
#'   \item reflected_weibull: \eqn{y = c + (d - c) * (1 - exp(-exp(b * (-log(x) + log(e))))}
#'   }
#' The parameters b, c, d, and e are the model parameters to be estimated.
#' @examples
#' logistic_form <- define_response_form("logistic", "x", "y")
#'
#' @export

define_response_form <- function(type, x = NULL, y = NULL){

  if (is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided and cannot be NULL.")
  }

  resp_forms <- list(
    logistic = y ~ c + ((d - c)/(1 + exp(b*(x - e)))),
    log_logistic = y ~ c + ( (d - c) / ( 1 + exp( b * ( log(x) - log(e))))),
    gompertz = y ~ c + (d - c) * exp(-exp( b * (x - e))),
    reflected_gompertz = y ~ c + (d - c) * (1 - exp(-exp( b * (-x + e)))),
    weibull = y ~ c + (d - c) * exp(-exp( b * (log(x) - log(e)))),
    reflected_weibull = y ~ c + (d - c) * (1 - exp(-exp( b * (-log(x) + log(e)))))
  )

  if (!type %in% names(resp_forms)) {
    stop(paste("Invalid type. It needs to be one of:", paste(names(resp_forms), collapse = ", ")))
  }

  resp_form = resp_forms[[type]]
  resp_form = replace_variables_in_formula(resp_form, old_vars = c("x","y"), new_vars = c(x, y))
  resp_form = brms::bf( resp_form , b + c + d + e ~ 1, nl = TRUE )

  return(resp_form)
}
