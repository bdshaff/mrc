#' Fit a response curve model using brms
#'
#' This function fits a response curve model using the brms package.
#'
#' @param data A data frame containing the data to be fitted.
#' @param x The name of the independent variable (predictor).
#' @param y The name of the dependent variable (response).
#' @param type The type of response curve model to fit. Options are "gompertz", "logistic", "weibull", or "exponential".
#' @param ... Additional arguments to be passed to the brms::brm function.
#' @return A fitted model object.
#' @details The function fits a response curve model using the specified type and returns the fitted model object.
#' @export


fit_response = function(data, x = NULL, y = NULL, type = "gompertz", ...){

  if (!(x %in% names(data)) || !(y %in% names(data))) {
    stop("Both 'x' and 'y' must be columns in the provided data.")
  }

  data <- data[,c(x,y)]

  #rename the columns of data by removing any _ or . in the column names
  colnames(data) <- gsub("[_.]", "", colnames(data))

  rc_formula = define_response_form(type, colnames(data)[1], colnames(data)[2])
  print(rc_formula)

  fit <- brms::brm(rc_formula, data = data, ...)

  fit$rc_type = type

  return(fit)
}
