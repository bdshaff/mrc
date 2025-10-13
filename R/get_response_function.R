#' Get the response function from a fitted model
#'
#' This function extracts the response function from a fitted model object. It allows you to specify which location's parameters to use (lower, center, or upper).
#' #' @param model A fitted model object returned by `fit_response()`.
#' #' @param location A string specifying which location's parameters to use. Must be one of "lower", "center", or "upper". Default is "center".
#' #' @return A function that takes a numeric vector of x values and returns the corresponding y values based on the specified response model and parameters.
#'
#' @export
get_response_function = function(model, location = "center"){

  if(!(location %in% c("lower", "center", "upper"))){
    stop("location must be one of 'left', 'center', or 'right'")
  }

  f = dispatch_response_model(model$rc_type)
  p = extract_parameters(model)[[location]]
  function(x) {
    f(x, b = p$b, c = p$c, d = p$d, e = p$e)
  }
}
