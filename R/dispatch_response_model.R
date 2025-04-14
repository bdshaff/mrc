#' Dispatch Response Model
#'
#' This function dispatches the appropriate response model based on the type provided.
#'
#' @param type A string indicating the type of response model to dispatch.
#'
#' @return A function corresponding to the specified response model.
#'
#' @details
#' The function takes a string input `type` and returns the corresponding response model function.
#' The available response models are:
#' - "logistic"
#' - "log_logistic"
#' - "gompertz"
#' - "reflected_gompertz"
#' - "weibull"
#' - "reflected_weibull"
#'
#' @export

dispatch_response_model = function(type){

  rcs = list(
    logistic = logistic_response_model,
    log_logistic = log_logistic_response_model,
    gompertz = gompertz_response_model,
    reflected_gompertz = reflected_gompertz_response_model,
    weibull = weibull_response_model,
    reflected_weibull = reflected_weibull_response_model
  )

  if (!type %in% names(rcs)) {
    stop(paste("Invalid type. It needs to be one of:", paste(names(rcs), collapse = ", ")))
  }

  return(rcs[[type]])

}
