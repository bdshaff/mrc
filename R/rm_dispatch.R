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

rm_dispatch = function(type){

  rms = list(
    logistic = rm_Logistic,
    log_logistic = rm_LogLogistic,
    gompertz = rm_Gompertz,
    reflected_gompertz = rm_GompertzRef,
    weibull = rm_Weibull,
    reflected_weibull = rm_WeibullRef
  )

  if (!type %in% names(rms)) {
    stop(paste("Invalid type. It needs to be one of:", paste(names(rms), collapse = ", ")))
  }

  return(rms[[type]])

}
