#' Extract parameters from a fitted model
#'
#' This function extracts the parameters from a fitted model object.
#'
#' @param rc_fit A fitted model object.
#' @return A list containing the center, lower, and upper bounds of the parameters.
#' @details The function extracts the parameters from the fitted model object and returns them in a list.
#'
#' @export

extract_parameters <- function(rc_fit) {
  posterior_summary <- summary(rc_fit)$fixed
  params <- posterior_summary[grepl("b|c|d|e", rownames(posterior_summary)), ]
  params <- as.data.frame(params)

  center = params$Estimate
  names(center) = c("b","c","d","e")

  lower = params$`l-95% CI`
  names(lower) = c("b","c","d","e")

  upper = params$`u-95% CI`
  names(upper) = c("b","c","d","e")

  params_list <- list(
    center = as.list(center),
    lower = as.list(lower),
    upper = as.list(upper)
  )

  return(params_list)
}
