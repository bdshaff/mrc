#' Infer response from a fitted model
#'
#' This function infers the response from a fitted model object.
#' @param rc_fit A fitted model object.
#' @param xrange A numeric vector of length 2 specifying the range of x values for prediction. Default is NULL, which uses the range of x in the data.
#' @return A data frame containing the predicted response values and the model response.
#' @details The function infers the response from the fitted model object and returns a data frame with the predicted response values and the model response.
#' @export

infer_response = function(rc_fit, xrange = NULL){

  rc_type = rc_fit$rc_type
  rc_data = rc_fit$data
  y = rc_fit$formula$resp
  x = names(rc_data)[names(rc_data) != y]

  if(is.null(xrange)){
    xrange = c(0, 2 * max(rc_data[,x]))
  }

  if (!is.numeric(xrange) || length(xrange) != 2 || xrange[1] >= xrange[2]) {
    stop("xrange must be a numeric vector of length 2 with the first element less than the second.")
  }

  xseq = seq(xrange[1], xrange[2], length.out = 1000)
  new_df = data.frame(x = xseq)
  colnames(new_df) = x

  pred_df = as.data.frame(predict(rc_fit, newdata = new_df))

  pred_df$Estimate = if_else(pred_df$Estimate < 0, 0, pred_df$Estimate)
  pred_df$Q2.5 = if_else(pred_df$Q2.5< 0, 0, pred_df$Q2.5)

  colnames(pred_df) = paste0(y,"_", colnames(pred_df))

  response_params <- extract_parameters(rc_fit)

  model_response = purrr::map_dfc(response_params, ~response_curve(xseq, .x, type = rc_type))
  colnames(model_response) = paste0(y,"_",rc_type,"_", colnames(model_response))

  pred_df = cbind(new_df, pred_df, model_response)

  return(pred_df)
}
