#' Plot the response rate curves
#'
#' This function plots the response rate curves for a given response curve fit.
#'
#' @param rc_fit A response curve fit object.
#' @param avg_cp The average cost per response. Default is 1.
#'
#' @return A ggplot object representing the response rate curves.
#' @export

plot_range = function(rc_fit, avg_cp = 1){

  spend <- response_df[,x] * avg_cp
  model_spend_df <- data.frame(spend = spend)
  model_resp_df <- response_df[,grepl(paste0(y,"_",rc_type,"_center"), colnames(response_df))]
  model_rr_df <- model_resp_df/response_df[,x]
  model_mrr_df = (response_df[,x] - lag(response_df[,x])) / (model_resp_df - lag(model_resp_df))
  model_cp_df <- spend/model_resp_df
  model_df <- cbind(model_spend_df, model_rr_df, model_mrr_df, model_cp_df)
  model_df <- model_df[model_df$spend > 0,]
  center_model_df <- model_df[,-1]
  spend_model_df <- model_df[1]
  colnames(center_model_df) <- c('Response Rate (RR)', 'Marginal RR', paste0('Cost Per ',y))

  gdf = purrr::map2_dfr(
    as.list(center_model_df),
    names(as.list(center_model_df)),
    ~cbind(spend_model_df, data.frame(value = .x, metric = .y))
  )

  opt_rng_df <- model_df[model_df$spend >= 0.5*min(rc_data[,x]) * avg_cp,]
  intersections = model_rr_df[order(sqrt((model_rr_df - model_mrr_df)^2))[1:2]]
  max_opt_spend = as.numeric(tail(opt_rng_df[opt_rng_df$model_rr_df %in% intersections,],1)["spend"] * 1.1)
  min_opt_spend = as.numeric(opt_rng_df[which.max(opt_rng_df$model_rr_df),"spend"] * 0.9)

  p <- ggplot2::ggplot(data = gdf, aes(x = spend, y = value)) +
    ggplot2::geom_line(aes(color = metric)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::ylab("RR, MRR, CP") +
    ggplot2::geom_ribbon(aes(x = spend, xmin = min_opt_spend, xmax = max_opt_spend),color = "grey", alpha = 0.1, fill = "darkgreen") +
    ggplot2::labs(
      title = "Response Rate Curves",
      subtitle = paste0("Optimal range from ", round(min_opt_spend), " to ", round(max_opt_spend))
    )

  return(p)
}
