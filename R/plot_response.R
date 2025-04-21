#' Plot the response of a fitted model
#'
#' This function plots the response of a fitted model.
#' @param rc_fit A fitted model object. It can be a brmsfit object or a list of brmsfit objects.
#' @param xrange A vector of length 2 specifying the range of x values to plot. If NULL, the range of x values in the data is used.
#' @param length.out An integer specifying the number of points to generate for the x-axis. Default is 1000.
#' @param points A logical value indicating whether to plot the data points. Default is TRUE.
#' @return A ggplot object.
#' @details The function plots the response of a fitted model object. It uses ggplot2 to create the plot and includes a title and subtitle with information about the model.
#' @export

plot_response = function(rc_fit, xrange = NULL, length.out = 1000, points = TRUE){

  if(is.brmsfit(rc_fit)){
    response_df = infer_response(rc_fit, xrange = xrange, length.out = length.out)
    rc_data = rc_fit$data
    rc_type = rc_fit$rc_type
    y = rc_fit$formula$resp
    x = names(rc_data)[names(rc_data) != y]

    r_names = colnames(response_df)

    ptitle = paste0(rc_type, " response model:")
    psubtitle = paste0(paste0(names(extract_parameters(rc_fit)$center),"=", round(unlist(extract_parameters(rc_fit)$center),4)), collapse = ", ")

    p =
      ggplot2::ggplot() +
      ggplot2::geom_line(data = response_df, aes(!!sym(r_names[1]), !!sym(r_names[2]))) +
      ggplot2::geom_ribbon(data = response_df, aes(x = !!sym(r_names[1]), ymin = !!sym(r_names[4]), ymax = !!sym(r_names[5])), alpha = 0.3, fill = "blue") +
      ggplot2::labs(title = ptitle, subtitle = psubtitle)

  }else if(is.list(rc_fit)){

    rc_data = rc_fit[[1]]$data
    y = rc_fit[[1]]$formula$resp
    x = names(rc_data)[names(rc_data) != y]

    rc_paramps = map(rc_fit, ~paste0(paste0(names(extract_parameters(.x)$center),"=", round(unlist(extract_parameters(.x)$center),4)), collapse = ", "))
    rc_dfs = imap(rc_fit, ~infer_response(.x, xrange = xrange, length.out = length.out)[,c(1,2,4,5)])
    rc_dfs = map2(rc_dfs, names(rc_dfs), ~ mutate(.x, model = .y))
    rc_dfs = map2(rc_dfs, rc_paramps, ~ mutate(.x, model = paste0(model,": ", .y)))
    rdf = bind_rows(rc_dfs)
    r_names = colnames(rdf)

    p =
      ggplot2::ggplot() +
      ggplot2::geom_line(data = rdf, aes(x = !!sym(r_names[1]), y = conversions_Estimate, color = model)) +
      ggplot2::geom_ribbon(data = rdf, aes(x = !!sym(r_names[1]), ymin = conversions_Q2.5, ymax = conversions_Q97.5, fill = model), alpha = 0.2) +
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(0.7, 0.2)) +
      ggplot2::labs(title = "Response Models")
  }

  if(points == TRUE){
    p = p + ggplot2::geom_point(data = rc_data, aes(!!sym(x), !!sym(y)), color = "darkred", alpha = 0.7)
  }

  return(p)
}
