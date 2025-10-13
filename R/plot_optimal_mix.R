#' Plot Optimal Mix
#'
#' This function plots the response curves along with the optimal points determined by the optimization results.
#' @param response_funs A named list of response functions.
#' @param res The result object from the optimization containing the optimal solution.
#' @return A ggplot object showing the response curves and optimal points.
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @export


plot_optimal_mix = function(response_funs, res){

  x_r = seq(0, ceiling(max(res$solution) * 1.5), length.out = 100)
  resp_curves = map_dfr(response_funs, ~tibble(x = x_r, y = .x(x_r)), .id = "channel")

  channels = names(response_funs)
  solution_df = tibble(channel = channels, x = res$solution, y = map2_dbl(response_funs, res$solution, ~.x(.y)))

  p = ggplot(resp_curves, aes(x = x, y = y, color = channel)) +
    geom_line() +
    theme_minimal() +
    labs(x = "x", y = "y", title = "Fitted Response Curves with Optimal Points") +
    geom_point(data = solution_df, aes(x = x, y = y), size = 3, inherit.aes = TRUE)

  return(p)
}
