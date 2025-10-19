#' Plot Marginal and Absolute Rates of Return for Multiple Resource Models
#'
#' This function generates a plot comparing the Marginal Rate of Return (MR) and Absolute Rate of Return (AR) for multiple resource models. It highlights key points such as the maximum MR and the intersection point where MR equals AR.
#' @param mrm A list of fitted model objects (e.g., brmsfit objects).
#' @param xrange A numeric vector of length 2 specifying the range of x values to consider. If NULL, the range is determined from the data.
#' @param ncol An integer specifying the number of columns in the facet wrap. Default is 2.
#' @return A ggplot object visualizing the MR and AR for each model.
#' @details The function computes the MR and AR for each model in the list and creates a faceted plot. It highlights the maximum MR point in red and the intersection point where MR equals AR in blue. A shaded green area indicates the range between these two points.
#'
#' @export


mrm_plot_return = function(mrm, xrange = NULL, ncol = 2) {

  response_df = purrr::map_dfr(mrm, ~mrm_infer(.x, xrange = xrange), .id = "channel")

  maxmr_df =
    response_df |>
    dplyr::group_by(channel) |>
    dplyr::filter(mr == max(mr, na.rm = TRUE)) |>
    select(channel, x, mr)

  maxar_df =
    response_df |>
    dplyr::group_by(channel) |>
    dplyr::filter(ar == max(ar, na.rm = TRUE)) |>
    select(channel, x, ar)

  x_range_df =
    maxmr_df |>
    select(channel, x) |>
    left_join(
      maxar_df |> select(channel, x), by = "channel", suffix = c("_min", "_max")
    )

  p = response_df |>
    dplyr::select(channel, x, ar, mr) |>
    tidyr::pivot_longer(cols = c(ar, mr), names_to = "type", values_to = "value") |>
    ggplot2::ggplot(aes(x = x, y = value, color = type)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    #theme(legend.position = "none") +
    ggplot2::facet_wrap(~channel, ncol = ncol) +
    ggplot2::geom_point(data = maxar_df, aes(x = x, y = ar), color = "blue", size = 2) +
    ggplot2::geom_vline(data = maxar_df, aes(xintercept = x), color = "blue", linetype = "dashed") +
    ggplot2::geom_point(data = maxmr_df, aes(x = x, y = mr), color = "red", size = 2) +
    ggplot2::geom_vline(data = maxmr_df, aes(xintercept = x), color = "red", linetype = "dashed") +
    #ensure separate annotations for each facet
    ggplot2::geom_rect(
      data = x_range_df,
      ggplot2::aes(xmin = x_min, xmax = x_max, ymin = -Inf, ymax = Inf),
      fill = "green", alpha = 0.2, inherit.aes = FALSE
    ) +
    #add x and y labels to the point
    ggplot2::geom_text(
      data = intersection_df,
      ggplot2::aes(x = x, y = mr, label = paste0("(", round(x, 2), ", ", round(mr, 2), ")")),
      vjust = -1, color = "blue", size = 3
    ) +
    ggplot2::geom_text(
      data = maxmr_df,
      ggplot2::aes(x = x, y = mr, label = paste0("(", round(x, 2), ", ", round(mr, 2), ")")),
      vjust = 2, color = "red", size = 3
    ) +
    ggplot2::labs(x = "x", y = "Rate", title = "Absolute and Marginal Rates of Return")

  return(p)
}
