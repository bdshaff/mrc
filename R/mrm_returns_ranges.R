#' Get Ranges of Maximum Marginal Returns and Average Returns
#' This function computes the ranges of x values corresponding to the maximum marginal returns (MR)
#' and average returns (AR) for a list of marginal response models (MRMs).
#' @param mrm A list of fitted model objects (e.g., brmsfit objects).
#' @param xrange A numeric vector of length 2 specifying the range of x values to consider. If NULL, the range is determined from the data.
#' @return A data frame containing the channels and their corresponding x ranges for maximum MR and AR.
#' @details The function computes the MR and AR for each model in the list and identifies the x values where MR and AR are maximized. It returns a data frame with the channel names and their respective x ranges.
#' @export

mrm_returns_ranges <- function(mrm, xrange = NULL) {

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
    left_join(
      maxar_df, by = "channel", suffix = c("_min", "_max")
    )

  return(x_range_df)
}
