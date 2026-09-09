#' Extract a time-varying parameter trajectory from a fitted mrmfit_tv object
#'
#' For each parameter in \code{mrm$varying}, evaluates
#' \code{brms::posterior_linpred(nlpar = <param>)} over a grid of the time
#' index and unscales the result to original data units, using the same
#' affine transforms \code{hlpr_unscale_params()} applies elsewhere in
#' the package.
#'
#' @param mrm A fitted \code{mrmfit_tv} object (itself a \code{brmsfit}, per
#'   \code{\link{fit_response_tv}}'s construction).
#' @param t_grid Numeric vector of time-index values (in \code{[0, 1]}, the
#'   same scale the model was fit on) to evaluate the trajectory at. Default
#'   is 200 evenly spaced points.
#' @return A long-format tibble with columns \code{t}, \code{date}, \code{param},
#'   \code{center}, \code{lower}, \code{upper} -- one row per (\code{t_grid}
#'   point) x (varying parameter).
#' @keywords internal

hlpr_params_tv <- function(mrm, t_grid = seq(0, 1, length.out = 200)) {

  sv      <- mrm$scale_values
  rc_type <- mrm$rc_type
  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  is_log  <- rc_type %in% log_forms

  x_range <- if (!is.null(sv$x_min) && !is.null(sv$x_max)) sv$x_max - sv$x_min else NULL
  y_range <- if (!is.null(sv$y_min) && !is.null(sv$y_max)) sv$y_max - sv$y_min else NULL
  x_offset <- if (!is.null(sv$x_offset)) sv$x_offset else 0

  unscale_fn <- function(param, z) {
    if (is.null(sv)) return(z)
    switch(param,
      b = if (is_log) z else z / x_range,
      e = if (is_log) exp(z) * x_range + sv$x_min - x_offset
          else z * x_range + sv$x_min - x_offset,
      c = z * y_range + sv$y_min,
      d = z * y_range + sv$y_min
    )
  }

  # Model data columns are (spend, kpi, t) in that fixed order (see
  # fit_response_tv.R), addressed positionally because column sanitization
  # means the original spend_col/kpi_col names may not match the actual
  # column names in $data -- the same reason as_mrmfit_list()'s data[[2]]
  # convention exists.
  x_col <- names(mrm$data)[1]
  y_col <- names(mrm$data)[2]

  nd <- data.frame(t = t_grid)
  nd[[x_col]] <- stats::median(mrm$data[[x_col]], na.rm = TRUE)
  nd[[y_col]] <- 0

  date_min <- mrm$date_range[1]
  date_max <- mrm$date_range[2]
  dates <- date_min + t_grid * as.numeric(date_max - date_min)

  purrr::map_dfr(mrm$varying, function(p) {
    nlpar <- if (is_log && p == "e") "le" else p
    lp <- brms::posterior_linpred(mrm, newdata = nd, nlpar = nlpar)
    center <- apply(lp, 2, stats::median)
    lower  <- apply(lp, 2, stats::quantile, 0.025)
    upper  <- apply(lp, 2, stats::quantile, 0.975)
    tibble::tibble(
      t      = t_grid,
      date   = dates,
      param  = p,
      center = unscale_fn(p, center),
      lower  = unscale_fn(p, lower),
      upper  = unscale_fn(p, upper)
    )
  })
}
