#' Infer response from a fitted model
#'
#' This function infers the response from a fitted model object.
#' @param mrm A fitted model object.
#' @param xrange A numeric vector of length 2 specifying the range of x values for prediction. Default is NULL, which uses the range of x in the data.
#' @param length.out An integer specifying the number of points to predict. Default is 1000.
#' @param scaled A logical indicating whether the model was fitted on scaled data. Default is TRUE.
#' @return A data frame containing the predicted response values and the model
#'   response. Includes both prediction intervals (\code{lower}, \code{upper} —
#'   include observation noise) and mean-function credible intervals
#'   (\code{lower_mu}, \code{upper_mu} — curve shape uncertainty only).
#' @details The function infers the response from the fitted model object and
#'   returns a data frame with the predicted response values and the model
#'   response. The x-axis is always in raw spend units. When units were supplied
#'   at fit time, a \code{units} column is appended (derived as spend / cpu).
#' @importFrom stats fitted
#' @export

mrm_infer <- function(mrm, xrange = NULL, length.out = 1000, scaled = TRUE) {

  if (!inherits(mrm, "mrmfit")) {
    stop("mrm must be a fitted model object created by fit_response()", call. = FALSE)
  }

  # Per-unit views from as_mrmfit_list() carry no posterior sampler, so they
  # cannot run predict()/fitted() on a fresh grid. Serve the cached inference
  # for default arguments; a custom grid requires the parent mrmfit_hier.
  if (inherits(mrm, "mrmfit_hier_unit")) {
    if (is.null(xrange) && length.out == 1000 && !is.null(mrm$response_df)) {
      hlpr_unit_view_warn("mrm_infer")
      return(mrm$response_df)
    }
    stop(
      "Custom `mrm_infer()` (xrange/length.out) is not available for a per-unit ",
      "view from `as_mrmfit_list()`; its cached `response_df` uses the parent ",
      "fit's inference grid. Call `mrm_infer()` on the parent `mrmfit_hier` for ",
      "a new grid.",
      call. = FALSE
    )
  }

  # Time-snapshot views from mrm_tv_snapshot() carry the same "cached, not
  # refittable" contract as an as_mrmfit_list() per-unit view, for the same
  # reason: the object is derived from posterior draws at one date, not a
  # standalone brmsfit that predict()/fitted() can run on with a fresh grid.
  if (inherits(mrm, "mrmfit_tv_snapshot")) {
    if (is.null(xrange) && length.out == 1000 && !is.null(mrm$response_df)) {
      warning(
        "mrm_infer() is designed for a single static `mrmfit`. You passed a ",
        "time-snapshot view from `mrm_tv_snapshot()`; the result is derived ",
        "from the snapshot's cached values at its target date, not a refit. ",
        "Use the parent `mrmfit_tv` (or `mrm_plot_tv()`) for the full ",
        "time-varying model.",
        call. = FALSE
      )
      return(mrm$response_df)
    }
    stop(
      "Custom `mrm_infer()` (xrange/length.out) is not available for a ",
      "time-snapshot view from `mrm_tv_snapshot()`; its cached `response_df` ",
      "uses the snapshot's own inference grid. Call `mrm_tv_snapshot()` again ",
      "for a different grid.",
      call. = FALSE
    )
  }

  rc_type <- mrm$rc_type
  rc_data <- mrm$data
  y <- mrm$formula$resp
  x <- names(rc_data)[names(rc_data) != y]
  response_params <- hlpr_params(mrm, scaled = scaled)

  if (is.null(xrange)) {
    x_min_obs <- min(rc_data[[x]], na.rm = TRUE)
    # For log-based forms (weibull, log_logistic etc.) x must be > 0.
    # Use the observed minimum rather than 0 to avoid log(0) = -Inf.
    xrange <- c(x_min_obs, 2 * max(rc_data[[x]], na.rm = TRUE))
  }

  if (!is.numeric(xrange) || length(xrange) != 2 || xrange[1] >= xrange[2]) {
    stop("xrange must be a numeric vector of length 2 with the first element less than the second.")
  }

  # Generate model prediction/extrapolation data frame
  xseq <- seq(xrange[1], xrange[2], length.out = length.out)
  new_df <- data.frame(x = xseq)
  colnames(new_df) <- x

  # Compute both prediction intervals (includes sigma) and mean-function
  # credible intervals (curve shape only). Both are stored on response_df
  # so downstream code can choose which to display without re-computation.
  pred_df <- as.data.frame(predict(mrm, newdata = new_df))
  mu_df   <- as.data.frame(fitted(mrm, newdata = new_df))

  # Helper to unscale a set of prediction columns
  unscale_y <- function(df, sv) {
    if (!is.null(sv$y_min) && !is.null(sv$y_max)) {
      y_range <- sv$y_max - sv$y_min
      for (col in colnames(df)) {
        if (grepl("Est.Error", col)) {
          df[[col]] <- df[[col]] * y_range
        } else {
          df[[col]] <- df[[col]] * y_range + sv$y_min
        }
      }
    } else if (!is.null(sv$y_mean) && !is.null(sv$y_sd)) {
      for (col in colnames(df)) {
        if (grepl("Est.Error", col)) {
          df[[col]] <- df[[col]] * sv$y_sd
        } else {
          df[[col]] <- df[[col]] * sv$y_sd + sv$y_mean
        }
      }
    }
    df
  }

  # Re-scale back to original scale if necessary
  if (scaled & !is.null(mrm$scale_values)) {
    sv <- mrm$scale_values
    x_offset <- if (!is.null(sv$x_offset)) sv$x_offset else 0

    # --- Unscale x ---
    if (!is.null(sv$x_min) && !is.null(sv$x_max)) {
      x_range <- sv$x_max - sv$x_min
      xseq <- xseq * x_range + sv$x_min - x_offset
      new_df[[x]] <- new_df[[x]] * x_range + sv$x_min - x_offset
    } else if (!is.null(sv$x_mean) && !is.null(sv$x_sd)) {
      xseq <- xseq * sv$x_sd + sv$x_mean - x_offset
      new_df[[x]] <- new_df[[x]] * sv$x_sd + sv$x_mean - x_offset
    }

    # --- Unscale y ---
    pred_df <- unscale_y(pred_df, sv)
    mu_df   <- unscale_y(mu_df, sv)

  } else {
    # No scaling — data is in raw model space
  }

  # Compute analytical center curve from posterior mean parameters
  center_response <- response(xseq, response_params$center, type = rc_type)

  # Smoothed prediction intervals (includes observation noise)
  lower_smooth <- pmax(smooth.spline(pred_df$Q2.5)$y, 0)
  upper_smooth <- smooth.spline(pred_df$Q97.5)$y

  # Smoothed mean-function credible intervals (curve shape only)
  lower_mu_smooth <- pmax(smooth.spline(mu_df$Q2.5)$y, 0)
  upper_mu_smooth <- smooth.spline(mu_df$Q97.5)$y

  model_response <- data.frame(
    center   = center_response,
    lower    = lower_smooth,
    upper    = upper_smooth,
    lower_mu = lower_mu_smooth,
    upper_mu = upper_mu_smooth
  )

  # Combine all results into a single data frame
  res_df <- cbind(new_df, pred_df, model_response)
  res_df$type <- rc_type
  res_df$resp_var <- y
  res_df$input_var <- x

  # Calculate additional metrics: absolute response (ar) and marginal response (mr)
  y <- "center"
  res_df$ar <- (res_df[[y]] - min(res_df[[y]])) / res_df[[x]]
  res_df$mr <- c(NA, diff(res_df[[y]]) / diff(res_df[[x]]))
  res_df$cp <- (res_df[[x]] / (res_df[[y]] - min(res_df[[y]])) * (sum(res_df[[y]] - min(res_df[[y]])) / sum(res_df[[y]])))
  res_df$cp_lower <- (res_df[[x]] / (res_df[[y]] - min(res_df[[y]])))

  # Derived metrics from prediction intervals
  y <- "lower"
  res_df$ar_lower <- (res_df[[y]] - min(res_df[[y]])) / res_df[[x]]
  res_df$mr_lower <- c(NA, diff(res_df[[y]]) / diff(res_df[[x]]))

  y <- "upper"
  res_df$ar_upper <- (res_df[[y]] - min(res_df[[y]])) / res_df[[x]]
  res_df$mr_upper <- c(NA, diff(res_df[[y]]) / diff(res_df[[x]]))
  res_df$cp_upper <- (res_df[[x]] / (res_df[[y]] - min(res_df[[y]])) * (sum(res_df[[y]] - min(res_df[[y]])) / sum(res_df[[y]])))

  # Derived metrics from mean-function credible intervals
  y <- "lower_mu"
  res_df$ar_lower_mu <- (res_df[[y]] - min(res_df[[y]])) / res_df[[x]]
  res_df$mr_lower_mu <- c(NA, diff(res_df[[y]]) / diff(res_df[[x]]))

  y <- "upper_mu"
  res_df$ar_upper_mu <- (res_df[[y]] - min(res_df[[y]])) / res_df[[x]]
  res_df$mr_upper_mu <- c(NA, diff(res_df[[y]]) / diff(res_df[[x]]))
  res_df$cp_upper_mu <- (res_df[[x]] / (res_df[[y]] - min(res_df[[y]])) * (sum(res_df[[y]] - min(res_df[[y]])) / sum(res_df[[y]])))

  # Add units column when units were supplied at fit time
  if (!is.null(mrm$units_col) && !is.null(mrm$cost_per_unit)) {
    cpu <- mrm$cost_per_unit
    x_col <- names(res_df)[1]
    res_df$units <- res_df[[x_col]] / cpu
  }

  return(res_df)
}
