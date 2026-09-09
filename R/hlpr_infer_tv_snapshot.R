# Internal helper: build a response_df for a mrm_tv_snapshot() view.
#
# Mirrors mrm_infer()'s body (R/mrm_infer.R) almost verbatim -- the ar/mr/cp
# arithmetic is copied exactly, not re-derived, since a subtle sign or
# off-by-one error there would silently corrupt opt_mix()'s decisions. The
# one structural difference mrm_infer() cannot handle is that a time-varying
# fit's predict()/fitted() calls need a `t` column in newdata (held fixed at
# the snapshot's target date) in addition to the spend grid, and the
# analytical center line needs parameter values evaluated AT that date
# (passed in as `center_params`) rather than mrm_infer()'s population-level
# hlpr_params(mrm)$center.
#
# mrm    the PARENT mrmfit_tv object (a real brmsfit; predict()/fitted() run
#        directly on it)
# t_target  scalar time index in [0, 1], the snapshot's target date
# center_params  list(b=,c=,d=,e=) evaluated at t_target, unscaled -- the
#        analytical curve's point-estimate parameters
#
# Returns a response_df with the same column set mrm_infer() produces.

hlpr_infer_tv_snapshot <- function(mrm, t_target, center_params,
                                   xrange = NULL, length.out = 1000) {

  rc_type <- mrm$rc_type
  rc_data <- mrm$data
  y <- mrm$formula$resp
  x <- setdiff(names(rc_data), c(y, "t"))

  if (is.null(xrange)) {
    x_min_obs <- min(rc_data[[x]], na.rm = TRUE)
    xrange <- c(x_min_obs, 2 * max(rc_data[[x]], na.rm = TRUE))
  }

  xseq <- seq(xrange[1], xrange[2], length.out = length.out)
  new_df <- data.frame(x = xseq, t = t_target)
  colnames(new_df) <- c(x, "t")

  pred_df <- as.data.frame(predict(mrm, newdata = new_df))
  mu_df   <- as.data.frame(fitted(mrm, newdata = new_df))

  unscale_y <- function(df, sv) {
    if (!is.null(sv$y_min) && !is.null(sv$y_max)) {
      y_range <- sv$y_max - sv$y_min
      for (col in colnames(df)) {
        if (grepl("Est.Error", col)) df[[col]] <- df[[col]] * y_range
        else df[[col]] <- df[[col]] * y_range + sv$y_min
      }
    } else if (!is.null(sv$y_mean) && !is.null(sv$y_sd)) {
      for (col in colnames(df)) {
        if (grepl("Est.Error", col)) df[[col]] <- df[[col]] * sv$y_sd
        else df[[col]] <- df[[col]] * sv$y_sd + sv$y_mean
      }
    }
    df
  }

  sv <- mrm$scale_values
  if (!is.null(sv)) {
    x_offset <- if (!is.null(sv$x_offset)) sv$x_offset else 0
    if (!is.null(sv$x_min) && !is.null(sv$x_max)) {
      x_range <- sv$x_max - sv$x_min
      xseq <- xseq * x_range + sv$x_min - x_offset
      new_df[[x]] <- new_df[[x]] * x_range + sv$x_min - x_offset
    } else if (!is.null(sv$x_mean) && !is.null(sv$x_sd)) {
      xseq <- xseq * sv$x_sd + sv$x_mean - x_offset
      new_df[[x]] <- new_df[[x]] * sv$x_sd + sv$x_mean - x_offset
    }
    pred_df <- unscale_y(pred_df, sv)
    mu_df   <- unscale_y(mu_df, sv)
  }

  center_response <- response(xseq, center_params, type = rc_type)

  lower_smooth <- pmax(smooth.spline(pred_df$Q2.5)$y, 0)
  upper_smooth <- smooth.spline(pred_df$Q97.5)$y
  lower_mu_smooth <- pmax(smooth.spline(mu_df$Q2.5)$y, 0)
  upper_mu_smooth <- smooth.spline(mu_df$Q97.5)$y

  model_response <- data.frame(
    center = center_response, lower = lower_smooth, upper = upper_smooth,
    lower_mu = lower_mu_smooth, upper_mu = upper_mu_smooth
  )

  new_df_out <- new_df[, x, drop = FALSE]
  res_df <- cbind(new_df_out, pred_df, model_response)
  res_df$type <- rc_type
  res_df$resp_var <- y
  res_df$input_var <- x

  yv <- "center"
  res_df$ar <- (res_df[[yv]] - min(res_df[[yv]])) / res_df[[x]]
  res_df$mr <- c(NA, diff(res_df[[yv]]) / diff(res_df[[x]]))
  res_df$cp <- (res_df[[x]] / (res_df[[yv]] - min(res_df[[yv]])) * (sum(res_df[[yv]] - min(res_df[[yv]])) / sum(res_df[[yv]])))
  res_df$cp_lower <- (res_df[[x]] / (res_df[[yv]] - min(res_df[[yv]])))

  yv <- "lower"
  res_df$ar_lower <- (res_df[[yv]] - min(res_df[[yv]])) / res_df[[x]]
  res_df$mr_lower <- c(NA, diff(res_df[[yv]]) / diff(res_df[[x]]))

  yv <- "upper"
  res_df$ar_upper <- (res_df[[yv]] - min(res_df[[yv]])) / res_df[[x]]
  res_df$mr_upper <- c(NA, diff(res_df[[yv]]) / diff(res_df[[x]]))
  res_df$cp_upper <- (res_df[[x]] / (res_df[[yv]] - min(res_df[[yv]])) * (sum(res_df[[yv]] - min(res_df[[yv]])) / sum(res_df[[yv]])))

  yv <- "lower_mu"
  res_df$ar_lower_mu <- (res_df[[yv]] - min(res_df[[yv]])) / res_df[[x]]
  res_df$mr_lower_mu <- c(NA, diff(res_df[[yv]]) / diff(res_df[[x]]))

  yv <- "upper_mu"
  res_df$ar_upper_mu <- (res_df[[yv]] - min(res_df[[yv]])) / res_df[[x]]
  res_df$mr_upper_mu <- c(NA, diff(res_df[[yv]]) / diff(res_df[[x]]))
  res_df$cp_upper_mu <- (res_df[[x]] / (res_df[[yv]] - min(res_df[[yv]])) * (sum(res_df[[yv]] - min(res_df[[yv]])) / sum(res_df[[yv]])))

  if (!is.null(mrm$units_col) && !is.null(mrm$cost_per_unit)) {
    cpu <- mrm$cost_per_unit
    x_col <- names(res_df)[1]
    res_df$units <- res_df[[x_col]] / cpu
  }

  res_df
}
