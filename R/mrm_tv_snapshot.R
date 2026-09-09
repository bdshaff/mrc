#' Extract a static curve from a time-varying fit at a specific date
#'
#' A fitted \code{\link{fit_response_tv}} model's curve is a function of
#' time, not a single static curve, so it does not itself feed
#' \code{\link{opt_mix}}. \code{mrm_tv_snapshot()} extracts the curve at one
#' date -- with full posterior uncertainty carried through, not just a point
#' estimate -- as an object that behaves like an ordinary
#' \code{\link{fit_response}} fit for every purpose \code{opt_mix()} needs.
#'
#' @param mrm An \code{mrmfit_tv} object returned by
#'   \code{\link{fit_response_tv}}.
#' @param at One of \code{"latest"} (the most recent fitted date, the natural
#'   choice for optimizing against "today's" curve) or \code{"earliest"}.
#'   Ignored if \code{date} is supplied.
#' @param date Optional specific date to snapshot at. A date outside the
#'   fitted period is allowed (the curve extrapolates) but warns.
#' @return An object of class \code{c("mrmfit_tv_snapshot", "mrmfit")} -- it
#'   inherits \code{"mrmfit"} deliberately, unlike the parent \code{mrmfit_tv}
#'   object, because a snapshot at a fixed date *is* a static curve. Carries
#'   the same field contract \code{\link{as_mrmfit_list}} builds for its
#'   per-unit views (\code{data}, \code{scale_values}, \code{response_df},
#'   \code{params_hier_unit}, ...), plus posterior draws of \code{b}/\code{c}/
#'   \code{d}/\code{e} at the target date (not just their medians) so
#'   \code{opt_mix()}'s posterior-based optimization path works exactly as it
#'   does on a real fit.
#'
#' @details
#' Every parameter not in \code{mrm$varying} is frozen at its ordinary
#' population-level posterior; every parameter in \code{mrm$varying} is
#' evaluated at the target date via \code{brms::posterior_linpred()}, so the
#' snapshot's own uncertainty correctly reflects the curve's uncertainty at
#' that specific point in the trajectory (typically wider at the edges of
#' the fitted period than in the middle, for a smooth or GP term).
#'
#' @seealso \code{\link{fit_response_tv}}, \code{\link{opt_mix}},
#'   \code{\link{as_mrmfit_list}}
#' @importFrom stats median quantile
#' @export

mrm_tv_snapshot <- function(mrm, at = c("latest", "earliest"), date = NULL) {

  if (!inherits(mrm, "mrmfit_tv")) {
    stop("mrm must be a fitted model object created by fit_response_tv()", call. = FALSE)
  }
  at <- match.arg(at)

  if (!is.null(date)) {
    target_date <- as.Date(date)
    if (target_date < mrm$date_range[1] || target_date > mrm$date_range[2]) {
      warning(
        "`date` (", format(target_date), ") is outside the fitted period [",
        format(mrm$date_range[1]), ", ", format(mrm$date_range[2]),
        "]; the curve is extrapolating.",
        call. = FALSE
      )
    }
  } else {
    target_date <- if (at == "latest") mrm$date_range[2] else mrm$date_range[1]
  }

  t_target <- as.numeric(target_date - mrm$date_range[1]) /
    as.numeric(mrm$date_range[2] - mrm$date_range[1])

  sv <- mrm$scale_values
  rc_type <- mrm$rc_type
  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  is_log <- rc_type %in% log_forms

  x_col <- names(mrm$data)[1]
  y_col <- names(mrm$data)[2]
  nd <- data.frame(t = t_target)
  nd[[x_col]] <- stats::median(mrm$data[[x_col]], na.rm = TRUE)
  nd[[y_col]] <- 0

  dd <- posterior::as_draws_df(mrm)

  x_range  <- if (!is.null(sv$x_min) && !is.null(sv$x_max)) sv$x_max - sv$x_min else NULL
  y_range  <- if (!is.null(sv$y_min) && !is.null(sv$y_max)) sv$y_max - sv$y_min else NULL
  x_offset <- if (!is.null(sv$x_offset)) sv$x_offset else 0

  extract_raw <- function(p) {
    nlpar <- if (is_log && p == "e") "le" else p
    if (p %in% mrm$varying) {
      lp <- brms::posterior_linpred(mrm, newdata = nd, nlpar = nlpar)
      raw <- as.numeric(lp[, 1])
    } else {
      raw <- dd[[paste0("b_", nlpar, "_Intercept")]]
    }
    if (is_log && p == "e") raw <- exp(raw)
    raw
  }

  unscale_draws <- function(p, raw) {
    if (is.null(sv)) return(raw)
    switch(p,
      b = if (is_log) raw else raw / x_range,
      e = raw * x_range + sv$x_min - x_offset,
      c = raw * y_range + sv$y_min,
      d = raw * y_range + sv$y_min
    )
  }

  pars <- c("b", "c", "d", "e")
  raw_draws    <- stats::setNames(lapply(pars, extract_raw), pars)
  scaled_draws <- stats::setNames(Map(unscale_draws, pars, raw_draws), pars)

  point <- list(
    center = lapply(scaled_draws, stats::median),
    lower  = lapply(scaled_draws, stats::quantile, probs = 0.025, names = FALSE),
    upper  = lapply(scaled_draws, stats::quantile, probs = 0.975, names = FALSE)
  )

  ud <- data.frame(
    b_b_Intercept = scaled_draws$b,
    b_c_Intercept = scaled_draws$c,
    b_d_Intercept = scaled_draws$d,
    b_e_Intercept = scaled_draws$e,
    sigma         = dd$sigma,
    .chain        = dd$.chain,
    .iteration    = dd$.iteration,
    .draw         = dd$.draw
  )
  class(ud) <- c("draws_df", "draws", "tbl_df", "tbl", "data.frame")

  # Per-snapshot observed data: response first, spend second, matching
  # as_mrmfit_list()'s convention (hlpr_get_weekly_spend() reads data[[2]]).
  udata <- data.frame(mrm$data[[y_col]], mrm$data[[x_col]])
  names(udata) <- c(y_col, x_col)

  obj <- list(
    rc_type          = rc_type,
    snapshot_date    = target_date,
    scale_values     = sv,
    scale_method     = mrm$scale_method,
    cost_per_unit    = mrm$cost_per_unit,
    units_col        = mrm$units_col,
    spend_col        = mrm$spend_col,
    kpi_col          = mrm$kpi_col,
    date_col         = mrm$date_col,
    date_range       = mrm$date_range,
    data             = udata,
    formula          = mrm$formula,
    params_hier_unit = point,
    R2               = mrm$R2,
    .snapshot_draws  = ud
  )
  class(obj) <- c("mrmfit_tv_snapshot", "mrmfit")

  obj$response_df <- hlpr_infer_tv_snapshot(mrm, t_target, center_params = point$center)
  obj$summary <- mrm_summary(obj)

  obj
}


#' @rdname mrm_tv_snapshot
#' @param x An \code{mrmfit_tv_snapshot} object.
#' @param ... Additional arguments (currently unused).
#' @exportS3Method posterior::as_draws_df
as_draws_df.mrmfit_tv_snapshot <- function(x, ...) {
  x$.snapshot_draws
}


#' Print method for mrmfit_tv_snapshot objects
#'
#' @param x A \code{mrmfit_tv_snapshot} object from \code{\link{mrm_tv_snapshot}}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.mrmfit_tv_snapshot <- function(x, ...) {
  cat(cli_rule("Time-varying curve snapshot"), "\n")
  cat("Snapshot date: ", format(x$snapshot_date), "\n", sep = "")
  cat("Channel: ", x$spend_col, "  |  KPI: ", x$kpi_col,
      "  |  Type: ", x$rc_type, "\n", sep = "")

  p <- x$params_hier_unit$center
  if (!is.null(p)) {
    cat(sprintf("Params: b=%.3g  c=%.3g  d=%.3g  e=%.3g\n", p$b, p$c, p$d, p$e))
  }

  cat("\nSnapshot from mrm_tv_snapshot() at a fixed date -- a static curve\n",
      "with full posterior uncertainty carried through. Curve/return/cost\n",
      "plots, mrm_params(), mrm_response_function(), mrm_summary(), and\n",
      "opt_mix() work on it. For the underlying time-varying model, use the\n",
      "parent mrmfit_tv (e.g. mrm_plot_tv(<fit>)).\n", sep = "")
  invisible(x)
}
