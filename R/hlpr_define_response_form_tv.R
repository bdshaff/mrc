#' Define a time-varying response form for a nonlinear model
#'
#' Builds a \code{brms} nonlinear formula for a time-varying response curve.
#' The curve math is identical to \code{\link{hlpr_define_response_form}}
#' (same \code{b}/\code{c}/\code{d}/\code{e} parameterization); the difference
#' is that selected parameters are modeled as smooth or Gaussian-process
#' functions of a time index \code{t} instead of a constant.
#'
#' @param type A character string specifying the response form. Valid options
#'   are "logistic", "log_logistic", "gompertz", "reflected_gompertz",
#'   "weibull", and "reflected_weibull".
#' @param x A character string with the (sanitized) name of the predictor
#'   (spend) variable.
#' @param y A character string with the (sanitized) name of the response (KPI)
#'   variable.
#' @param t A character string with the name of the time-index column
#'   (numeric, typically scaled to \code{[0, 1]}).
#' @param varying A character vector naming which of \code{b}, \code{c},
#'   \code{d}, \code{e} vary over time. Parameters not listed are modeled with
#'   a population-level intercept only (\code{~ 1}).
#' @param method One of \code{"spline"} (\code{s(t)}), \code{"gp_approx"}
#'   (Hilbert-space approximate Gaussian process, \code{gp(t, k = k, c = 5/4)}),
#'   or \code{"gp"} (exact Gaussian process, \code{gp(t)}).
#' @param k Basis dimension for \code{"spline"}/\code{"gp_approx"}. Ignored
#'   for \code{method = "gp"} (exact GP has no basis dimension).
#'
#' @return A \code{brmsformula} object with \code{nl = TRUE}.
#' @details
#' For log-based forms the midpoint enters as \code{log(e)}, which requires
#' \code{e > 0}. A time-varying \code{e} (via a smooth or GP term) can violate
#' that during sampling, so -- exactly as
#' \code{\link{hlpr_define_response_form_hier}} does for group-level
#' deviations -- the midpoint is reparameterized on the log scale: an internal
#' parameter \code{le} (= \code{log(e)}, unconstrained) replaces \code{log(e)}
#' in the formula. This reparameterization applies to every log-based fit,
#' whether or not \code{e} is in \code{varying}, because it is a property of
#' the formula, not of which parameters are time-varying. \code{le} is
#' translated back to \code{e} at extraction time (see
#' \code{\link{hlpr_params_tv}}), so nothing downstream sees \code{le}.
#'
#' @seealso \code{\link{hlpr_define_response_form}},
#'   \code{\link{hlpr_define_response_form_hier}}, \code{\link{fit_response_tv}}
#' @keywords internal

hlpr_define_response_form_tv <- function(type, x = NULL, y = NULL, t = NULL,
                                         varying = "e",
                                         method = "spline",
                                         k = 10) {

  if (is.null(x) || is.null(y)) {
    stop("Both 'x' and 'y' must be provided and cannot be NULL.", call. = FALSE)
  }
  if (is.null(t)) {
    stop("'t' (the time-index column name) must be provided.", call. = FALSE)
  }
  if (is.null(varying) || length(varying) < 1) {
    stop("'varying' must contain at least one parameter name.", call. = FALSE)
  }

  varying <- intersect(varying, c("b", "c", "d", "e"))

  # --- Log-form midpoint reparameterization (e -> le = log(e)) ---
  # Mirrors hlpr_define_response_form_hier.R exactly.
  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  is_log <- type %in% log_forms
  midpoint_par <- if (is_log) "le" else "e"
  valid_pars <- c("b", "c", "d", midpoint_par)
  out_name <- function(vp) if (vp == "le") "e" else vp

  if (is_log) {
    log_forms_le <- list(
      log_logistic      = y ~ c + ((d - c) / (1 + exp(b * (log(x) - le)))),
      weibull            = y ~ c + (d - c) * exp(-exp(b * (log(x) - le))),
      reflected_weibull  = y ~ c + (d - c) * (1 - exp(-exp(b * (-log(x) + le))))
    )
    main_form <- hlpr_replace_variables_in_formula(
      log_forms_le[[type]], old_vars = c("x", "y"), new_vars = c(x, y))
  } else {
    base_bf <- hlpr_define_response_form(type, x, y)
    main_form <- base_bf$formula
    if (is.null(main_form)) {
      stop("Could not extract the base response formula for type '", type, "'.",
           call. = FALSE)
    }
  }

  # --- Time-varying term for each parameter in `varying` ---
  time_term <- switch(method,
    spline    = paste0("s(", t, ", k = ", k, ")"),
    gp_approx = paste0("gp(", t, ", k = ", k, ", c = 5/4)"),
    gp        = paste0("gp(", t, ")"),
    stop("Unknown `method`: '", method, "'. Must be one of: spline, gp_approx, gp.",
         call. = FALSE)
  )

  par_formulas <- lapply(valid_pars, function(p) {
    if (out_name(p) %in% varying) {
      stats::as.formula(paste0(p, " ~ ", time_term))
    } else {
      stats::as.formula(paste0(p, " ~ 1"))
    }
  })

  resp_form <- do.call(
    brms::bf,
    c(list(main_form), par_formulas, list(nl = TRUE))
  )

  resp_form
}
