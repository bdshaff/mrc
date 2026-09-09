#' Fit a time-varying response curve model using brms
#'
#' Fits a response curve model in which one or more of the curve parameters
#' (\code{b}, \code{c}, \code{d}, \code{e}) are modeled as smooth or
#' Gaussian-process functions of time, instead of being constant across the
#' whole fitted period. This lets the response curve itself evolve -- a
#' growing ceiling, a drifting midpoint -- without the fixed-bucket tradeoffs
#' of \code{\link{fit_response_hier}} (see Details).
#'
#' @inheritParams fit_response
#' @param varying Character vector naming which of \code{b}, \code{c},
#'   \code{d}, \code{e} vary over time; parameters not listed stay constant
#'   (population-level). Default \code{"e"} (see Details for why).
#'   \code{"c"} (the floor) is accepted but warns, since it rarely drifts.
#' @param method One of \code{"spline"} (\code{s(t)}, the default),
#'   \code{"gp_approx"} (Hilbert-space approximate Gaussian process,
#'   comparable cost to \code{"spline"}), or \code{"gp"} (exact Gaussian
#'   process -- available, but see Details for why it is not recommended as a
#'   default).
#' @param k Basis dimension for \code{"spline"}/\code{"gp_approx"}. Ignored
#'   (with a warning if explicitly supplied) for \code{method = "gp"}.
#' @param identifiability_check Logical; run the pre-fit spend-variability
#'   heuristic in \code{\link{hlpr_tv_identifiability}} before sampling.
#'   Default \code{TRUE}. This is advisory only -- see Details.
#' @param min_spend_ratio,min_spend_ratio_multi Thresholds passed to
#'   \code{\link{hlpr_tv_identifiability}}.
#' @return A fitted model object of class \code{mrmfit_tv}.
#'
#' @details
#' **Why `varying = "e"` by default.** Three response curve parameterizations
#' were compared across three channels with very different spend variation
#' (robust quantile ratios of roughly 1.9x, 5-31x, and 800x) during this
#' feature's development. Letting the midpoint (\code{e}) vary was the only
#' choice that converged cleanly on all three; the ceiling (\code{d}) was
#' close behind; letting the steepness (\code{b}) vary, or letting two
#' parameters vary jointly, produced genuine posterior multimodality on the
#' narrowest-range channel that six times the warmup/iterations and a
#' stricter \code{adapt_delta} did not resolve. \code{d}, \code{b}, and joint
#' combinations remain available -- just not the default.
#'
#' **Two-layer identifiability gate.** \code{identifiability_check} runs a
#' fast pre-fit heuristic (see \code{\link{hlpr_tv_identifiability}}) based on
#' spend variability, before the (potentially slow) sampler call -- but it is
#' advisory, built from limited evidence. The **post-fit convergence check is
#' authoritative**: after fitting, R-hat, effective sample size, divergences,
#' and treedepth saturation are checked across all parameters (not just the
#' population ones -- smooth/GP coefficients need to mix well too), and a
#' consolidated warning fires if any threshold is crossed, with a pointer to
#' \code{$diagnostics} and concrete next steps. `print()` surfaces both gates'
#' results prominently.
#'
#' **Method cost.** Measured on one channel: \code{"spline"} -- 76s, 0
#' divergences, 0% max-treedepth. Exact \code{"gp"} -- 2,914s (49 minutes)
#' *even parallelized across 4 cores*, and 25% of transitions hit max
#' treedepth (a real sampler-geometry problem, not just slowness).
#' \code{"gp_approx"} matched \code{"spline"}'s cost and cleanliness. Default
#' to \code{"spline"}; reach for \code{"gp"} only when its smoothness
#' properties are specifically needed and the cost is acceptable.
#'
#' **Relationship to `fit_response_hier()`.** Hierarchical time-bucketing
#' (\code{fit_response_hier(group = <time bucket>)}) already exists and works
#' for the ceiling alone at moderate bucket counts, but is unsafe for shape
#' parameters at real-world bucket counts: with 9 quarterly buckets, pooling
#' only the ceiling freezes the shape parameters at a population value that
#' can fit no individual bucket, while pooling all three shape/scale
#' parameters together can fail to converge outright because too few groups
#' remain to estimate multiple group-level variances. \code{fit_response_tv()}
#' avoids both failure modes by not bucketing at all.
#'
#' **Optimization.** A fitted \code{mrmfit_tv} does not itself feed
#' \code{\link{opt_mix}} -- its curve is a function of time, not a single
#' static curve. Use \code{\link{mrm_tv_snapshot}} to extract a static curve
#' (with full posterior uncertainty) at a specific date, which does interop
#' with \code{opt_mix()}.
#'
#' @seealso \code{\link{fit_response}}, \code{\link{fit_response_hier}},
#'   \code{\link{mrm_tv_snapshot}}, \code{\link{mrm_plot_tv}}
#' @importFrom stats median quantile
#' @export

fit_response_tv <- function(data,
                            spend = NULL,
                            kpi = NULL,
                            date = NULL,
                            units = NULL,
                            type = "gompertz",
                            varying = "e",
                            method = c("spline", "gp_approx", "gp"),
                            k = 10,
                            identifiability_check = TRUE,
                            min_spend_ratio = 3,
                            min_spend_ratio_multi = 8,
                            auto = TRUE,
                            scale_data = TRUE,
                            scale_method = "min_max",
                            midpoint_range = NULL,
                            ceiling_max = NULL,
                            floor_min = NULL,
                            prior = NULL,
                            anchor_strength = NULL,
                            anchor_zero = NULL,
                            chains = 4,
                            iter = 4000,
                            warmup = 1000,
                            control = list(adapt_delta = 0.95, max_treedepth = 12),
                            backend = "cmdstanr",
                            refresh = 500,
                            ...) {

  # --- Resolve backend (cmdstanr preferred, rstan fallback) ---
  backend <- hlpr_resolve_backend(backend)

  # --- method / k cross-check ---
  method <- match.arg(method)
  if (method == "gp" && !missing(k)) {
    warning(
      "`method = \"gp\"` uses an exact covariance and ignores `k`; `k` is ",
      "only used by `method = \"spline\"`/`\"gp_approx\"`.",
      call. = FALSE
    )
  }

  # --- Guard: warmup must be less than iter (matches fit_response) ---
  if (warmup >= iter) {
    new_warmup <- floor(0.5 * iter)
    warning(
      "`warmup` (", warmup, ") must be less than `iter` (", iter, "). ",
      "Setting `warmup = ", new_warmup, "`.",
      call. = FALSE
    )
    warmup <- new_warmup
  }

  # --- Map domain names to internal x/y ---
  x <- spend
  y <- kpi

  if (is.null(x) || is.null(y) || is.null(date)) {
    stop("'spend', 'kpi', and 'date' must all be specified.", call. = FALSE)
  }

  missing_cols <- setdiff(c(x, y, date), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Column(s) not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # --- varying validation ---
  if (is.null(varying) || length(varying) == 0 || all(is.na(varying))) {
    stop(
      "`varying` must specify at least one of 'b', 'c', 'd', 'e'.",
      call. = FALSE
    )
  }
  varying <- unique(varying)
  bad_varying <- setdiff(varying, c("b", "c", "d", "e"))
  if (length(bad_varying) > 0) {
    stop(
      "`varying` contains unrecognized parameter name(s): ",
      paste(bad_varying, collapse = ", "),
      ". Must be a subset of 'b', 'c', 'd', 'e'.",
      call. = FALSE
    )
  }
  if ("c" %in% varying) {
    warning(
      "`varying` includes 'c' (the floor). This rarely drifts over time; ",
      "confirm this is intended.",
      call. = FALSE
    )
  }

  # --- Validate and compute cost_per_unit from units (matches fit_response) ---
  cost_per_unit <- 1.0
  if (!is.null(units)) {
    if (!(units %in% names(data))) {
      stop("Units column '", units, "' not found in data.", call. = FALSE)
    }
    if (any(is.na(data[[units]]))) {
      stop("Units column '", units, "' contains NA values.", call. = FALSE)
    }
    if (any(data[[units]] == 0)) {
      stop("Units column '", units, "' contains zero values; cannot compute cost per unit.",
           call. = FALSE)
    }
    cost_per_unit <- sum(data[[x]], na.rm = TRUE) / sum(data[[units]], na.rm = TRUE)
  }

  if (!is.null(anchor_zero)) {
    warning(
      "`anchor_zero` is deprecated. Floor anchoring is now handled via ",
      "`anchor_strength` in mrm_prior(). See ?mrmopt_prior for details.",
      call. = FALSE
    )
  }

  has_simple_prior <- !is.null(midpoint_range) || !is.null(ceiling_max) ||
    !is.null(floor_min) || !is.null(anchor_strength)
  has_raw_prior <- !is.null(prior) && inherits(prior, "brmsprior")

  if (has_simple_prior && has_raw_prior) {
    stop(
      "Cannot specify both a raw `brmsprior` and simplified prior arguments ",
      "(`midpoint_range`, `ceiling_max`, `floor_min`, `anchor_strength`). ",
      "Use one or the other.",
      call. = FALSE
    )
  }

  if (auto) {
    scale_data <- TRUE
    if (has_raw_prior) {
      warning(
        "In auto mode, raw `prior` is ignored. ",
        "Set `auto = FALSE` to use a custom brmsprior.",
        call. = FALSE
      )
      prior <- NULL
    }
  }

  # --- Date range + time index ---
  # Captured/computed before subsetting, same ordering fit_response() uses.
  date_range <- range(data[[date]], na.rm = TRUE)
  if (isTRUE(date_range[1] == date_range[2])) {
    stop(
      "`date` has only 1 distinct value; a time-varying curve requires at ",
      "least 2 distinct dates.",
      call. = FALSE
    )
  }
  t_vec <- as.numeric(data[[date]] - date_range[1]) / as.numeric(date_range[2] - date_range[1])

  # --- Pre-fit identifiability heuristic (advisory; see ?hlpr_tv_identifiability) ---
  if (identifiability_check) {
    id_check <- hlpr_tv_identifiability(
      spend = data[[x]], varying = varying,
      min_spend_ratio = min_spend_ratio,
      min_spend_ratio_multi = min_spend_ratio_multi
    )
    if (id_check$flag) {
      warning(
        "Spend variability looks low for the requested `varying` set: ",
        "robust ratio (p95/p05) = ", round(id_check$spend_ratio, 2),
        ", below the threshold of ", id_check$threshold_used,
        " used for ", if (id_check$risky_request) "this multi-parameter/steepness request" else "a single parameter",
        ". This is an advisory heuristic, not a certainty \u2014 the post-fit ",
        "convergence check (R-hat, ESS, divergences) is authoritative. If it ",
        "also flags trouble, consider `varying = \"e\"` alone or `method = ",
        "\"gp_approx\"`.",
        call. = FALSE
      )
    }
  } else {
    id_check <- NULL
  }

  # Subset to model columns; keep original column order (x, y) then append t.
  data <- data[, c(x, y)]

  if (any(data[[y]] < 0, na.rm = TRUE)) {
    stop(
      "Response variable '", y, "' contains negative values. ",
      "Response curve models require non-negative response data.",
      call. = FALSE
    )
  }

  # --- Scaling (t is NOT passed through hlpr_scale_data -- it is already on
  # [0, 1] by construction and unrelated to the x/y scaling it performs) ---
  if (scale_data) {

    scaled_data_list <- hlpr_scale_data(data, x, y, scale_method, type = type)
    data <- scaled_data_list$scaled_data
    scale_values <- scaled_data_list$scale_values

    if (has_raw_prior) {
      prior <- prior
    } else {
      mrm_prior_args <- list()
      if (!is.null(midpoint_range)) mrm_prior_args$midpoint_range <- midpoint_range
      if (!is.null(ceiling_max)) mrm_prior_args$ceiling_max <- ceiling_max
      if (!is.null(floor_min)) mrm_prior_args$floor_min <- floor_min
      if (!is.null(anchor_strength)) mrm_prior_args$anchor_strength <- anchor_strength

      user_mrm_prior <- do.call(mrmopt_prior, mrm_prior_args)

      prior <- hlpr_resolve_prior_tv(
        mrm_prior = user_mrm_prior,
        scaled_data = data,
        x = x, y = y,
        scale_method = scale_method,
        scale_values = scale_values,
        type = type
      )
    }

  } else {
    scale_values <- NULL
    if (is.null(prior)) {
      stop("If scale_data is FALSE, a `prior` (brmsprior object) must be provided.",
           call. = FALSE)
    }
    if (!inherits(prior, "brmsprior")) {
      stop("The provided prior is not of class 'brmsprior'. Please provide a valid prior.",
           call. = FALSE)
    }
    log_forms <- c("log_logistic", "weibull", "reflected_weibull")
    required_pars <- if (type %in% log_forms) c("b", "c", "d", "le") else c("b", "c", "d", "e")
    if (!all(required_pars %in% prior$nlpar)) {
      stop("The provided prior does not contain all required parameters (",
           paste(required_pars, collapse = ", "), ").",
           call. = FALSE)
    }
  }

  data$t <- t_vec

  # Rename spend/kpi columns by removing any _ or . (matches fit_response());
  # `t` is a synthetic name that never needs sanitizing.
  names(data)[1:2] <- gsub("[_.]", "", names(data)[1:2])

  rc_formula <- hlpr_define_response_form_tv(
    type, names(data)[1], names(data)[2], t = "t",
    varying = varying, method = method, k = k
  )
  print(rc_formula)

  fit <- brms::brm(
    rc_formula,
    data = data,
    prior = prior,
    chains = chains,
    iter = iter,
    warmup = warmup,
    control = control,
    backend = backend,
    refresh = refresh,
    ...
  )

  fit$scale_values <- scale_values
  fit$scale_method <- if (scale_data) scale_method else NULL
  fit$date_range <- date_range
  fit$rc_type <- type
  fit$spend_col <- x
  fit$kpi_col <- y
  fit$cost_per_unit <- if (!is.null(units)) cost_per_unit else NULL
  fit$date_col <- date
  fit$units_col <- units
  fit$varying <- varying
  fit$method <- method
  fit$k <- k
  fit$identifiability <- id_check
  fit$formula <- rc_formula

  # Assign class before post-processing so downstream calls (posterior_linpred,
  # nuts_params) operate on an object already carrying the metadata they need.
  class(fit) <- c("mrmfit_tv", class(fit))

  # --- Post-fit convergence gate (authoritative; see Details above) ---
  np <- brms::nuts_params(fit)
  draw_summary <- posterior::summarise_draws(posterior::as_draws_df(fit))
  max_td <- if (!is.null(control$max_treedepth)) control$max_treedepth else 10
  n_divergent <- sum(np$Parameter == "divergent__" & np$Value > 0)
  pct_max_td <- 100 * mean(np$Parameter == "treedepth__" & np$Value >= max_td)
  max_rhat <- max(draw_summary$rhat, na.rm = TRUE)
  min_ess_bulk <- min(draw_summary$ess_bulk, na.rm = TRUE)

  diagnostics <- list(
    n_divergent = n_divergent, pct_max_treedepth = pct_max_td,
    max_rhat = max_rhat, min_ess_bulk = min_ess_bulk,
    flag = (max_rhat > 1.01) || (n_divergent > 0) || (pct_max_td > 1) || (min_ess_bulk < 400)
  )
  fit$diagnostics <- diagnostics

  if (diagnostics$flag) {
    warning(
      "Convergence check flagged this fit: max R-hat = ", round(max_rhat, 3),
      ", min ESS = ", round(min_ess_bulk), ", ", n_divergent,
      " divergent transition(s), ", round(pct_max_td, 1), "% max-treedepth. ",
      "This can mean genuine posterior multimodality (more sampling will not ",
      "fix it) rather than a tuning problem. Consider `varying = \"e\"` alone, ",
      "`method = \"gp_approx\"`, or `fit_response()` if this channel's spend ",
      "range cannot support time variation. See `x$diagnostics`.",
      call. = FALSE
    )
  }

  fit$trajectory <- hlpr_params_tv(fit)
  fit$R2 <- tryCatch(tibble::as_tibble(brms::bayes_R2(fit)), error = function(e) NULL)

  fit
}
