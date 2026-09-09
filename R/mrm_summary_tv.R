#' Summarize a time-varying response curve fit
#'
#' Produces a tibble with one row per time-varying parameter, summarizing its
#' trajectory (start/end/min/max/net change) alongside the fit's
#' identifiability and convergence diagnostics.
#'
#' @param mrm An \code{mrmfit_tv} object returned by
#'   \code{\link{fit_response_tv}}.
#' @return A tibble with columns \code{param}, \code{start}, \code{end},
#'   \code{min}, \code{max}, \code{net_change}, \code{pct_change}, plus the
#'   fit-level \code{spend_ratio}, \code{max_rhat}, \code{min_ess_bulk},
#'   \code{n_divergent}, \code{pct_max_treedepth}, and \code{flagged}
#'   (logical, \code{TRUE} if either gate flagged the fit).
#' @export

mrm_summary_tv <- function(mrm) {

  if (!inherits(mrm, "mrmfit_tv")) {
    stop("mrm must be a fitted model object created by fit_response_tv()", call. = FALSE)
  }

  traj <- mrm$trajectory
  ic <- mrm$identifiability
  dg <- mrm$diagnostics

  rows <- lapply(mrm$varying, function(p) {
    tp <- traj[traj$param == p, ]
    tp <- tp[order(tp$t), ]
    start_v <- tp$center[1]
    end_v   <- tp$center[nrow(tp)]
    tibble::tibble(
      param      = p,
      start      = start_v,
      end        = end_v,
      min        = min(tp$center),
      max        = max(tp$center),
      net_change = end_v - start_v,
      pct_change = 100 * (end_v - start_v) / start_v
    )
  })

  out <- dplyr::bind_rows(rows)
  out$spend_ratio        <- if (!is.null(ic)) ic$spend_ratio else NA_real_
  out$max_rhat           <- if (!is.null(dg)) dg$max_rhat else NA_real_
  out$min_ess_bulk       <- if (!is.null(dg)) dg$min_ess_bulk else NA_real_
  out$n_divergent        <- if (!is.null(dg)) dg$n_divergent else NA_integer_
  out$pct_max_treedepth  <- if (!is.null(dg)) dg$pct_max_treedepth else NA_real_
  out$flagged            <- (!is.null(ic) && isTRUE(ic$flag)) || (!is.null(dg) && isTRUE(dg$flag))

  out
}
