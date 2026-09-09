#' Print method for mrmfit_tv objects
#'
#' Displays a compact summary of a fitted time-varying response curve model:
#' the channel and KPI, which parameters vary and by which method, the
#' identifiability and convergence gate results (surfaced prominently -- these
#' are safety-relevant and easy to miss otherwise), a trajectory summary for
#' each varying parameter, and the Bayes R2.
#'
#' @param x An \code{mrmfit_tv} object returned by
#'   \code{\link{fit_response_tv}}.
#' @param ... Additional arguments (ignored).
#' @return The object \code{x}, invisibly.
#'
#' @export

print.mrmfit_tv <- function(x, ...) {

  dollar <- function(v) paste0("$", formatC(v, format = "f", big.mark = ",", digits = 0))
  comma  <- function(v) formatC(v, format = "f", big.mark = ",", digits = 0)
  labels <- list(b = "growth rate", c = "floor", d = "ceiling", e = "midpoint")
  fmt_val <- function(p, v) {
    if (p == "b") formatC(v, format = "e", digits = 2)
    else if (p == "e") dollar(v)
    else comma(v)
  }

  cat(cli_rule(paste0("Time-Varying Response Curve: ", x$rc_type)), "\n")
  cat("Channel: ", x$spend_col, "  |  KPI: ", x$kpi_col, "\n", sep = "")
  cat("Varying: ", paste(x$varying, collapse = ", "),
      "  |  Method: ", x$method,
      if (x$method != "gp") paste0(" (k = ", x$k, ")") else "",
      "\n", sep = "")

  # --- Identifiability + convergence gates (prominent, not buried) ---
  cat(cli_rule("Identifiability & Convergence"), "\n")
  ic <- x$identifiability
  if (!is.null(ic)) {
    cat(sprintf("  Pre-fit spend ratio: %.2f (threshold %.2f)%s\n",
                ic$spend_ratio, ic$threshold_used,
                if (ic$flag) " -- FLAGGED (advisory)" else ""))
  } else {
    cat("  Pre-fit check: skipped (identifiability_check = FALSE)\n")
  }
  dg <- x$diagnostics
  if (!is.null(dg)) {
    cat(sprintf("  Post-fit: max R-hat %.3f | min ESS %.0f | %d divergent | %.1f%% max-treedepth%s\n",
                dg$max_rhat, dg$min_ess_bulk, dg$n_divergent, dg$pct_max_treedepth,
                if (dg$flag) " -- FLAGGED (authoritative)" else ""))
    if (dg$flag) {
      cat("  This can mean genuine posterior multimodality, not a tuning problem.\n")
    }
  }

  # --- Trajectory summary per varying parameter ---
  cat(cli_rule("Trajectory"), "\n")
  traj <- x$trajectory
  if (!is.null(traj)) {
    for (p in x$varying) {
      tp <- traj[traj$param == p, ]
      tp <- tp[order(tp$t), ]
      start_v <- tp$center[1]; end_v <- tp$center[nrow(tp)]
      cat(sprintf("  %-20s start %s   end %s   range [%s, %s]\n",
                  paste0(p, " (", labels[[p]], "):"),
                  fmt_val(p, start_v), fmt_val(p, end_v),
                  fmt_val(p, min(tp$center)), fmt_val(p, max(tp$center))))
    }
  } else {
    cat("  Trajectory not available.\n")
  }

  # --- Bayes R2 ---
  cat(cli_rule("Bayes R2"), "\n")
  if (!is.null(x$R2)) {
    cat(sprintf("  R2: %.4f (95%% CI: [%.4f, %.4f])\n",
                x$R2$Estimate, x$R2$Q2.5, x$R2$Q97.5))
  } else {
    cat("  R2 not available.\n")
  }

  cat("\nUse mrm_plot_tv(x) to visualize; mrm_tv_snapshot(x) for a static curve ",
      "usable with opt_mix().\n", sep = "")

  invisible(x)
}
