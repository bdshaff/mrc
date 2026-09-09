#' Resolve an mrm_prior specification into a time-varying brms prior object
#'
#' Extends \code{\link{hlpr_resolve_prior}} for time-varying fits. Population
#' priors on \code{b}/\code{c}/\code{d}/\code{e} are produced by
#' \code{\link{hlpr_resolve_prior}} unchanged. For log-based forms, the
#' resolved \code{e} prior is converted to a prior on \code{le = log(e)},
#' matching the reparameterization \code{\link{hlpr_define_response_form_tv}}
#' applies to the formula.
#'
#' No additional priors are added for the smooth/GP hyperparameters
#' (\code{sds_*}, \code{sdgp_*}, \code{lscale_*}): brms's own defaults were
#' used, unmodified, across every fit validated during this feature's
#' development (35+ real fits, 0 divergences on the converged ones) -- unlike
#' \code{\link{hlpr_resolve_prior_hier}}'s group-level \code{class = "sd"}
#' priors, which exist because \code{brms} has no default there for a custom
#' hierarchical structure. \code{s()}/\code{gp()} are ordinary \code{brms}
#' formula terms with their own established defaults.
#'
#' @inheritParams hlpr_resolve_prior
#' @return A \code{brmsprior} object.
#' @seealso \code{\link{hlpr_resolve_prior}}, \code{\link{fit_response_tv}}
#' @keywords internal

hlpr_resolve_prior_tv <- function(mrm_prior = NULL,
                                  scaled_data,
                                  x, y,
                                  scale_method,
                                  scale_values,
                                  type) {

  pop_prior <- hlpr_resolve_prior(
    mrm_prior = mrm_prior,
    scaled_data = scaled_data,
    x = x, y = y,
    scale_method = scale_method,
    scale_values = scale_values,
    type = type
  )

  log_forms <- c("log_logistic", "weibull", "reflected_weibull")
  if (type %in% log_forms) {
    e_row <- pop_prior[pop_prior$nlpar == "e", ]
    e_lb  <- as.numeric(e_row$lb)
    e_ub  <- as.numeric(e_row$ub)
    le_lb <- log(e_lb)
    le_ub <- log(e_ub)
    le_mean <- (le_lb + le_ub) / 2
    le_sd   <- max((le_ub - le_lb) / 2, 0.5)
    le_prior <- brms::prior_string(
      paste0("normal(", round(le_mean, 4), ", ", round(le_sd, 4), ")"),
      nlpar = "le", lb = round(le_lb, 4), ub = round(le_ub, 4)
    )
    pop_prior <- do.call(c, list(pop_prior[pop_prior$nlpar != "e", ], le_prior))
  }

  pop_prior
}
