## =============================================================================
## Step 3: time-varying response curves via a smooth term on a curve parameter
## =============================================================================
## Gate 1 showed fit_response_hier(group = "quarter", pool = "d") works and
## identifies a substantial group SD (0.664, 90% CI [0.416, 1.17]) -- but the
## binding constraint is only 8 usable buckets from 103 weeks, and the function's
## own default pool = c("b","e","d") diverged badly (458 divergences) because
## three group-level SDs cannot be estimated from 8 groups.
##
## A smooth term sidesteps bucketing entirely: `d ~ s(t)` or `d ~ gp(t)` gives a
## continuous ceiling trajectory with pooling built in, no bucket boundaries, and
## no min_obs dropping of the short 2026Q1 tail.
##
## This script establishes whether that actually fits on real data before any
## package function is written. It deliberately reuses mrmopt's own scaling and
## prior machinery so the result is comparable to the Gate 1 fits.
##
## Run with `Rscript dev/tv_smooth_curves.R`. All fits cached via brms `file=`.
## =============================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(tibble); library(ggplot2); library(brms); library(mrmopt)
})

t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time() - t0, units = "secs")),
                         sprintf(...), "\n", sep = "")

CSV <- "/Users/roeh/decomp_2024-02-04_2026-01-18_2026-02-03 18_39_53.316291+00_00.csv"
CH   <- "clicks_clk_affiliate"   # same channel as Gate 1, for comparability
TYPE <- "gompertz"
cache_dir <- "data-raw/tv_smooth_cache"
fit_dir   <- file.path(cache_dir, "fits")
plot_dir  <- file.path(cache_dir, "plots")
for (d in c(cache_dir, fit_dir, plot_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

## -----------------------------------------------------------------------
## 1. Same weekly series as Gate 1
## -----------------------------------------------------------------------

weekly <- read_csv(CSV, col_select = c(date, variable_type, variable, spend,
                                       total_contrib_opps),
                   show_col_types = FALSE, progress = FALSE) |>
  filter(variable_type == "media", variable == CH) |>
  group_by(date) |>
  summarise(spend = sum(spend), kpi = sum(total_contrib_opps), .groups = "drop") |>
  arrange(date)

say("%s: %d weeks, %s to %s", CH, nrow(weekly),
    format(min(weekly$date)), format(max(weekly$date)))

## -----------------------------------------------------------------------
## 2. Reuse mrmopt's scaling + prior machinery (so this is comparable)
## -----------------------------------------------------------------------

sc <- mrmopt:::hlpr_scale_data(as.data.frame(weekly[, c("spend", "kpi")]),
                               x = "spend", y = "kpi",
                               scale_method = "min_max", type = TYPE)
sv <- sc$scale_values

dat <- sc$scaled_data |>
  mutate(date = weekly$date,
         # normalized time index in [0, 1]; smooths/GPs want a numeric covariate
         t = as.numeric(date - min(date)) / as.numeric(max(date) - min(date)))

base_prior <- mrmopt:::hlpr_resolve_prior(
  mrm_prior = mrmopt_prior(), scaled_data = sc$scaled_data,
  x = "spend", y = "kpi", scale_method = "min_max",
  scale_values = sv, type = TYPE)

cat("\n--- Priors carried over from mrmopt's auto tier ---\n"); print(base_prior)

# d is on the scaled KPI axis: unscale with the same affine map hlpr_unscale_params uses
y_range <- sv$y_max - sv$y_min
unscale_d <- function(z) z * y_range + sv$y_min

## -----------------------------------------------------------------------
## 3. Fits: flat baseline, spline, GP
## -----------------------------------------------------------------------

gomp <- kpi ~ c + (d - c) * exp(-exp(b * (spend - e)))

## Exact gp(t) (O(n^3) covariance, evaluated every leapfrog step inside a
## nonlinear formula) took >900s for a SINGLE chain sequentially -- the
## original run was killed after ~40 min with only 1 of 4 chains done. Refit
## with cores = 4 (this machine has 8) for real parallelism, and add the
## Hilbert-space APPROXIMATE GP (gp(t, k = 10)) as the practical alternative --
## O(n*k) instead of O(n^3), and what a shipped feature would default to.
forms <- list(
  flat      = bf(gomp, b + c + d + e ~ 1,                nl = TRUE),
  spline    = bf(gomp, d ~ s(t), b + c + e ~ 1,           nl = TRUE),
  gp        = bf(gomp, d ~ gp(t), b + c + e ~ 1,          nl = TRUE),
  gp_approx = bf(gomp, d ~ gp(t, k = 10, c = 5/4), b + c + e ~ 1, nl = TRUE)
)

fit_one <- function(nm) {
  say("Fitting %s ...", nm)
  brm(forms[[nm]], data = dat, prior = base_prior,
      chains = 4, iter = 4000, warmup = 1000, refresh = 0,
      cores = 4, backend = "cmdstanr", seed = 4021,
      control = list(adapt_delta = 0.99, max_treedepth = 12),
      file = file.path(fit_dir, nm))
}
fits <- map(set_names(names(forms)), fit_one)

## -----------------------------------------------------------------------
## 4. Diagnostics
## -----------------------------------------------------------------------

diagnostics <- imap_dfr(fits, function(f, nm) {
  np <- nuts_params(f); s <- posterior::summarise_draws(posterior::as_draws_df(f))
  tibble(model = nm,
         n_divergent  = sum(np$Parameter == "divergent__" & np$Value > 0),
         max_rhat     = max(s$rhat, na.rm = TRUE),
         min_ess_bulk = min(s$ess_bulk, na.rm = TRUE),
         loo_elpd     = tryCatch(loo(f)$estimates["elpd_loo", "Estimate"],
                                 error = function(e) NA_real_))
})
write_csv(diagnostics, file.path(cache_dir, "diagnostics.csv"))
cat("\n--- Sampler diagnostics + LOO ---\n"); print(as.data.frame(diagnostics), digits = 4)

## LOO comparison: does letting d vary over time actually earn its keep?
loo_cmp <- tryCatch({
  lc <- loo_compare(loo(fits$flat), loo(fits$spline), loo(fits$gp))
  rownames(lc) <- c("flat", "spline", "gp")[match(rownames(lc), c("model1","model2","model3"))]
  lc
}, error = function(e) { message("loo_compare failed: ", conditionMessage(e)); NULL })
if (!is.null(loo_cmp)) { cat("\n--- LOO comparison (best first) ---\n"); print(loo_cmp) }

## -----------------------------------------------------------------------
## 5. Extract the ceiling trajectory d(t)
## -----------------------------------------------------------------------
## posterior_linpred(nlpar = "d") gives draws of the `d` nonlinear parameter on
## the scaled KPI axis; unscale with the same affine map as hlpr_unscale_params.

nd <- tibble(t = seq(0, 1, length.out = 200), spend = median(dat$spend), kpi = 0) |>
  mutate(date = min(weekly$date) +
           t * as.numeric(max(weekly$date) - min(weekly$date)))

d_traj <- imap_dfr(fits[c("spline", "gp", "gp_approx")], function(f, nm) {
  lp <- posterior_linpred(f, newdata = nd, nlpar = "d")
  tibble(model = nm, date = nd$date, t = nd$t,
         center = unscale_d(apply(lp, 2, median)),
         lower  = unscale_d(apply(lp, 2, quantile, 0.025)),
         upper  = unscale_d(apply(lp, 2, quantile, 0.975)))
})

flat_d <- unscale_d(median(posterior_linpred(fits$flat, newdata = nd[1, ], nlpar = "d")))
say("Flat (no time variation) ceiling: %.0f", flat_d)

write_csv(d_traj, file.path(cache_dir, "ceiling_trajectory.csv"))

## -----------------------------------------------------------------------
## 6. Compare against the Gate 1 quarterly estimates
## -----------------------------------------------------------------------

gate1 <- file.path("data-raw/hier_time_cache/param_comparison.csv")
q_pts <- if (file.exists(gate1)) {
  read_csv(gate1, show_col_types = FALSE) |>
    filter(param == "d") |>
    mutate(q_mid = as.Date(paste0(substr(quarter, 1, 4), "-",
                                  c("02","05","08","11")[as.integer(substr(quarter, 6, 6))],
                                  "-15"))) |>
    select(quarter, q_mid, independent, hier_d)
} else NULL

p <- ggplot(d_traj, aes(date, center)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.25, colour = NA) +
  geom_line(aes(colour = model), linewidth = 0.9) +
  geom_hline(yintercept = flat_d, linetype = "dashed", colour = "grey30")
if (!is.null(q_pts)) {
  p <- p +
    geom_point(data = q_pts, aes(q_mid, independent), colour = "grey35",
               shape = 1, size = 2.6, inherit.aes = FALSE) +
    geom_point(data = q_pts, aes(q_mid, hier_d), colour = "firebrick",
               shape = 17, size = 2.4, inherit.aes = FALSE)
}
p <- p +
  scale_y_continuous(labels = scales::comma) +
  labs(title = paste0(CH, " — ceiling d(t): smooth vs quarterly buckets"),
       subtitle = paste0("Lines/ribbons = s(t) and gp(t) with 95% CI | dashed = flat (no time variation)\n",
                         "open circles = independent per-quarter | red triangles = hierarchical pool = 'd'"),
       x = NULL, y = "Ceiling (d), KPI units", colour = NULL, fill = NULL) +
  theme_minimal() + theme(legend.position = "top")

ggsave(file.path(plot_dir, "01_ceiling_smooth_vs_buckets.png"), p, width = 10, height = 6)

say("Done. Outputs in %s/", cache_dir)
