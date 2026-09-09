## =============================================================================
## Utility test #2: fit_response() vs fit_response_tv() on Paid Search (Brand).
## =============================================================================
## Chosen after checking the static R2 gate + actual plot on 4 channels first
## (Linear TV 0.365, YouTube 0.499, Streaming 0.565, Paid Search Brand 0.533).
## Paid Search Brand is the only one whose observed spend range visibly spans
## the curve's inflection AND flattening region -- a genuine, eye-confirmed
## S-curve, not just a mathematically-located "peak" beyond the data (as the
## other three showed). It is also the channel where varying = "e" (the
## default) was already known, from earlier work today, to stay clean when
## varying = "b" or joint variation produced genuine multimodality -- a
## favorable starting point on both counts.
##
## Every lesson from the Linear TV run is built in from the start this time:
##   - check the raw e TRAJECTORY (not just print()'s convergence gate) for
##     physical plausibility against the observed spend range
##   - check log_curve_no_peak PER ROW before trusting peak_mr/peak_ar/decay70
##     (mrm_summary() silently relabels those columns when it's TRUE)
##   - plot each curve on its OWN axes, not only a shared-axis overlay
##     (mrms_plot_compare() can visually flatten a real curve next to a wild one)
## =============================================================================

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(purrr); library(tibble)})
devtools::load_all(quiet = TRUE)
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time()-t0, units="secs")), sprintf(...), "\n", sep="")

cache_dir <- "data-raw/tv_vs_static_cache/paid_search_brand"
plot_dir  <- file.path(cache_dir, "plots")
for (d in c(cache_dir, plot_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

weekly <- readRDS("/private/tmp/claude-503/-Users-roeh-Documents-roeh/5f6df883-8b06-4933-a577-4d0f482ab17c/scratchpad/clicks_paid_search__brand_weekly.rds") |> as.data.frame()
say("Paid Search (Brand): %d weeks, spend [$%.0f, $%.0f]", nrow(weekly), min(weekly$spend), max(weekly$spend))

## -----------------------------------------------------------------------
## 1. Fit both models at real settings (static already cached from the R2-gate check)
## -----------------------------------------------------------------------

fit_static <- fit_response(
  weekly, spend = "spend", kpi = "kpi", date = "date", type = "logistic",
  chains = 4, iter = 4000, warmup = 1000, refresh = 0,
  file = file.path(cache_dir, "static_logistic")
)

say("Fitting fit_response_tv(type='logistic', varying='e')...")
fit_tv <- fit_response_tv(
  weekly, spend = "spend", kpi = "kpi", date = "date", type = "logistic",
  varying = "e", method = "spline",
  chains = 4, iter = 4000, warmup = 1000, refresh = 0,
  file = file.path(cache_dir, "tv_logistic")
)
say("Both fits done.")
print(fit_tv)

## -----------------------------------------------------------------------
## 2. Fit quality
## -----------------------------------------------------------------------

fit_quality <- tibble(
  model = c("static", "time_varying"),
  R2_estimate = c(fit_static$R2$Estimate, fit_tv$R2$Estimate),
  R2_lower    = c(fit_static$R2$Q2.5, fit_tv$R2$Q2.5),
  R2_upper    = c(fit_static$R2$Q97.5, fit_tv$R2$Q97.5)
)
write_csv(fit_quality, file.path(cache_dir, "fit_quality.csv"))
cat("\n--- Fit quality (Bayes R2) ---\n")
print(as.data.frame(fit_quality), digits = 4)
cat("\n--- Convergence gates ---\n")
print(fit_tv$identifiability); print(fit_tv$diagnostics)

## -----------------------------------------------------------------------
## 3. TRAJECTORY PLAUSIBILITY CHECK -- before anything else. Flags any point
## where the varying parameter (e, a spend-scale midpoint) sits outside a
## generous multiple of the observed spend range -- the exact failure mode
## that made Linear TV's headline numbers untrustworthy despite a clean
## convergence gate.
## -----------------------------------------------------------------------

obs_min <- min(weekly$spend); obs_max <- max(weekly$spend)
plaus_lo <- -0.5 * obs_max   # e has no reason to be negative; small negative overshoot tolerated
plaus_hi <- 3 * obs_max

traj <- fit_tv$trajectory
traj$implausible <- traj$center < plaus_lo | traj$center > plaus_hi
pct_implausible <- 100 * mean(traj$implausible)

cat(sprintf("\n--- Trajectory plausibility: e vs observed spend range [$%.0f, $%.0f] ---\n", obs_min, obs_max))
cat(sprintf("Plausible band checked: [$%.0f, $%.0f] (-0.5x to 3x observed max)\n", plaus_lo, plaus_hi))
cat(sprintf("%.1f%% of the trajectory falls OUTSIDE the plausible band\n", pct_implausible))
idx <- round(seq(1, nrow(traj), length.out = 8))
print(traj[idx, c("date","center","lower","upper","implausible")], n = Inf)

## -----------------------------------------------------------------------
## 4. Snapshots across the trajectory
## -----------------------------------------------------------------------

snap_dates <- c(
  early = fit_tv$date_range[1],
  mid   = fit_tv$date_range[1] + round(as.numeric(diff(fit_tv$date_range)) / 2),
  late  = fit_tv$date_range[2]
)
snaps <- imap(snap_dates, function(d, nm) mrm_tv_snapshot(fit_tv, date = d) |> suppressWarnings())

## -----------------------------------------------------------------------
## 5. Credible-band width (mean-function CI), from each fit's own cached grid
## -----------------------------------------------------------------------

common_grid <- seq(0, obs_max, length.out = 200)
band_width <- function(fit, label) {
  rdf <- fit$response_df
  x_col <- names(rdf)[1]
  w <- approx(rdf[[x_col]], rdf$upper_mu - rdf$lower_mu, xout = common_grid, rule = 2)$y
  tibble(model = label, spend = common_grid, ci_width = w,
        pct_of_range = 100 * w / diff(range(rdf$center)))
}
bands <- bind_rows(
  band_width(fit_static, "static"),
  imap_dfr(snaps, ~ band_width(.x, paste0("tv_", .y)))
)
write_csv(bands, file.path(cache_dir, "band_widths.csv"))
band_summary <- bands |> group_by(model) |> summarise(mean_ci_width = mean(ci_width), mean_pct_of_range = mean(pct_of_range), .groups = "drop")
cat("\n--- Mean credible-band width, across the OBSERVED spend range ---\n")
print(as.data.frame(band_summary), digits = 4)

## -----------------------------------------------------------------------
## 6. Saturation / peak-MR / peak-AR -- checking log_curve_no_peak PER ROW
## before trusting the column labels (mrm_summary() relabels them when TRUE).
## -----------------------------------------------------------------------

insight_row <- function(fit, label) {
  s <- mrm_summary(fit)
  no_peak <- isTRUE(attr(s, "log_curve_no_peak"))
  tibble(model = label, log_curve_no_peak = no_peak,
        col1_spend = s$range_min_spend,  # peak MR spend, UNLESS no_peak
        col2_spend = s$range_peak_spend, # peak AR spend, UNLESS no_peak
        col3_spend = s$range_max_spend,  # 70% decay spend, UNLESS no_peak
        r2 = fit$R2$Estimate)
}
insights <- bind_rows(
  insight_row(fit_static, "static (full history)"),
  imap_dfr(snaps, ~ insight_row(.x, paste0("tv snapshot: ", .y, " (", format(.x$snapshot_date), ")")))
)
write_csv(insights, file.path(cache_dir, "insights.csv"))
cat("\n--- Saturation insight (col1/2/3 mean peak_mr/peak_ar/decay70 IF log_curve_no_peak=FALSE) ---\n")
cat("--- IF log_curve_no_peak=TRUE, col1/2/3 instead mean 2x-current-MR / current / 0.5x-current-MR spend ---\n")
print(as.data.frame(insights), digits = 4)

## -----------------------------------------------------------------------
## 7. Plots: each curve on ITS OWN axes (not just a shared-axis overlay)
## -----------------------------------------------------------------------

p_static <- mrm_plot_response(fit_static) + coord_cartesian(xlim = c(0, obs_max))
ggsave(file.path(plot_dir, "01_static_only.png"), p_static, width = 8, height = 5)

for (nm in names(snaps)) {
  p <- tryCatch(mrm_plot_response(snaps[[nm]]) + coord_cartesian(xlim = c(0, obs_max)),
               error = function(e) NULL)
  if (!is.null(p)) ggsave(file.path(plot_dir, paste0("02_tv_", nm, "_only.png")), p, width = 8, height = 5)
}

p_compare <- mrms_plot_compare(c(list(Static = fit_static), setNames(snaps, paste0("TV @ ", names(snaps)))),
                               plot_type = "response", layout = "facet", interval = "none")
ggsave(file.path(plot_dir, "03_compare_faceted.png"), p_compare, width = 10, height = 7)

p_evolution <- mrm_plot_tv(fit_tv, type = "evolution")
ggsave(file.path(plot_dir, "04_tv_evolution.png"), p_evolution, width = 11, height = 5.5)

p_trajectory <- mrm_plot_tv(fit_tv, type = "trajectory", param = "e")
ggsave(file.path(plot_dir, "05_tv_trajectory_e.png"), p_trajectory, width = 8, height = 4.5)

say("Done. Outputs in %s/", cache_dir)
