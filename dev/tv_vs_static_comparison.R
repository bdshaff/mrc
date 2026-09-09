## =============================================================================
## Utility test: fit_response() (static, full history) vs fit_response_tv()
## (time-varying) on real Linear TV data, same curve form.
## =============================================================================
## Curve form: logistic, not log_logistic. Probed both first -- log_logistic's
## fitted steepness here (|b| ~ 0.04) puts it in the "no interior MR peak"
## regime (mr declines monotonically from spend=0), so it cannot answer the
## "where does marginal return maximize" question at all. logistic finds a
## real interior peak and fit marginally better on this data.
##
## Compares: fit quality (Bayes R2), credible-band width (mean-function CI,
## curve-shape uncertainty only), and the saturation/peak-MR/peak-AR spend
## levels -- one number from the static fit vs. a trajectory of numbers from
## snapshots across the time-varying fit, to test whether the "optimal
## operating point" implied by the model actually moves over the observed
## history (the real test of whether this functionality adds insight beyond
## the static model, not just a different-looking plot).
##
## v2: monthly snapshots (one per calendar month in range, not just
## early/mid/late) plotted together on a single overlay with a continuous
## date color scale, since mrms_plot_compare()'s discrete per-model palette
## doesn't scale past a handful of series.
## =============================================================================

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(purrr); library(tibble)})
devtools::load_all(quiet = TRUE)
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time()-t0, units="secs")), sprintf(...), "\n", sep="")

cache_dir <- "data-raw/tv_vs_static_cache"
plot_dir  <- file.path(cache_dir, "plots")
for (d in c(cache_dir, plot_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

weekly <- readRDS("/private/tmp/claude-503/-Users-roeh-Documents-roeh/5f6df883-8b06-4933-a577-4d0f482ab17c/scratchpad/linear_tv_weekly.rds") |> as.data.frame()
say("Linear TV: %d weeks, %s to %s", nrow(weekly), format(min(weekly$date)), format(max(weekly$date)))

## -----------------------------------------------------------------------
## 1. Fit both models at REAL settings (not toy/probe settings) -- reuses
## cached fits from the prior run if present.
## -----------------------------------------------------------------------

say("Fitting static fit_response(type='logistic') on full history...")
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

cat("\n--- Time-varying convergence gates ---\n")
print(fit_tv$identifiability)
print(fit_tv$diagnostics)

## -----------------------------------------------------------------------
## 3. Monthly snapshots across the trajectory (one per calendar month)
## -----------------------------------------------------------------------

month_starts <- seq(
  from = lubridate::floor_date(fit_tv$date_range[1], "month"),
  to   = lubridate::floor_date(fit_tv$date_range[2], "month"),
  by   = "month"
)
# clip to the actual fitted range so we don't extrapolate past either end
snap_dates <- pmin(pmax(month_starts, fit_tv$date_range[1]), fit_tv$date_range[2])
names(snap_dates) <- format(snap_dates, "%Y-%m")
say("Building %d monthly snapshots (%s to %s)...", length(snap_dates),
    names(snap_dates)[1], names(snap_dates)[length(snap_dates)])

snaps <- imap(snap_dates, function(dt, nm) {
  mrm_tv_snapshot(fit_tv, date = as.Date(dt)) |> suppressWarnings()
})

## -----------------------------------------------------------------------
## 4. Credible-band width: mean-function CI (curve-shape uncertainty only),
## as a percentage of the curve's own height, evaluated at a common spend
## grid so static and each snapshot are directly comparable.
## -----------------------------------------------------------------------

common_grid <- seq(0, max(weekly$spend), length.out = 200)

band_width <- function(fit, label) {
  rdf <- fit$response_df
  x_col <- names(rdf)[1]
  approx_width <- approx(rdf[[x_col]], rdf$upper_mu - rdf$lower_mu, xout = common_grid, rule = 2)$y
  approx_center <- approx(rdf[[x_col]], rdf$center, xout = common_grid, rule = 2)$y
  tibble(model = label, spend = common_grid,
        ci_width = approx_width, center = approx_center,
        pct_of_range = 100 * approx_width / diff(range(rdf$center)))
}

band_static <- band_width(fit_static, "static")
band_snaps <- imap_dfr(snaps, ~ band_width(.x, paste0("tv_", .y)))
bands <- bind_rows(band_static, band_snaps)
write_csv(bands, file.path(cache_dir, "band_widths.csv"))

band_summary <- bands |> group_by(model) |>
  summarise(mean_ci_width = mean(ci_width), mean_pct_of_range = mean(pct_of_range), .groups = "drop")
cat("\n--- Mean credible-band width (curve-shape uncertainty), across the spend grid ---\n")
print(as.data.frame(band_summary), digits = 4)

## -----------------------------------------------------------------------
## 5. Saturation / peak-MR / peak-AR spend: one number (static) vs. a
## trajectory of numbers (time-varying snapshots)
## -----------------------------------------------------------------------

insight_row <- function(fit, label, date = NA) {
  s <- mrm_summary(fit)
  tibble(model = label, date = date,
        peak_mr_spend  = s$range_min_spend,
        peak_ar_spend  = s$range_peak_spend,
        decay_70_spend = s$range_max_spend,
        weekly_spend   = s$weekly_spend,
        r2             = fit$R2$Estimate)
}

insights <- bind_rows(
  insight_row(fit_static, "static (full history)"),
  imap_dfr(snaps, ~ insight_row(.x, paste0("tv: ", .y), .x$snapshot_date))
)
write_csv(insights, file.path(cache_dir, "insights_monthly.csv"))
cat("\n--- Saturation / peak-MR / peak-AR spend: static vs. monthly TV snapshots ---\n")
print(as.data.frame(insights), digits = 4)

## -----------------------------------------------------------------------
## 6. Visual comparison: static curve vs. ALL monthly TV snapshots, on a
## single overlay plot with a continuous date color scale.
## -----------------------------------------------------------------------

snap_curves <- imap_dfr(snaps, function(s, nm) {
  rdf <- s$response_df
  tibble(spend = rdf[[names(rdf)[1]]], center = rdf$center, date = s$snapshot_date, label = nm)
})

static_curve <- {
  rdf <- fit_static$response_df
  tibble(spend = rdf[[names(rdf)[1]]], center = rdf$center)
}

p_monthly <- ggplot(snap_curves, aes(x = spend, y = center, group = label, color = date)) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_line(data = static_curve, aes(x = spend, y = center), inherit.aes = FALSE,
            color = "black", linewidth = 1.1, linetype = "dashed") +
  scale_color_viridis_c(trans = "date", name = "Snapshot month") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Linear TV: static curve vs. every monthly TV snapshot",
       subtitle = "Dashed black = static fit_response() on full history. Colored lines = one fit_response_tv() snapshot per calendar month.",
       x = "Spend", y = "KPI") +
  theme_minimal(base_size = 13)

ggsave(file.path(plot_dir, "01_static_vs_all_monthly_snapshots.png"), p_monthly, width = 10, height = 6.5)

p_evolution <- mrm_plot_tv(fit_tv, type = "evolution")
ggsave(file.path(plot_dir, "02_tv_evolution.png"), p_evolution, width = 11, height = 5.5)

p_trajectory <- mrm_plot_tv(fit_tv, type = "trajectory", param = "e")
ggsave(file.path(plot_dir, "03_tv_trajectory_e.png"), p_trajectory, width = 8, height = 4.5)

say("Done. Outputs in %s/", cache_dir)
