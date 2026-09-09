## =============================================================================
## Step 1 (v2): can time periods be treated as a hierarchy for pooled estimates?
## =============================================================================
## v2 corrections after reviewing v1 results:
##   - v1's channel filter excluded any channel with a zero-spend week. That
##     rule only applies to LOG-BASED curve forms (log_logistic, weibull,
##     reflected_weibull), which need x > 0. gompertz -- what this script
##     fits -- is defined at x = 0 (f(0) = c), so the filter was wrong and it
##     excluded impressions_video__linear: 808x robust spend ratio, the best-
##     identified channel in the dataset, for 4 zero-spend weeks out of 103.
##   - v1 plotted quarterly POINT ESTIMATES joined by straight line segments,
##     which is why the trajectory looked like disconnected line fragments.
##     v2 uses mrms_plot_compare() to overlay full per-quarter response
##     CURVES, which is what the package actually has for this.
## Channels here (by request): impressions_video__linear (Linear TV),
## impressions_video__streaming (Streaming), clicks_paid_search__brand
## (Paid Search).
##
## Tests whether fit_response_hier() -- which already ships -- produces sensible
## pooled per-period curve parameters when `group` is a TIME bucket rather than a
## sub-channel. If it does, a separate rolling-window function may only ever be
## worth building as a cheap un-pooled diagnostic.
##
## The comparison is the classic shrinkage triptych, per channel:
##   no pooling       -- fit_response() per quarter, independently
##   partial pooling  -- fit_response_hier(group = "quarter")   <- the thing tested
##   complete pooling -- fit_response() on the full series
##
## Run with `Rscript dev/hier_time_pooling.R`. Every fit is cached via brms
## `file=`, so an interrupted run loses nothing and reruns are instant.
## =============================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(tibble); library(ggplot2); library(mrmopt)
})

t0 <- Sys.time()
el <- function() as.numeric(Sys.time() - t0, units = "secs")
say <- function(...) cat(sprintf("[%5.0fs] ", el()), sprintf(...), "\n", sep = "")

CSV <- "/Users/roeh/decomp_2024-02-04_2026-01-18_2026-02-03 18_39_53.316291+00_00.csv"
cache_dir <- "data-raw/hier_time_cache"
plot_dir  <- file.path(cache_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

TYPE <- "gompertz"   # defined at x = 0 (f(0) = c); no need to avoid zero-spend weeks
CHANNELS <- c(
  impressions_video__linear    = "Linear TV",
  impressions_video__streaming = "Streaming",
  clicks_paid_search__brand    = "Paid Search (Brand)"
)

## -----------------------------------------------------------------------
## 1. Aggregate the decomposition to national weekly, per channel
## -----------------------------------------------------------------------

weekly_all <- read_csv(
  CSV,
  col_select = c(date, variable_type, variable, spend, clicks, impressions,
                 total_contrib_opps),
  show_col_types = FALSE, progress = FALSE
) |>
  filter(variable_type == "media") |>
  group_by(date, channel = variable) |>
  summarise(across(c(spend, clicks, impressions, total_contrib_opps), sum),
            .groups = "drop") |>
  rename(kpi = total_contrib_opps) |>
  mutate(quarter = paste0(format(date, "%Y"), "Q", quarters(date) |> substr(2, 2))) |>
  arrange(channel, date)

stopifnot(n_distinct(weekly_all$channel) == 9, !anyNA(weekly_all$spend))
say("Aggregated: %d weeks x %d channels.", n_distinct(weekly_all$date), n_distinct(weekly_all$channel))

## -----------------------------------------------------------------------
## 2. Per-channel triptych
## -----------------------------------------------------------------------

fit_channel <- function(ch, label) {

  say("=== %s (%s) ===", label, ch)
  dat <- weekly_all |> filter(channel == ch) |> mutate(quarter = factor(quarter)) |> as.data.frame()

  # units: gompertz tolerates zero SPEND weeks fine, but fit_response() hard-
  # errors on a ZERO in the units column itself. Linear TV has 3 zero-impression
  # weeks, so units is dropped for that channel only (loses cost-per-unit
  # reporting, nothing else).
  units_col <- if (grepl("^clicks_", ch) && all(dat$clicks > 0)) "clicks"
              else if (grepl("^impressions_", ch) && all(dat$impressions > 0)) "impressions"
              else NULL
  say("units column: %s | quarters: %d | weeks/quarter: %.1f | zero-spend weeks: %d",
      units_col %||% "none (dropped)", nlevels(dat$quarter),
      nrow(dat) / nlevels(dat$quarter), sum(dat$spend == 0))

  fdir <- file.path(cache_dir, "fits", ch)
  dir.create(fdir, showWarnings = FALSE, recursive = TRUE)
  fit_args <- list(spend = "spend", kpi = "kpi", date = "date", type = TYPE,
                   chains = 4, iter = 4000, warmup = 1000, refresh = 0)
  if (!is.null(units_col)) fit_args$units <- units_col

  fit_complete <- do.call(fit_response, c(
    list(data = dat, file = file.path(fdir, "complete")), fit_args))

  fits_indep <- map(levels(dat$quarter), function(q) {
    d_q <- dat |> filter(quarter == q)
    tryCatch(
      do.call(fit_response, c(list(data = d_q, file = file.path(fdir, paste0("indep_", q))), fit_args)),
      error = function(e) { say("  quarter %s FAILED: %s", q, conditionMessage(e)); NULL }
    )
  }) |> set_names(levels(dat$quarter))
  fits_indep <- fits_indep[!vapply(fits_indep, is.null, logical(1))]

  fit_hier_d <- do.call(fit_response_hier, c(
    list(data = dat, group = "quarter", pool = "d", file = file.path(fdir, "hier_d")),
    fit_args, list(control = list(adapt_delta = 0.95, max_treedepth = 12))))

  list(channel = ch, label = label, dat = dat, units_col = units_col,
       fit_complete = fit_complete, fits_indep = fits_indep, fit_hier_d = fit_hier_d)
}

results <- imap(CHANNELS, function(label, ch) fit_channel(ch, label))

## -----------------------------------------------------------------------
## 3. Numeric comparison: does the hierarchical estimate sit between?
## -----------------------------------------------------------------------

comparison <- imap_dfr(results, function(r, ch) {
  complete_d <- mrm_params(r$fit_complete) |> filter(param == "d") |> pull(center)
  indep_d <- imap_dfr(r$fits_indep, function(f, q)
    tibble(quarter = q, independent = mrm_params(f) |> filter(param == "d") |> pull(center)))
  hier_lv <- r$fit_hier_d$params_hier$levels[["quarter"]] |> select(quarter = id, hier_d = d)
  indep_d |> left_join(hier_lv, by = "quarter") |>
    mutate(channel = ch, label = r$label, complete = complete_d,
           shrink_frac = (independent - hier_d) / (independent - complete))
})
write_csv(comparison, file.path(cache_dir, "param_comparison_v2.csv"))

cat("\n--- Ceiling (d): independent vs hierarchical (pool='d') vs complete, by channel ---\n")
print(as.data.frame(comparison |> select(label, quarter, independent, hier_d, complete, shrink_frac)),
      digits = 4)

cat("\n--- Shrinkage summary by channel ---\n")
print(as.data.frame(comparison |> group_by(label) |> summarise(
  n = n(), median_shrink_frac = round(median(shrink_frac, na.rm = TRUE), 2),
  .groups = "drop")))

## -----------------------------------------------------------------------
## 4. Group SD + diagnostics
## -----------------------------------------------------------------------

group_sd <- imap_dfr(results, function(r, ch) {
  s <- posterior::summarise_draws(posterior::as_draws_df(r$fit_hier_d))
  s |> filter(grepl("^sd_quarter__", variable)) |>
    select(variable, median, q5, q95, rhat, ess_bulk) |> mutate(channel = ch, label = r$label)
})
write_csv(group_sd, file.path(cache_dir, "group_sd_v2.csv"))
cat("\n--- Group SD on the time level (scaled space) ---\n")
print(as.data.frame(group_sd |> select(label, everything(), -channel)), digits = 3)

diag_of <- function(f, label) {
  np <- brms::nuts_params(f)
  s  <- posterior::summarise_draws(posterior::as_draws_df(f))
  tibble(model = label, n_divergent = sum(np$Parameter == "divergent__" & np$Value > 0),
         max_rhat = max(s$rhat, na.rm = TRUE), min_ess_bulk = min(s$ess_bulk, na.rm = TRUE))
}
diagnostics <- imap_dfr(results, function(r, ch) bind_rows(
  diag_of(r$fit_complete, paste0(r$label, " / complete")),
  diag_of(r$fit_hier_d,   paste0(r$label, " / hier_d")),
  imap_dfr(r$fits_indep, ~ diag_of(.x, paste0(r$label, " / indep_", .y)))
))
write_csv(diagnostics, file.path(cache_dir, "diagnostics_v2.csv"))
cat("\n--- Sampler diagnostics ---\n")
print(as.data.frame(diagnostics |> filter(n_divergent > 0 | max_rhat > 1.01)), digits = 4)
cat(sprintf("(%d/%d fits clean: 0 divergences, max_rhat <= 1.01)\n",
            sum(diagnostics$n_divergent == 0 & diagnostics$max_rhat <= 1.01), nrow(diagnostics)))

## -----------------------------------------------------------------------
## 5. mrms_plot_compare(): actual per-quarter CURVES, not point estimates
## -----------------------------------------------------------------------

for (ch in names(results)) {
  r <- results[[ch]]

  # (a) Independent per-quarter curves, all overlaid -- the "spaghetti" view.
  p_indep <- mrms_plot_compare(r$fits_indep, plot_type = "response",
                               layout = "overlay", interval = "none")
  ggsave(file.path(plot_dir, paste0("indep_curves_", ch, ".png")), p_indep, width = 9, height = 6)

  # (b) Hierarchical per-quarter curves (as_mrmfit_list unit views) overlaid --
  # the pooled/shrunk counterpart to (a), same channel and quarters.
  units_list <- as_mrmfit_list(r$fit_hier_d) |> suppressWarnings()
  p_hier <- mrms_plot_compare(units_list, plot_type = "response",
                              layout = "overlay", interval = "none")
  ggsave(file.path(plot_dir, paste0("hier_curves_", ch, ".png")), p_hier, width = 9, height = 6)

  # (c) Independent vs hierarchical, faceted per quarter, side by side.
  paired <- list()
  for (q in names(r$fits_indep)) {
    if (q %in% names(units_list)) {
      paired[[paste0(q, " (independent)")]] <- r$fits_indep[[q]]
      paired[[paste0(q, " (pooled)")]] <- units_list[[q]]
    }
  }
  if (length(paired) >= 2) {
    p_pair <- mrms_plot_compare(paired, plot_type = "response",
                                layout = "facet", interval = "confidence")
    ggsave(file.path(plot_dir, paste0("paired_facet_", ch, ".png")), p_pair,
          width = 12, height = 9)
  }

  say("Plots written for %s.", r$label)
}

say("Done. Outputs in %s/", cache_dir)
