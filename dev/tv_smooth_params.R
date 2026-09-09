## =============================================================================
## Step 3c: does the b/d/e s(t) ranking hold across channels? + curve-evolution
## visualization
## =============================================================================
## Step 3b found, on Streaming: e_t and d_t tie for best LOO, b_t helps less,
## and the joint d_e model buys no real predictive gain over the single-
## parameter winners while costing identifiability headroom (min ESS ~1500 vs
## ~2900-3000). This script repeats that 5-model battery (flat, d_t, b_t, e_t,
## d_e) on Linear TV and Paid Search (Brand) to see whether the ranking is
## channel-general or Streaming-specific, and adds a visualization of the
## fitted CURVE evolving over time (not just a single parameter's trajectory)
## for the best model on each channel.
##
## Only s(t) is used (the "less expensive" method from Step 3: 76s and 0
## divergences vs exact gp(t)'s 2914s and 25% max-treedepth).
##
## Run with `Rscript dev/tv_smooth_params.R`. All fits cached via brms `file=`.
## =============================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(tibble); library(ggplot2); library(brms); library(mrmopt)
})

t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time() - t0, units = "secs")),
                         sprintf(...), "\n", sep = "")

CSV  <- "/Users/roeh/decomp_2024-02-04_2026-01-18_2026-02-03 18_39_53.316291+00_00.csv"
TYPE <- "gompertz"
cache_dir <- "data-raw/tv_smooth_params_cache"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

CHANNELS <- c(
  impressions_video__linear    = "Linear TV",
  impressions_video__streaming = "Streaming",
  clicks_paid_search__brand    = "Paid Search (Brand)"
)

## -----------------------------------------------------------------------
## 1. Aggregate once, all media channels
## -----------------------------------------------------------------------

weekly_all <- read_csv(CSV, col_select = c(date, variable_type, variable, spend,
                                           total_contrib_opps),
                       show_col_types = FALSE, progress = FALSE) |>
  filter(variable_type == "media") |>
  group_by(date, channel = variable) |>
  summarise(spend = sum(spend), kpi = sum(total_contrib_opps), .groups = "drop") |>
  arrange(channel, date)

## -----------------------------------------------------------------------
## 2. Per-channel: fit the 5-model battery
## -----------------------------------------------------------------------

gomp <- kpi ~ c + (d - c) * exp(-exp(b * (spend - e)))
varying <- list(flat = character(0), d_t = "d", b_t = "b", e_t = "e", d_e = c("d", "e"))

run_channel <- function(ch, label) {
  say("=== %s (%s) ===", label, ch)
  weekly <- weekly_all |> filter(channel == ch) |> arrange(date)

  sc <- mrmopt:::hlpr_scale_data(as.data.frame(weekly[, c("spend", "kpi")]),
                                 x = "spend", y = "kpi", scale_method = "min_max", type = TYPE)
  sv <- sc$scale_values
  dat <- sc$scaled_data |>
    mutate(date = weekly$date,
           t = as.numeric(date - min(date)) / as.numeric(max(date) - min(date)))
  base_prior <- mrmopt:::hlpr_resolve_prior(
    mrm_prior = mrmopt_prior(), scaled_data = sc$scaled_data,
    x = "spend", y = "kpi", scale_method = "min_max", scale_values = sv, type = TYPE)

  forms <- list(
    flat = bf(gomp, b + c + d + e ~ 1,           nl = TRUE),
    d_t  = bf(gomp, d ~ s(t), b + c + e ~ 1,     nl = TRUE),
    b_t  = bf(gomp, b ~ s(t), c + d + e ~ 1,     nl = TRUE),
    e_t  = bf(gomp, e ~ s(t), b + c + d ~ 1,     nl = TRUE),
    d_e  = bf(gomp, d ~ s(t), e ~ s(t), b + c ~ 1, nl = TRUE)
  )

  fdir <- file.path(cache_dir, "fits", ch)
  dir.create(fdir, showWarnings = FALSE, recursive = TRUE)
  fits <- map(set_names(names(forms)), function(nm) {
    say("  fitting %s ...", nm)
    brm(forms[[nm]], data = dat, prior = base_prior,
        chains = 4, iter = 4000, warmup = 1000, refresh = 0,
        cores = 4, backend = "cmdstanr", seed = 4021,
        control = list(adapt_delta = 0.99, max_treedepth = 12),
        file = file.path(fdir, nm))
  })

  diagnostics <- imap_dfr(fits, function(f, nm) {
    np <- nuts_params(f); s <- posterior::summarise_draws(posterior::as_draws_df(f))
    tibble(channel = ch, label = label, model = nm,
           n_divergent  = sum(np$Parameter == "divergent__" & np$Value > 0),
           pct_max_td   = round(100 * mean(np$Parameter == "treedepth__" & np$Value >= 12), 1),
           max_rhat     = max(s$rhat, na.rm = TRUE),
           min_ess_bulk = min(s$ess_bulk, na.rm = TRUE),
           loo_elpd     = tryCatch(loo(f)$estimates["elpd_loo", "Estimate"], error = function(e) NA_real_))
  })

  list(channel = ch, label = label, weekly = weekly, sc = sc, sv = sv, dat = dat,
       fits = fits, diagnostics = diagnostics)
}

results <- imap(CHANNELS, function(label, ch) run_channel(ch, label))

## -----------------------------------------------------------------------
## 3. Cross-channel comparison: does the ranking hold?
## -----------------------------------------------------------------------

all_diag <- bind_rows(map(results, "diagnostics"))
write_csv(all_diag, file.path(cache_dir, "diagnostics_all_channels.csv"))

cat("\n--- LOO elpd by channel x model (higher = better out-of-sample fit) ---\n")
print(as.data.frame(all_diag |> select(label, model, loo_elpd) |>
                      pivot_wider(names_from = model, values_from = loo_elpd)), digits = 4)

cat("\n--- Rank of each model within its channel (1 = best) ---\n")
ranks <- all_diag |> group_by(label) |> mutate(rank = rank(-loo_elpd)) |>
  select(label, model, loo_elpd, rank) |> arrange(label, rank)
print(as.data.frame(ranks), digits = 4)

cat("\n--- Sampler health flags (any divergences, >1% max-treedepth, or rhat > 1.01) ---\n")
flags <- all_diag |> filter(n_divergent > 0 | pct_max_td > 1 | max_rhat > 1.01)
if (nrow(flags) == 0) cat("(none)\n") else print(as.data.frame(flags), digits = 4)

## -----------------------------------------------------------------------
## 4. Curve-evolution visualization for the best model per channel
## -----------------------------------------------------------------------
## Two complementary views of f(spend, t), combined via patchwork:
##   (a) a continuous heatmap/surface: spend x date, fill = predicted KPI --
##       shows the FULL evolving curve at a glance, no need to pick snapshots.
##   (b) discrete curve snapshots at N evenly-spaced dates, overlaid on shared
##       axes and colour-coded by date -- precisely readable shape comparisons
##       that the continuous heatmap can only suggest visually.
## Together these show both the continuous evolution and exact before/after
## shapes in one static figure, without needing an animation.

plot_dir <- file.path(cache_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

curve_evolution_plot <- function(r, model_nm, n_snapshots = 5, n_x = 80, n_t = 60) {
  f   <- r$fits[[model_nm]]
  sv  <- r$sv
  dat <- r$dat

  x_range_s <- range(dat$spend)   # scaled [0,1]-ish spend range actually fit
  grid <- expand_grid(
    spend = seq(x_range_s[1], x_range_s[2], length.out = n_x),
    t     = seq(0, 1, length.out = n_t)
  )
  grid$kpi <- 0
  grid$date <- min(r$weekly$date) + grid$t * as.numeric(max(r$weekly$date) - min(r$weekly$date))

  ep <- posterior_epred(f, newdata = grid, re_formula = NA)
  grid$center <- apply(ep, 2, median)

  # unscale to original units for both axes
  x_range <- sv$x_max - sv$x_min
  y_range <- sv$y_max - sv$y_min
  grid <- grid |> mutate(
    spend_orig = spend * x_range + sv$x_min,
    kpi_orig   = center * y_range + sv$y_min
  )

  p_heat <- ggplot(grid, aes(spend_orig, date, fill = kpi_orig)) +
    geom_raster(interpolate = TRUE) +
    geom_contour(aes(z = kpi_orig), colour = "white", alpha = 0.35, linewidth = 0.3) +
    scale_fill_viridis_c(labels = scales::comma, name = "Predicted\nKPI") +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_date(date_labels = "%b '%y") +
    labs(title = "Continuous evolution", x = "Weekly spend", y = NULL) +
    theme_minimal() + theme(legend.position = "right")

  snap_dates <- seq(min(r$weekly$date), max(r$weekly$date), length.out = n_snapshots)
  # snap each requested date to the nearest available grid t
  snaps <- map_dfr(snap_dates, function(sd) {
    tt <- as.numeric(sd - min(r$weekly$date)) / as.numeric(max(r$weekly$date) - min(r$weekly$date))
    nearest_t <- unique(grid$t)[which.min(abs(unique(grid$t) - tt))]
    grid |> filter(t == nearest_t) |> mutate(snapshot_date = sd)
  })

  p_snap <- ggplot(snaps, aes(spend_orig, kpi_orig, colour = snapshot_date, group = snapshot_date)) +
    geom_line(linewidth = 1) +
    scale_colour_viridis_c(trans = "date", labels = scales::date_format("%b '%y"), name = NULL) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste0(n_snapshots, " snapshots"), x = "Weekly spend", y = "Predicted KPI") +
    theme_minimal() + theme(legend.position = "right")

  patchwork::wrap_plots(p_heat, p_snap, nrow = 1, widths = c(1.1, 1)) +
    patchwork::plot_annotation(
      title = paste0(r$label, " — response curve evolution (", model_nm, ")"),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
}

for (ch in names(results)) {
  r <- results[[ch]]
  best <- r$diagnostics |> arrange(desc(loo_elpd)) |> slice(1) |> pull(model)
  say("Best model for %s by LOO: %s. Building curve-evolution plot...", r$label, best)
  p <- curve_evolution_plot(r, best)
  ggsave(file.path(plot_dir, paste0("curve_evolution_", ch, ".png")), p, width = 13, height = 5.5)
}

say("Done. Outputs in %s/", cache_dir)
