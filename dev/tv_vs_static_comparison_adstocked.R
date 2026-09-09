## =============================================================================
## v3: same static vs. time-varying comparison, but on ADSTOCKED spend
## (geometric decay = 0.75, grid-searched earlier in this session to maximize
## raw correlation with KPI: r 0.61 -> 0.92). Motivation: v1/v2 on raw spend
## found both models nearly linear across the observed range -- either because
## e sits outside observed spend (TV trajectory) or because b (growth rate) is
## ~0 (static). If raw spend is a noisy proxy for the carryover-adjusted spend
## that actually drives KPI, the saturation curve fit on RAW spend may simply
## never resolve -- adstocking first is the correct fix, not a curve-form change.
## =============================================================================

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(purrr); library(tibble)})
devtools::load_all(quiet = TRUE)
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time()-t0, units="secs")), sprintf(...), "\n", sep="")

cache_dir <- "data-raw/tv_vs_static_cache_adstocked"
plot_dir  <- file.path(cache_dir, "plots")
for (d in c(cache_dir, plot_dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

weekly <- readRDS("/private/tmp/claude-503/-Users-roeh-Documents-roeh/5f6df883-8b06-4933-a577-4d0f482ab17c/scratchpad/linear_tv_weekly.rds") |> as.data.frame()
weekly <- weekly[order(weekly$date), ]

geo_adstock <- function(x, decay) {
  out <- numeric(length(x))
  out[1] <- x[1]
  for (i in 2:length(x)) out[i] <- x[i] + decay * out[i-1]
  out
}
decay_r <- 0.75
weekly$spend_adstocked <- geo_adstock(weekly$spend, decay_r)
say("Linear TV: %d weeks. decay=%.2f | cor(raw,kpi)=%.3f | cor(adstocked,kpi)=%.3f",
    nrow(weekly), decay_r, cor(weekly$spend, weekly$kpi), cor(weekly$spend_adstocked, weekly$kpi))
cat("adstocked spend range:", range(weekly$spend_adstocked), " (raw range:", range(weekly$spend), ")\n")

## -----------------------------------------------------------------------
## 1. Fit both models on ADSTOCKED spend
## -----------------------------------------------------------------------

say("Fitting static fit_response(type='logistic') on adstocked spend...")
fit_static <- fit_response(
  weekly, spend = "spend_adstocked", kpi = "kpi", date = "date", type = "logistic",
  chains = 4, iter = 4000, warmup = 1000, refresh = 0,
  file = file.path(cache_dir, "static_logistic")
)

say("Fitting fit_response_tv(type='logistic', varying='e') on adstocked spend...")
fit_tv <- fit_response_tv(
  weekly, spend = "spend_adstocked", kpi = "kpi", date = "date", type = "logistic",
  varying = "e", method = "spline",
  chains = 4, iter = 4000, warmup = 1000, refresh = 0,
  file = file.path(cache_dir, "tv_logistic")
)

say("Both fits done.")
print(fit_static)
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

cat("\n--- Static params ---\n")
print(mrm_summary(fit_static))

cat("\n--- Time-varying convergence gates ---\n")
print(fit_tv$identifiability)
print(fit_tv$diagnostics)

## -----------------------------------------------------------------------
## 3. Monthly snapshots
## -----------------------------------------------------------------------

month_starts <- seq(
  from = lubridate::floor_date(fit_tv$date_range[1], "month"),
  to   = lubridate::floor_date(fit_tv$date_range[2], "month"),
  by   = "month"
)
snap_dates <- pmin(pmax(month_starts, fit_tv$date_range[1]), fit_tv$date_range[2])
names(snap_dates) <- format(snap_dates, "%Y-%m")
say("Building %d monthly snapshots...", length(snap_dates))

snaps <- imap(snap_dates, function(dt, nm) {
  mrm_tv_snapshot(fit_tv, date = as.Date(dt)) |> suppressWarnings()
})

## -----------------------------------------------------------------------
## 4. Visual: static curve vs. all monthly TV snapshots, single overlay,
## with observed adstocked-spend range marked.
## -----------------------------------------------------------------------

snap_curves <- imap_dfr(snaps, function(s, nm) {
  rdf <- s$response_df
  tibble(spend = rdf[[names(rdf)[1]]], center = rdf$center, date = s$snapshot_date, label = nm)
})
static_curve <- {
  rdf <- fit_static$response_df
  tibble(spend = rdf[[names(rdf)[1]]], center = rdf$center)
}
obs_range <- range(weekly$spend_adstocked)

p_monthly <- ggplot(snap_curves, aes(x = spend, y = center, group = label, color = date)) +
  geom_rect(aes(xmin = obs_range[1], xmax = obs_range[2], ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.3) +
  geom_line(linewidth = 0.6, alpha = 0.85) +
  geom_line(data = static_curve, aes(x = spend, y = center), inherit.aes = FALSE,
            color = "black", linewidth = 1.1, linetype = "dashed") +
  scale_color_viridis_c(trans = "date", name = "Snapshot month") +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Linear TV (adstocked spend): static curve vs. every monthly TV snapshot",
       subtitle = paste0("Geometric adstock decay=", decay_r, ". Grey band = observed adstocked-spend range. Dashed black = static fit."),
       x = "Spend (adstocked)", y = "KPI") +
  theme_minimal(base_size = 13)

ggsave(file.path(plot_dir, "01_static_vs_all_monthly_snapshots_adstocked.png"), p_monthly, width = 10, height = 6.5)

p_evolution <- mrm_plot_tv(fit_tv, type = "evolution")
ggsave(file.path(plot_dir, "02_tv_evolution.png"), p_evolution, width = 11, height = 5.5)

say("Done. Outputs in %s/", cache_dir)
