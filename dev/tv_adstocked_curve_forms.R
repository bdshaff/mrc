## Fit all available response curve forms on adstocked Linear TV spend,
## compare R2 and visually check which shows genuine saturation curvature
## within the OBSERVED adstocked-spend range (not extrapolated).

suppressMessages({library(dplyr); library(readr); library(ggplot2); library(purrr); library(tibble)})
devtools::load_all(quiet = TRUE)
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time()-t0, units="secs")), sprintf(...), "\n", sep="")

cache_dir <- "data-raw/tv_vs_static_cache_adstocked/curve_forms"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

weekly <- readRDS("/private/tmp/claude-503/-Users-roeh-Documents-roeh/5f6df883-8b06-4933-a577-4d0f482ab17c/scratchpad/linear_tv_weekly.rds") |> as.data.frame()
weekly <- weekly[order(weekly$date), ]
geo_adstock <- function(x, decay) { out <- numeric(length(x)); out[1] <- x[1]; for (i in 2:length(x)) out[i] <- x[i] + decay * out[i-1]; out }
weekly$spend_adstocked <- geo_adstock(weekly$spend, 0.75)

types <- c("logistic", "log_logistic", "gompertz", "reflected_gompertz", "weibull", "reflected_weibull")

fits <- list()
for (ty in types) {
  say("Fitting %s...", ty)
  fits[[ty]] <- tryCatch(
    fit_response(
      weekly, spend = "spend_adstocked", kpi = "kpi", date = "date", type = ty,
      chains = 4, iter = 4000, warmup = 1000, refresh = 0,
      file = file.path(cache_dir, ty)
    ) |> suppressMessages() |> suppressWarnings(),
    error = function(e) { cat("  FAILED:", conditionMessage(e), "\n"); NULL }
  )
}

say("All fits attempted.")

## --- R2 comparison ---
r2_tbl <- imap_dfr(fits, function(f, nm) {
  if (is.null(f)) return(tibble(type = nm, R2 = NA, R2_lower = NA, R2_upper = NA, b = NA))
  tibble(type = nm, R2 = f$R2$Estimate, R2_lower = f$R2$Q2.5, R2_upper = f$R2$Q97.5,
        b = f$params_summary$Estimate[f$params_summary$param == "b"])
})
write_csv(r2_tbl, file.path(cache_dir, "r2_comparison.csv"))
cat("\n--- R2 by curve form (adstocked spend) ---\n")
print(as.data.frame(r2_tbl), digits = 4)

## --- Overlay plot: all curve forms + observed range shading ---
obs_range <- range(weekly$spend_adstocked)
curves <- imap_dfr(fits, function(f, nm) {
  if (is.null(f)) return(NULL)
  rdf <- f$response_df
  tibble(spend = rdf[[names(rdf)[1]]], center = rdf$center, type = nm)
})

p <- ggplot(curves, aes(x = spend, y = center, color = type)) +
  geom_rect(data = NULL, aes(xmin = obs_range[1], xmax = obs_range[2], ymin = -Inf, ymax = Inf),
            inherit.aes = FALSE, fill = "grey85", alpha = 0.4) +
  geom_point(data = weekly, aes(x = spend_adstocked, y = kpi), inherit.aes = FALSE,
            color = "grey40", alpha = 0.4, size = 1.3) +
  geom_line(linewidth = 1) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Linear TV (adstocked spend): all curve forms",
       subtitle = "Grey band = observed adstocked-spend range",
       x = "Spend (adstocked)", y = "KPI", color = "Curve form") +
  theme_minimal(base_size = 13)

ggsave(file.path(cache_dir, "all_curve_forms.png"), p, width = 10, height = 6.5)

## zoomed to observed range only, to directly judge in-range curvature
p_zoom <- p + coord_cartesian(xlim = obs_range)
ggsave(file.path(cache_dir, "all_curve_forms_zoomed.png"), p_zoom, width = 10, height = 6.5)

say("Done. Outputs in %s/", cache_dir)
