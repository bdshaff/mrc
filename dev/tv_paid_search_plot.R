## Rebuild Paid Search (Brand)'s curve-evolution plot using e_t -- the only
## model that converged cleanly (see dev/tv_paid_search_fix.R). d_e, which the
## automatic "pick by LOO" logic in tv_smooth_params.R selected, has confirmed
## multimodality (R-hat got WORSE after 6x the sampling) and its elpd of 149.5
## cannot be trusted.

suppressMessages({library(dplyr); library(tidyr); library(readr); library(brms); library(mrmopt); library(ggplot2); library(purrr); library(tibble)})
CSV <- "/Users/roeh/decomp_2024-02-04_2026-01-18_2026-02-03 18_39_53.316291+00_00.csv"
CH  <- "clicks_paid_search__brand"

weekly <- read_csv(CSV, col_select = c(date, variable_type, variable, spend, total_contrib_opps),
                   show_col_types = FALSE, progress = FALSE) |>
  filter(variable_type == "media", variable == CH) |>
  group_by(date) |> summarise(spend = sum(spend), kpi = sum(total_contrib_opps), .groups = "drop") |>
  arrange(date)

sc <- mrmopt:::hlpr_scale_data(as.data.frame(weekly[, c("spend","kpi")]), x="spend", y="kpi",
                               scale_method="min_max", type="gompertz")
sv <- sc$scale_values
dat <- sc$scaled_data |> mutate(date = weekly$date,
       t = as.numeric(date - min(date)) / as.numeric(max(date) - min(date)))

f <- readRDS(file.path("data-raw/tv_smooth_params_cache/fits", CH, "e_t.rds"))
r <- list(weekly = weekly, sc = sc, sv = sv, dat = dat, label = "Paid Search (Brand)")

x_range_s <- range(dat$spend)
grid <- expand_grid(spend = seq(x_range_s[1], x_range_s[2], length.out = 80),
                    t = seq(0, 1, length.out = 60))
grid$kpi <- 0
grid$date <- min(r$weekly$date) + grid$t * as.numeric(max(r$weekly$date) - min(r$weekly$date))
ep <- posterior_epred(f, newdata = grid, re_formula = NA)
grid$center <- apply(ep, 2, median)
x_range <- sv$x_max - sv$x_min; y_range <- sv$y_max - sv$y_min
grid <- grid |> mutate(spend_orig = spend * x_range + sv$x_min, kpi_orig = center * y_range + sv$y_min)

p_heat <- ggplot(grid, aes(spend_orig, date, fill = kpi_orig)) +
  geom_raster(interpolate = TRUE) +
  geom_contour(aes(z = kpi_orig), colour = "white", alpha = 0.35, linewidth = 0.3) +
  scale_fill_viridis_c(labels = scales::comma, name = "Predicted\nKPI") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_date(date_labels = "%b '%y") +
  labs(title = "Continuous evolution", x = "Weekly spend", y = NULL) +
  theme_minimal() + theme(legend.position = "right")

n_snapshots <- 5
snap_dates <- seq(min(r$weekly$date), max(r$weekly$date), length.out = n_snapshots)
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

p <- patchwork::wrap_plots(p_heat, p_snap, nrow = 1, widths = c(1.1, 1)) +
  patchwork::plot_annotation(
    title = "Paid Search (Brand) — response curve evolution (e_t, the only converged model)",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13)))

ggsave("data-raw/tv_smooth_params_cache/plots/curve_evolution_clicks_paid_search__brand.png",
      p, width = 13, height = 5.5)
cat("done\n")
