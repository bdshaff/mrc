## Investigate Paid Search (Brand)'s convergence failure on b_t/d_t/d_e:
## 0 divergences, 0% max-treedepth, but R-hat up to 1.53 and ESS down to 7 --
## the signature of chains stuck in different MODES, not a step-size problem.
## Hypothesis: Paid Search's spend only varies 1.9x (robust ratio) -- letting
## b or d drift on top of an already weakly-identified curve likely creates
## several equally-good solutions. Test: (a) does more warmup/iter fix it
## (a tuning problem would resolve; multimodality would not), (b) did chains
## actually land in different places for the population parameters.

suppressMessages({library(dplyr); library(readr); library(brms); library(mrmopt); library(purrr)})
t0 <- Sys.time()
say <- function(...) cat(sprintf("[%5.0fs] ", as.numeric(Sys.time()-t0, units="secs")), sprintf(...), "\n", sep="")

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
base_prior <- mrmopt:::hlpr_resolve_prior(mrm_prior = mrmopt_prior(), scaled_data = sc$scaled_data,
                                          x="spend", y="kpi", scale_method="min_max",
                                          scale_values = sv, type="gompertz")
gomp <- kpi ~ c + (d - c) * exp(-exp(b * (spend - e)))

## --- Step 1: does per-chain summary already show multimodality in the ORIGINAL fits? ---
orig_dir <- file.path("data-raw/tv_smooth_params_cache/fits", CH)
for (nm in c("b_t", "d_t")) {
  f <- readRDS(file.path(orig_dir, paste0(nm, ".rds")))
  draws <- posterior::as_draws_df(f)
  by_chain <- draws |> group_by(.chain) |>
    summarise(across(matches("^b_(b|c|d|e)_Intercept$"), median), .groups = "drop")
  cat(sprintf("\n--- %s: per-chain median of population Intercepts (multimodality check) ---\n", nm))
  print(as.data.frame(by_chain), digits = 4)
}

## --- Step 2: does more warmup/iter + higher adapt_delta fix it? ---
forms <- list(
  d_t = bf(gomp, d ~ s(t), b + c + e ~ 1, nl = TRUE),
  b_t = bf(gomp, b ~ s(t), c + d + e ~ 1, nl = TRUE),
  d_e = bf(gomp, d ~ s(t), e ~ s(t), b + c ~ 1, nl = TRUE)
)
fix_dir <- file.path("data-raw/tv_smooth_params_cache/fits_refit", CH)
dir.create(fix_dir, showWarnings = FALSE, recursive = TRUE)

refits <- map(set_names(names(forms)), function(nm) {
  say("refitting %s with adapt_delta=0.999, iter=6000, warmup=2000 ...", nm)
  brm(forms[[nm]], data = dat, prior = base_prior,
      chains = 4, iter = 6000, warmup = 2000, refresh = 0,
      cores = 4, backend = "cmdstanr", seed = 9001,
      control = list(adapt_delta = 0.999, max_treedepth = 14),
      file = file.path(fix_dir, nm))
})

diag <- imap_dfr(refits, function(f, nm) {
  np <- nuts_params(f); s <- posterior::summarise_draws(posterior::as_draws_df(f))
  tibble(model = nm, n_divergent = sum(np$Parameter=="divergent__" & np$Value>0),
         pct_max_td = round(100*mean(np$Parameter=="treedepth__" & np$Value>=14), 1),
         max_rhat = max(s$rhat, na.rm=TRUE), min_ess_bulk = min(s$ess_bulk, na.rm=TRUE),
         loo_elpd = tryCatch(loo(f)$estimates["elpd_loo","Estimate"], error=function(e) NA_real_))
})
cat("\n--- After refit (more warmup/iter, higher adapt_delta) ---\n")
print(as.data.frame(diag), digits = 4)

for (nm in names(refits)) {
  draws <- posterior::as_draws_df(refits[[nm]])
  by_chain <- draws |> group_by(.chain) |>
    summarise(across(matches("^b_(b|c|d|e)_Intercept$"), median), .groups = "drop")
  cat(sprintf("\n--- %s (refit): per-chain median of population Intercepts ---\n", nm))
  print(as.data.frame(by_chain), digits = 4)
}
