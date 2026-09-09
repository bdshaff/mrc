# mrmopt Package — Project Memory

## Overview

**mrmopt** (Media Response Modeling and Optimization) is an R package by Ben Denis Shaffer for Bayesian nonlinear media response modeling. It fits saturation/diminishing-returns curves to media spend vs. KPI data using `brms` (Stan backend), quantifies uncertainty via posterior distributions, and supports media mix optimization via `nloptr`.

- **Version**: 0.1.0
- **License**: MIT
- **Docs**: https://roeh-marketing.github.io/mrmopt/

---

## Recent Build Fixes (June 2026)

- **Missing plot.opt_mix_result S3 method**: Created `R/plot.opt_mix_result.R` — dispatches `plot(x, type = ...)` to appropriate `opt_plot_*` functions (allocation, kpi, comparison, posterior, curves, returns). Added `S3method(plot,opt_mix_result)` to NAMESPACE. Fixed vignette error in `hierarchical_curves.Rmd:218` where `plot(opt_post, type = "posterior")` was falling through to `plot.default()`.
- **Vignette build performance**: Moved 4 slow vignettes (fitting_and_analysis, diagnostics_and_comparison, hierarchical_curves, optimization) to separate build process via `.Rbuildignore`. These require full MCMC sampling and slow down `R CMD build` / `devtools::check()`. Created `data-raw/build_slow_vignettes.R` script to build them separately for pkgdown site. See `VIGNETTE_BUILD.md` for workflow.
- **Rd cross-references**: `\link{mrm_prior}` → `\link{mrmopt_prior}` in `R/fit_response.R` and `R/hlpr_resolve_prior.R` (the function is named `mrmopt_prior`, not `mrm_prior`)
- **`.Rbuildignore`**: Added `^ROADMAP\.md$` and `^conversation-export\.html$` to suppress non-standard top-level file NOTE
- **Undefined globals**: Added `@importFrom stats fitted` to `R/mrm_infer.R`; added `@importFrom dplyr bind_rows mutate select` and `@importFrom tibble tibble` to `R/opt_generate_constraints.R`; replaced `%>%` with `|>` in `opt_generate_constraints.R`
- **Vignette error (`getting_started.Rmd`)**: `mrm_plot_diagnostics()` used `trace_plot / pp_plot` where `trace_plot` is a `bayesplot_grid` S7 object — S7 intercepts the `/` operator before patchwork can. Fixed by wrapping: `patchwork::wrap_elements(trace_plot) / pp_plot`
- **`forcats` undeclared**: Added `forcats` to `Suggests` in DESCRIPTION (used via `forcats::fct_reorder()` in `vignettes/optimization.Rmd`)
- **pkgdown build**: `getting_started` vignette was missing from `_pkgdown.yml` articles index — added as first entry under "Getting Started"
- **Log-scale MR peak fix**: The vignette and code incorrectly claimed log-scale curves (log_logistic, weibull, reflected_weibull) always have monotonically decreasing marginal return. In fact, dy/dx = dy/d(log x) × 1/x, so an interior MR peak exists when |b| > 1 (common in practice). Fixed: (1) rewrote Marginal Return section in `response_curve_theory.Rmd` with correct characterization and new figure showing MR vs |b|; (2) `mrm_summary.R` and `mrm_summary_hier.R` now detect interior MR peaks from data (`which.max(mr)`) rather than assuming all log-form curves lack one; (3) added tests in `test-hlpr_summary_core.R` for both |b| > 1 (has peak) and |b| ≤ 1 (no peak) cases
- **Vignette build strategy**: Stan compilation during vignette rendering causes rstan/rmarkdown `sink()` incompatibility, making it impossible to build vignettes with full code execution in standard `R CMD build`. Solution: (1) slow vignettes (getting_started, fitting_and_analysis, diagnostics_and_comparison, hierarchical_curves, optimization) are in `.Rbuildignore` so `R CMD build` and `devtools::check()` skip them; (2) `data-raw/build_slow_vignettes.R` manually renders them to `docs/articles/` before site deployment; (3) pkgdown uses the pre-built HTML instead of rebuilding. Result: package builds fast (~30s), vignettes render fully when needed (~5-10 min), and docs site always has current content. See `VIGNETTE_BUILD.md`.

---

## Package Structure

```
mrmopt/
├── R/                    # 42+ source files
├── tests/testthat/       # 25 test files (testthat, 403+ tests)
├── vignettes/            # 3 tutorials
├── man/                  # Roxygen-generated docs
├── docs/                 # pkgdown site
├── DESCRIPTION
└── NAMESPACE
```

---

## Naming Conventions

| Prefix | Meaning |
|--------|---------|
| `mrm_*` | User-facing analysis/plotting functions |
| `fit_*` | Model fitting entry points |
| `opt_*` | Optimization functions |
| `rm_*` | Response model curve functions (S-curves) |
| `hlpr_*` | Internal helper functions |

---

## Response Curve Types (6 total)

All use 4 parameters: **b** (steepness), **c** (floor), **d** (ceiling), **e** (midpoint/inflection).

| Type | Form | Notes |
|------|------|-------|
| `logistic` | `c + (d-c)/(1 + exp(b*(x-e)))` | Standard |
| `gompertz` | `c + (d-c)*exp(-exp(b*(x-e)))` | Standard |
| `reflected_gompertz` | Reflected S-curve | Standard |
| `weibull` | Log-based | Requires x > 0 |
| `log_logistic` | Log-based | Requires x > 0 |
| `reflected_weibull` | Reflected log-based | Requires x > 0 |

Log-based forms (`weibull`, `log_logistic`, `reflected_weibull`) use ratio scaling (x/max) and require special handling if zeros are present in spend data.

---

## Core S3 Objects

### `mrmfit` (extends `brmsfit`)
Returned by `fit_response()`. Key fields:
- `$response_df` — cached inference results (unscaled)
- `$summary` — `mrm_summary` object
- `$params_summary` — parameter summaries
- `$scale_values` — scaling metadata for unscaling
- `$rc_type` — response curve type string
- `$cost_per_unit` — cost-per-unit if `units` supplied
- `$date_range` — `c(min_date, max_date)` from input data

### `mrmfit_tv` (extends `brmsfit`; does NOT inherit `mrmfit`)
Returned by `fit_response_tv()`. One or more of `b`/`c`/`d`/`e` are modeled as
`s(t)`/`gp(t)` functions of time instead of constants — see `## Time-Varying
Response Curves` below for the evidence behind the default `varying = "e"`
and `method = "spline"`. Does not inherit `mrmfit`: single-curve `mrm_*`
methods assume static parameters (same reasoning as `mrmfit_hier`). Key
fields: `$varying`, `$method`, `$k`, `$trajectory` (long tibble, one row per
time-grid point × varying parameter, from `hlpr_params_tv()`),
`$identifiability` (pre-fit heuristic result), `$diagnostics` (post-fit
convergence gate — **authoritative**; the pre-fit heuristic is advisory
only), `$scale_values`, `$rc_type`, `$date_range`. Use `mrm_tv_snapshot()` to
get a static curve at one date for `opt_mix()` — a `mrmfit_tv` cannot itself
be optimized (its objective is time, not a point).

### `mrmfit_tv_snapshot` (extends `mrmfit`)
Returned by `mrm_tv_snapshot(fit_response_tv_object, at = )`. Unlike
`as_mrmfit_list()`'s per-unit views (constructed from a hierarchy that
already exists), this is built by evaluating `posterior_linpred()` at one
target date for every varying parameter and reading ordinary population
draws for frozen ones — so it deliberately **does** inherit `mrmfit` (a
snapshot at a fixed date is a genuine static curve). Reuses
`hlpr_params()`'s existing `params_hier_unit` shortcut (same field name
`as_mrmfit_list()` uses — deliberate reuse, not a naming accident) and a
synthetic `.snapshot_draws` object with a registered
`as_draws_df.mrmfit_tv_snapshot` method, mirroring `as_mrmfit_list()`'s
`.unit_draws` mechanism so `opt_mix()`'s posterior-based path works
identically. `response_df`/`summary` are built via a new
`hlpr_infer_tv_snapshot()` (a `t`-aware sibling of `mrm_infer()`, whose
`ar`/`mr`/`cp` arithmetic it copies verbatim rather than re-derives) — this
requires a small addition to `mrm_infer()`'s existing per-unit-view
short-circuit so it also recognizes `mrmfit_tv_snapshot` objects, otherwise
`mrm_summary()`'s internal `mrm_infer()` call would try `predict()`/`fitted()`
on an object with no live posterior sampler.

### `mrmopt_prior`
List-based prior specification. Created with `mrmopt_prior()`.

### `mrm_summary`
Tibble with attributes for formatted printing.

---

## Main Workflow

```r
# 1. Fit
fit <- fit_response(
  data    = my_data,
  spend   = "ad_spend",
  kpi     = "conversions",
  date    = "week",
  units   = "impressions",   # optional
  type    = "gompertz",
  midpoint_range = c(0.1, 0.5),
  ceiling_max    = 3
)

# 2. Inspect
print(fit)
plot(fit)            # dashboard: response + AR/MR + cost-per
plot(fit, type = "diagnostics")  # trace plots + PPCs

# 3. Analyze
mrm_params(fit)
mrm_summary(fit)


# 4. Compare models
mrm_plot_compare(list(gompertz = fit_g, logistic = fit_l), layout = "overlay")

# 5. Optimize (point estimate — fast, single solution)
opt <- opt_mix(list(ch1 = fit1, ch2 = fit2), budget = 500000)
print(opt)           # formatted console summary (calls summary())
summary(opt)         # same formatted output
opt_table(opt)       # tidy tibble with per-channel deltas (current vs optimal)

# 5b. Optimize (posterior — distribution of solutions)
opt_post <- opt_mix(list(ch1 = fit1, ch2 = fit2),
                    method = "posterior", budget = 500000, n_draws = 200)
plot(opt_post, type = "posterior")

# 5c. Period budgets (e.g., $10M annual)
opt_annual <- opt_mix(models, budget = 10000000, n_weeks = 52)

# 5d. Flexible budget: target ROI (budget is an output)
opt_roi <- opt_mix(models, objective = "target_roi", target_roi = 2.5)
opt_roi$achieved_roi       # actual ROI at solution
opt_roi$budget_info$weekly_budget  # discovered budget

# 5e. Flexible budget: target mROI (per-channel marginal return threshold)
opt_mroi <- opt_mix(models, objective = "target_mroi", target_mroi = 0.5)
opt_mroi$channel_mroi      # per-channel mROI at solution
opt_mroi$budget_info$weekly_budget  # sum of per-channel solutions

# 5f. Cost-per-KPI framing (for non-revenue KPIs like leads/visits)
# Same solver as target_roi, but input/output framed as cost-per
opt_cpk <- opt_mix(models, objective = "target_cpk", target_cpk = 50)
opt_cpk$achieved_cpk       # actual CPK at solution (e.g. $48.50/lead)

# 5g. Marginal cost-per-KPI (stop spending when next lead costs > $100)
opt_mcpk <- opt_mix(models, objective = "target_mcpk", target_mcpk = 100)
opt_mcpk$channel_mcpk      # per-channel marginal cost-per at solution

# 6. Visualize on response curves
plot(opt, type = "curves")     # response curves with current + optimal points
plot(opt, type = "returns")    # AR/MR curves with current + optimal points

# 7. Compare two optimization results
comp <- compare(opt, opt_post)
plot(comp, type = "spend")     # dumbbell chart
summary(opt)                   # tidy comparison tibble with deltas
```

---

## Optimization Architecture

### `opt_mix()` — Main Entry Point

Single function with two methods and three objectives:

**Methods:**
- **`method = "point"`** (default): Uses posterior median parameters → single nloptr solve (~1s)
- **`method = "posterior"`**: Optimizes over N posterior draws → distribution of solutions (~6s for 200 draws × 9 channels)

**Objectives:**
- **`objective = "max_kpi"`** (default): Maximize total KPI under a fixed budget equality constraint `sum(x) == budget`.
- **`objective = "target_roi"`**: Maximize incremental KPI subject to portfolio ROI ≥ `target_roi`. Budget is free (output, not input). ROI = Σ[f(x)-f(0)] / Σx. Uses nloptr COBYLA with an inequality constraint.
- **`objective = "target_mroi"`**: Per-channel root-finding — set each channel's spend to where dy/dx = `target_mroi`. No multi-channel optimizer needed; uses grid search + `uniroot()` per channel. Budget is the sum of per-channel solutions.
- **`objective = "target_cpk"`**: Cost-per-KPI wrapper around `target_roi`. Same solver, but accepts `target_cpk` (spend/KPI) instead of `target_roi` (KPI/spend). Internally converts: `target_roi = 1 / target_cpk`. Output reports CPK instead of ROI. Useful for non-revenue KPIs (leads, visits, etc.).
- **`objective = "target_mcpk"`**: Marginal-cost-per-KPI wrapper around `target_mroi`. Accepts `target_mcpk` (marginal spend per KPI unit) instead of `target_mroi` (marginal KPI per dollar). Internally converts: `target_mroi = 1 / target_mcpk`.

### Internal Architecture

```
opt_mix()
├─ hlpr_auto_constraints() / hlpr_parse_constraints()  # constraint setup
│
├─ objective = "max_kpi"
│  ├─ opt_mix_point()          # point-estimate path
│  │  ├─ mrm_response_function()  # extract median curve
│  │  └─ hlpr_opt_solve()         # nloptr COBYLA (sum(x)==budget)
│  └─ opt_mix_posterior()      # posterior path
│     ├─ hlpr_extract_draws()     # pre-extract & unscale all draws
│     ├─ make_draw_objective()    # build objective from raw draws + rm_dispatch
│     └─ hlpr_opt_solve() × N    # one solve per draw
│
├─ objective = "target_roi"
│  ├─ opt_mix_target_roi_point()
│  │  ├─ hlpr_baseline_kpi()      # f(0) per channel for ROI constraint
│  │  └─ hlpr_opt_solve()         # nloptr COBYLA (ROI inequality, no budget eq)
│  └─ opt_mix_target_roi_posterior()
│     ├─ hlpr_extract_draws() + hlpr_baseline_kpi_vec()
│     └─ hlpr_opt_solve() × N
│
└─ objective = "target_mroi"
   ├─ opt_mix_target_mroi_point()
   │  └─ hlpr_find_mroi_spend() per channel  # grid + uniroot
   └─ opt_mix_target_mroi_posterior()
      └─ hlpr_find_mroi_spend() per channel per draw
```

The posterior path uses raw parameter draws + `rm_dispatch()` instead of `brms::posterior_epred()` — a 10,000x speedup that makes posterior optimization practical.

### Key Files

| File | Purpose |
|------|---------|
| `R/opt_mix.R` | Main function + constraint helpers (`hlpr_auto_constraints`, `hlpr_parse_constraints`) |
| `R/opt_mix_target_roi.R` | Target ROI point + posterior solvers |
| `R/opt_mix_target_mroi.R` | Target mROI point + posterior solvers |
| `R/hlpr_opt_solve.R` | Thin nloptr wrapper (shared core) |
| `R/hlpr_extract_draws.R` | Pre-extracts & unscales all posterior draws for fast evaluation |
| `R/hlpr_baseline_kpi.R` | Compute f(0) per channel (scalar + vectorised) for ROI constraint |
| `R/hlpr_numerical_mr.R` | Numerical marginal return + `hlpr_find_mroi_spend()` root-finder |
| `R/hlpr_build_solution.R` | Builds the unified solution tibble (current + optimal metrics) |
| `R/print.opt_mix_result.R` | Formatted console output |
| `R/plot.opt_mix_result.R` | 6 plot types: allocation, kpi, comparison, posterior, curves, returns |
| `R/opt_summary.R` | Formatted console summary (objective-aware) |
| `R/opt_table.R` | Tidy comparison tibble with deltas |
| `R/compare.opt_mix_result.R` | `compare()` generic + method: side-by-side diff of two results |
| `R/plot.opt_mix_compare.R` | Dumbbell plot for compare results (spend + kpi) |
| `R/hlpr_opt_metrics.R` | Interpolates KPI/AR/MR/CP at arbitrary spend from response_df |

### Return Structure (`opt_mix_result` S3 class)

All objectives return the same top-level structure:
- `$solution` — unified tibble (same columns for point and posterior)
- `$constraints` — tibble: channel, lb, ub, x0
- `$budget_info` — list: total_budget, weekly_budget, n_weeks, current_weekly.
  For flexible-budget objectives, `weekly_budget` and `total_budget` are computed from the optimal solution.
- `$method` — `"point"` or `"posterior"`
- `$objective` — `"max_kpi"`, `"target_roi"`, or `"target_mroi"`
- `$mrms` — the named list of `mrmfit` models (used by `curves` and `returns` plots)
- `$draws_matrix` / `$kpi_matrix` / `$solution_draws` / `$n_draws` / `$draw_ids` — posterior-only (NULL for point)
- `$nloptr_result` / `$response_funs` — point-only (NULL for posterior)
- `$target_roi` / `$achieved_roi` — target_roi objective only (NULL otherwise)
- `$target_mroi` / `$channel_mroi` — target_mroi objective only (NULL otherwise)
- `$target_cpk` / `$achieved_cpk` — target_cpk objective only (reciprocals of ROI values; NULL otherwise)
- `$target_mcpk` / `$channel_mcpk` — target_mcpk objective only (reciprocals of mROI values; NULL otherwise)

### Solution Tibble Columns

The `$solution` tibble contains:
- **Current state**: `current_weekly_spend`, `current_weekly_units`, `current_weekly_kpi`, `current_cost_per`, `current_rr`, `current_spend_share`, `current_kpi_share`
- **Optimal state**: `weekly_spend`, `weekly_kpi`, `weekly_units`, `cost_per`, `rr` (each with `_lower`/`_upper` CI columns — NA for point)
- **Period totals**: `period_spend`, `period_kpi`, `period_units`
- **Shares**: `spend_share`, `kpi_share`

Units assume static cost-per-unit (`mrm$cost_per_unit`). NA when model fit without `units`.

### Constraint Specification

**Auto-generated** (default): Derives bounds from model return rate ranges × `bounds_multiplier`.

**User-supplied** via `constraints` data frame:

| Column | Required? | Description |
|--------|-----------|-------------|
| `channel` | Yes | Must match model names |
| `min_spend` | Yes | Absolute lower bound (weekly $) |
| `max_spend` | Yes | Absolute upper bound (weekly $) |
| `min_share` | No | Minimum share of budget [0, 1] |
| `max_share` | No | Maximum share of budget [0, 1] |
| `fixed` | No | Lock spend at `min_spend` (logical) |

When both absolute and share bounds are present, the tighter constraint wins.

**Note:** Share-based constraints (`min_share`/`max_share`) are only supported for `objective = "max_kpi"` (fixed budget). They are warned about and stripped for flexible-budget objectives (`target_roi`, `target_mroi`) since there is no fixed budget to compute shares against.

### Plot Types (`plot.opt_mix_result`)

| Type | Description |
|------|-------------|
| `"allocation"` | Grouped bar: current vs optimal spend (default). Posterior adds CI error bars. |
| `"kpi"` | Grouped bar: current vs optimal KPI |
| `"comparison"` | Dumbbell chart: current → optimal spend per channel |
| `"posterior"` | Violin + boxplot of spend distributions (posterior only) |
| `"curves"` | Faceted response curves with current (red) + optimal (blue) points per channel |
| `"returns"` | Faceted AR/MR curves with current + optimal points — shows marginal and average return at both positions |

### `compare()` — Side-by-Side Diff

`compare(a, b, labels)` takes two `opt_mix_result` objects and returns an `opt_mix_compare` tibble with per-channel spend/KPI/CP values from each result, deltas, and a TOTAL row. Column names are dynamically generated from `labels` (defaults to method names when comparing point vs posterior).

`plot(comp, type = "spend")` produces a dumbbell chart with dots for each result and a faint current-spend reference. Also supports `type = "kpi"`.

---

## Time-Varying Response Curves

`fit_response_tv()` fits `b`/`c`/`d`/`e` as `s(t)`/`gp(t)` functions of time
inside `brms`'s existing nonlinear formula interface — no hand-written Stan,
despite an earlier roadmap draft claiming otherwise.

**Call graph:** `fit_response_tv()` → `hlpr_tv_identifiability()` (pre-fit,
advisory) → `hlpr_scale_data()`/`hlpr_resolve_prior_tv()` (reused from the
base fit path; `hlpr_resolve_prior_tv()` only adds the `e → le` log-form
reparameterization) → `hlpr_define_response_form_tv()` → `brms::brm()` →
post-fit convergence gate (authoritative) → `hlpr_params_tv()` for the cached
trajectory. `mrm_tv_snapshot()` and `hlpr_infer_tv_snapshot()` are the
`opt_mix()` bridge — see the `mrmfit_tv_snapshot` entry under Core S3
Objects. `mrm_plot_tv()` covers `type = "trajectory"` (parameter vs. time)
and `type = "evolution"` (a heatmap of the fitted surface, time on x / spend
on y, plus discrete curve snapshots colored by date, spend on x / KPI on y —
combined via `patchwork`).

**Key files:** `R/fit_response_tv.R`, `R/hlpr_define_response_form_tv.R`,
`R/hlpr_resolve_prior_tv.R`, `R/hlpr_tv_identifiability.R`,
`R/hlpr_params_tv.R`, `R/mrm_tv_snapshot.R`, `R/hlpr_infer_tv_snapshot.R`,
`R/print.mrmfit_tv.R`, `R/mrm_summary_tv.R`, `R/mrm_plot_tv.R`.

**Design decisions and their evidence** (from `dev/tv_smooth_curves.R`,
`dev/tv_smooth_params.R`, `dev/tv_paid_search_fix.R`, `dev/hier_time_pooling.R`
— 35+ real fits across three channels with very different spend variation):

- **Why not `fit_response_hier(group = <time bucket>)`?** It already works
  for pooling the ceiling alone at moderate bucket counts, but with 9
  quarterly buckets, `pool = "d"` alone froze `b`/`e` at a population value
  that fit no individual bucket (Linear TV: population `e = 1.81M` vs. every
  quarter's own independently-fit midpoint of 253K–987K — numerically linear
  to rounding error over any single quarter's actual spend range), while
  `pool = c("b","e","d")` (the function's own default) diverged outright
  (458 divergences, ESS 70) because too few groups remained to estimate
  three group-level variances.
- **`method = "spline"` default, not exact GP.** Measured on one channel:
  `s(t)` — 76s, 0 divergences, 0% max-treedepth. Exact `gp(t)` — 2,914s (49
  min) *even parallelized across 4 cores*, and 25% of transitions hit max
  treedepth (a real sampler-geometry problem, not just slowness).
  `gp(t, k=, c=5/4)` (the Hilbert-space approximation, `method = "gp_approx"`)
  matched `s(t)`'s cost and cleanliness.
- **`varying = "e"` default, not the original roadmap sketch's
  `c("d","e")`.** Across three channels with robust spend-quantile ratios of
  ~1.9x, ~5-31x, and ~800x, letting `e` vary was the only choice clean on
  all three. `d` was close behind. Letting `b` vary alone, or letting two
  parameters vary jointly, produced genuine posterior multimodality on the
  narrowest-range channel — confirmed by refitting with 6x the
  warmup/iterations and a stricter `adapt_delta` (0.999 vs 0.99): the
  problem chain landed in the *same* alternate mode both times, and the
  joint-variation case's R-hat got *worse* with more sampling. This is why
  the post-fit convergence gate is authoritative and the pre-fit
  spend-ratio heuristic in `hlpr_tv_identifiability()` is explicitly
  documented as advisory only, built from limited evidence.
- **No free intercept-equivalent tuning was needed.** Unlike the (shelved)
  cross-channel synergy work, `brms`'s own default smooth/GP hyperparameter
  priors were sufficient across every fit validated — no custom prior tier
  was added for `sds_*`/`sdgp_*`/`lscale_*`.

---

## Prior Specification (3-tier system)

1. **Automatic** (`auto = TRUE` in `fit_response`) — smart defaults
2. **Simplified** via `mrmopt_prior()` — scale-invariant bounds:
   - `midpoint_range`: inflection point as fraction of x-axis
   - `ceiling_max`: multiplier on observed max response
   - `floor_min`: lower asymptote in original units
   - `anchor_strength`: fraction of observed y range used as prior SD on the floor (`c`) parameter (default `0.05`). Controls how tightly the floor is constrained around `floor_min`. Set to `NULL` for loose behavior.
3. **Manual** — raw `brms::prior()` objects

---

## Data Scaling Strategy

1. Compute scaling parameters from real data only
2. For log-forms: inject offset if zeros detected, then ratio-scale (x/max)
3. For standard forms: min-max or standardization
4. Store scaling values on `$scale_values` for automatic unscaling in inference

---

## Testing Infrastructure

**30 test files** in `tests/testthat/` with **714+ tests**. Key conventions:
- **`helper-mock.R`** provides `make_mock_mrmfit()` fixture — builds lightweight mock `mrmfit` objects without MCMC, enabling fast isolated tests. Also provides `as_draws_df.mock_brmsfit()` for testing posterior draw extraction.
- Tests cover: response models, helpers, scaling, fitting (input validation), plotting, palette, parameters, optimization (build_solution, extract_draws, constraints, opt_mix validation)
- Run with `devtools::test()`

---

## Key Recent Changes (from prior sessions)

- **`fit_response_tv()`**: New function fitting time-varying response curves
  via `s(t)`/`gp(t)` terms inside `brms`'s existing nonlinear formula
  interface (no hand-written Stan). New class `mrmfit_tv` (does not inherit
  `mrmfit`) and `mrmfit_tv_snapshot` (does, via `mrm_tv_snapshot()` — the
  `opt_mix()` bridge). New files: `R/fit_response_tv.R`,
  `R/hlpr_define_response_form_tv.R`, `R/hlpr_resolve_prior_tv.R`,
  `R/hlpr_tv_identifiability.R`, `R/hlpr_params_tv.R`, `R/mrm_tv_snapshot.R`,
  `R/hlpr_infer_tv_snapshot.R`, `R/print.mrmfit_tv.R`, `R/mrm_summary_tv.R`,
  `R/mrm_plot_tv.R`. One small addition to an existing file:
  `mrm_infer()`'s per-unit-view short-circuit now also recognizes
  `mrmfit_tv_snapshot` objects (needed so `mrm_summary()`'s internal
  `mrm_infer()` call works on a snapshot). See `## Time-Varying Response
  Curves` above for the full design rationale and the evidence (35+ real
  fits) behind `varying = "e"`/`method = "spline"` as defaults.
- **Cost-per-KPI objectives**: Added `objective = "target_cpk"` and `objective = "target_mcpk"` — convenience wrappers around `target_roi`/`target_mroi` that accept and report cost-per-KPI instead of ROI. Internally converts: `target_roi = 1/target_cpk`, `target_mroi = 1/target_mcpk`. Return gains `$target_cpk`/`$achieved_cpk`, `$target_mcpk`/`$channel_mcpk`. `opt_summary.R` displays CPK/mCPK framing. Useful for non-revenue KPIs (leads, visits, opportunities).
- **Flexible-budget optimisation objectives**: Added `objective = "target_roi"` and `objective = "target_mroi"` to `opt_mix()`. Target ROI maximises incremental KPI subject to portfolio ROI ≥ target (budget is an output). Target mROI sets each channel's spend where dy/dx = target (per-channel root-finding, no multi-channel solver). Both support `method = "point"` and `method = "posterior"`. New files: `R/opt_mix_target_roi.R`, `R/opt_mix_target_mroi.R`, `R/hlpr_baseline_kpi.R`, `R/hlpr_numerical_mr.R`. Return structure gains `$objective`, `$target_roi`/`$achieved_roi`, `$target_mroi`/`$channel_mroi`. `opt_summary.R` and `opt_table.R` are objective-aware. Mock fixture (`helper-mock.R`) now includes `params_hier_unit` for `hlpr_params()` compatibility.
- **opt_mix API redesign**: `summary.opt_mix_result()` now produces the formatted console output (previously done by `print`). `print.opt_mix_result()` is a thin wrapper calling `summary()`. `opt_table()` is a new plain exported function that returns the tidy comparison tibble (previously returned by `summary()`). All internal `plot_opt_*` helpers are now standalone exported functions named `opt_plot_allocation()`, `opt_plot_comparison()`, `opt_plot_posterior()`, `opt_plot_curves()`, `opt_plot_returns()`, and `opt_plot_compare()`. `plot.opt_mix_result()` and `plot.opt_mix_compare()` are thin dispatchers calling the `opt_plot_*` functions. `opt_plot_posterior()` now hard-errors (instead of message + fallback) when called on a point result.
- **`compare()` function**: S3 generic + method for side-by-side diff of two `opt_mix_result` objects with `plot.opt_mix_compare` dumbbell chart.
- **Response curve overlay plots**: `plot(opt, type = "curves")` shows response curves with current + optimal points; `plot(opt, type = "returns")` shows AR/MR curves at both positions. Uses `hlpr_opt_metrics()` for interpolation.
- **`mrms` stored on result**: `opt_mix()` now stores the model list on the result so curve/returns plots can access response data.
- **`opt_mix()` rewrite**: Two-layer architecture with `method = "point"` (fast, single solution) and `method = "posterior"` (distribution of solutions via raw posterior draws — 10,000x faster than `posterior_epred()`). Unified return structure, S3 class `opt_mix_result` with `print`, `plot`, and `summary` methods. Budget/n_weeks support for period-level optimization.
- **Constraint system**: User-supplied constraints data frame with absolute bounds (`min_spend`/`max_spend`), share-based bounds (`min_share`/`max_share`), and fixed channels (`fixed = TRUE`).
- **`hlpr_build_solution()`**: Shared builder for unified solution tibble with current-state metrics, optimal units (static CPU), response rates, CIs, and shares.
- **`hlpr_extract_draws()`**: Pre-extracts and unscales all posterior draws from fitted models for fast optimization loop evaluation.
- **`anchor_strength` prior**: Replaced the synthetic (0,0) anchor point injection with `anchor_strength` in `mrmopt_prior()` — a prior-based floor constraint that works uniformly across all 6 curve types. `anchor_zero` is deprecated.
- **Trace plot labels**: Fixed strip label truncation — renamed mcmc.list columns to short labels (`b`, `c`, `d`, `e`) before passing to `bayesplot::mcmc_trace()` in `R/plot.mrmfit.R`
- **`date_range` metadata**: Stored `c(min(date), max(date))` on all fitted models during `fit_response()`
- **`mrm_plot_compare()` label collision fix**: When comparing same-channel/same-type models across time periods, appends short date range `"Mon 'YY–Mon 'YY"`; respects user-supplied `names(models)`
- **cmdstanr default backend (July 2026)**: Benchmarking showed `cmdstanr` is ~9x faster than `rstan` for typical mrmopt models (192-run factorial benchmark across backends, iterations, chains, curve types, adapt_delta, and dataset sizes). Changes: (1) added `cmdstanr` to DESCRIPTION `Imports`; (2) `fit_response()` and `fit_response_hier()` gained a `backend` parameter defaulting to `"cmdstanr"`, with automatic fallback to `"rstan"` if cmdstanr/CmdStan not installed; (3) `R/hlpr_resolve_backend.R` implements the detection + fallback logic; (4) `getting_started.Rmd` documents the backend and installation instructions. Benchmark script lives at `~/Documents/roeh/benchmark_backends.R` (outside the package).
