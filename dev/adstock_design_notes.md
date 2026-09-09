# Adstock as a preprocessing option (reference note, not an implementation plan)

**Status: not being implemented now.** No code changes. This session's action is two
documentation writes:

1. **`ROADMAP.md`** — replace the existing "Adstock Support" entry (Medium-Term) with a
   short, roadmap-voiced version of the "Corrected direction" below (plain preprocessing,
   user-supplied decay, no estimation); add a one-line pointer to `dev/adstock_design_notes.md`
   for anyone wanting the fuller research. Also add an **Experimental** label to the
   "Time-Varying Response Curves" Recently-Shipped entry for `fit_response_tv()`.
2. **`dev/adstock_design_notes.md`** (new) — the full content below (corrected direction +
   the ruled-out joint-estimation research) preserved verbatim as the evidentiary record,
   matching this project's existing convention of keeping detailed research in `dev/` while
   `ROADMAP.md` stays concise.

## Corrected direction

Adstock should **not** be jointly estimated (no new sampled parameter, no grid-of-fits +
stacking approximation, no new `mrmfit_adstock` S3 class). It should be a **plain,
deterministic preprocessing step on spend**, applied the same way `scale_data`/
`scale_method` already transform spend today — a fixed transform computed from a supplied
decay value, not a modeled unknown.

Concretely: `fit_response(..., adstock = FALSE)` (or a numeric decay, e.g. `adstock = 0.75`)
applies `hlpr_geo_adstock(spend, decay)` — the plain recursive `out[t] = spend[t] +
decay*out[t-1]` transform, sorted by `date` first since row order is load-bearing here
(new to this codebase; today `fit_response()` drops `date` after computing `$date_range`
and never sorts by it) — to the raw spend column **before** `hlpr_scale_data()` runs,
exactly mirroring the existing preprocessing-pipeline position of scaling. No new nlpar, no
new prior tier, no new Stan code, no new return class — the fitted object is a completely
ordinary `mrmfit`, just fit on adstocked spend. The user picks (or is guided toward) a
decay value the same way they already pick `scale_method`; this package doesn't estimate
it for them.

A small companion helper is still worth keeping from the earlier research below:
`hlpr_geo_adstock_future(hist_spend, future_spend, decay)` for projecting KPI from planned
future spend — seeds the recursion from real historical state rather than resetting to zero
at week 1 of a plan (validated by hand this session; see the "adstock the planned spend"
exchange). This can ship as a simple exported utility alongside the preprocessing option,
independent of any estimation question.

**Separately: `fit_response_tv()` (shipped earlier this session) should be documented and
labeled as experimental** wherever it's described (`ROADMAP.md`, `AGENTS.md`, `?fit_response_tv`)
— a note to carry into whatever writeup captures this.

---

## Why the estimated version (jointly-estimated decay, full posterior) was ruled out

Kept here for reference so this research doesn't need to be redone if joint estimation is
ever reconsidered.

## Original Context (superseded by the corrected direction above)

`ROADMAP.md`'s "Adstock Support" entry (Medium-Term) sketches `fit_response(..., adstock =
TRUE)` as jointly estimating a geometric adstock decay parameter alongside the saturation
curve, "with full posterior uncertainty," inside a single `brms::brm()` call. This session's
own ad hoc work motivated the feature directly: on real Linear TV data, raw spend/KPI
correlation was a weak 0.61 (spend is jagged week-to-week, KPI moves smoothly — carryover,
not noise); pre-transforming spend with a single grid-searched decay (0.75) raised it to
0.92 and took the static curve's R² from 0.365 to 0.855. That grid search was correctly
flagged at the time as overfit-prone (one point estimate, maximized against the same 103
observations it's evaluated on) — this plan is how to do that properly.

Before writing any code, this planning pass researched two things: (1) the exact extension
points in `fit_response()`/`hlpr_define_response_form.R`/`hlpr_scale_data.R` a new `adstock`
argument would need (via a full codebase survey), and (2) whether the roadmap's literal
promise — a single `brm()` call with decay as a genuine jointly-sampled nonlinear parameter,
full continuous posterior — is actually achievable inside brms's formula interface. It is
not, for a concrete, load-bearing reason:

**Geometric adstock is a recursive, order-dependent transform** (`adstocked[t] = spend[t] +
decay·adstocked[t-1]`) that needs the *entire* spend vector in time order and one sequential
Stan loop, re-evaluated once per leapfrog step as a function of the current `decay` draw.
brms's `stanvar()` mechanism is purely **additive** — it injects new Stan code into named
blocks, but cannot rewrite the `mu[n] = ... C_1[n] ...` line brms itself auto-generates for
the main nonlinear predictor, where `C_1` is a fixed `data`-block array populated once from
the raw spend column. No formula composition (`nlf()`, extra `bf()` terms, more `stanvar()`
blocks) can retarget that reference to a `transformed parameters`-block adstocked vector —
this was confirmed by generating and reading brms's actual output Stan code, not assumed.
The only way to get a true joint continuous posterior is to hand-patch brms's generated Stan
code (regex-replace the auto-generated covariate reference) and reattach it to a `brmsfit`
via brms's `empty = TRUE` escape hatch — a real, working mechanism, but one that depends on
the *exact text* of brms-generated code and internal parameter-naming conventions
(`Intercept_decay` in `parameters`, not the `b_decay_Intercept` alias that only exists in
`generated quantities`, confirmed by deriving it rather than guessing) — substantially more
fragile and version-sensitive than anything else in this package, and confirmed to have zero
precedent anywhere in `R/` (`grep -rn "stanvar\|nlf(" R/` returns nothing).

**Decision: ship the lower-risk, fully-precedented approach now; file the continuous-joint-
posterior mechanism as separately gated future R&D, not part of this implementation.** The
lower-risk approach — fit `fit_response()` at a grid of fixed decay values (reusing 100% of
existing code, zero new Stan) and combine via PSIS-LOO stacking weights — delivers a
genuine, if grid-resolution-limited, approximate posterior over decay, and every downstream
piece of the package (`mrm_*`, `opt_mix()`) can consume it through the same synthetic-draws
mechanism already established and shipped this session for `mrm_tv_snapshot()`. This is a
real, defensible interpretation of "estimated with posterior uncertainty," just not a
continuous one. If a future session wants to pursue the continuous version, a feasibility
spike script is sketched at the end of this document as the starting point — it is not part
of this plan's scope.

*(This file previously held the planning record for `fit_response_tv()`, which shipped
earlier this session — see `AGENTS.md`'s "Time-Varying Response Curves" section and
`ROADMAP.md`'s Recently Shipped entry for that design record. This file now covers the
`adstock = TRUE` plan below.)*

---

## Public API

```r
fit_response(data, spend = NULL, kpi = NULL, date = NULL, units = NULL,
            auto = TRUE, type = "gompertz", scale_data = TRUE, scale_method = "min_max",
            midpoint_range = NULL, ceiling_max = NULL, floor_min = NULL, prior = NULL,
            adstock = FALSE,                          # NEW
            decay_grid = seq(0.1, 0.9, by = 0.2),      # NEW — 5 points by default
            decay_weighting = c("stacking", "pseudobma"),  # NEW
            chains = 4, iter = 4000, warmup = 1000,
            control = list(adapt_delta = 0.95),
            infer_xrange = NULL, infer_length = 1000,
            anchor_strength = NULL, anchor_zero = NULL,
            backend = "cmdstanr", refresh = 500, ...)
```

`date` is already a required argument for every existing caller (used today only for
`$date_range`) — `adstock = TRUE` is the first place it becomes load-bearing for row order,
not just metadata.

Every other argument (`type`, `scale_data`, `scale_method`, `midpoint_range`, `ceiling_max`,
`floor_min`, `prior`, `units`, `anchor_strength`, `chains`/`iter`/`warmup`/`control`,
`backend`) passes through **completely unmodified** to each per-grid-point fit — this
approach needs **zero changes** to `hlpr_define_response_form.R`, `hlpr_resolve_prior.R`,
`hlpr_scale_data.R`, or any Stan code. That is the direct payoff of the grid+stacking design
over the continuous-posterior mechanism: every existing validated code path in
[R/fit_response.R](mrmopt/R/fit_response.R) keeps working exactly as today for each
individual grid fit.

---

## Implementation flow

### 1. Branch near the top of `fit_response()`

Immediately after the existing `warmup >= iter` guard
([R/fit_response.R:107-115](mrmopt/R/fit_response.R:107)) and before the required-column
checks, validate `adstock`/`decay_grid`/`decay_weighting`, then delegate:

```r
if (isTRUE(adstock)) {
  return(hlpr_fit_response_adstock_grid(
    data = data, spend = spend, kpi = kpi, date = date, units = units,
    decay_grid = decay_grid, decay_weighting = match.arg(decay_weighting),
    type = type, auto = auto, scale_data = scale_data, scale_method = scale_method,
    midpoint_range = midpoint_range, ceiling_max = ceiling_max, floor_min = floor_min,
    prior = prior, chains = chains, iter = iter, warmup = warmup, control = control,
    infer_xrange = infer_xrange, infer_length = infer_length,
    anchor_strength = anchor_strength, anchor_zero = anchor_zero,
    backend = backend, refresh = refresh, ...
  ))
}
```

Validation: `decay_grid` must be numeric, all values strictly inside `(0, 1)`, length ≥ 2;
`decay_weighting` via `match.arg`. No new prior machinery is needed — decay is never a
sampled nlpar in this design, just a fixed value that varies across grid *fits*.

### 2. `R/hlpr_adstock_transform.R` (new, internal, no brms dependency)

```r
hlpr_geo_adstock(x, decay)
# out[1] <- x[1]; out[t] <- x[t] + decay * out[t-1]      -- pure R recursion

hlpr_geo_adstock_future(hist_spend, future_spend, decay)
# adstocks c(hist_spend, future_spend) as ONE continuous series so the recursion's
# state carries forward from real history into the plan, then returns only the
# future portion -- formalizes the manual concatenate-then-tail() pattern this
# session verified by hand for KPI projection (see the "adstock the planned
# spend" exchange: seeding from zero at week 1 of a plan silently discards real
# carryover from actual recent spend).
```

Both are pure, deterministic, zero external dependencies — fully unit-testable without
brms/Stan.

### 3. `R/hlpr_fit_response_adstock_grid.R` (new, internal)

```r
hlpr_fit_response_adstock_grid(data, spend, kpi, date, decay_grid, decay_weighting, ...)
```

- Sorts `data` by `data[[date]]` ascending **first** — adstock's row-order requirement is
  new to this codebase (today `fit_response()` drops `date` after computing `date_range`
  and never sorts by it; this is the first place order becomes load-bearing) and must be
  enforced explicitly, not assumed from caller input.
- Computes `hlpr_geo_adstock(data[[spend]], r)` for each `r` in `decay_grid`, producing one
  adstocked spend column per grid point, **on raw spend, before any scaling** — geometric
  adstock does not commute with an affine (offset) transform (min-max scaling), only with a
  pure ratio scale; adstocking first and letting each grid fit's own `hlpr_scale_data()` run
  afterward on the already-adstocked column, exactly as this session's `dev/
  tv_vs_static_comparison_adstocked*.R` scripts already did by hand, sidesteps this
  entirely — no scaling-order bug to introduce.
- Calls `fit_response(data_r, spend = "spend_adstocked", kpi=, date=, adstock = FALSE, ...)`
  once per grid point, forwarding every other argument unchanged, and forwarding `file =`
  (via `...`) with a per-decay suffix if the caller supplied one, so grid fits are
  individually cacheable exactly like any other `brm(file=)` fit — the same caching pattern
  this session's dev scripts already relied on.
- Computes `brms::loo(fit_r)` per grid fit and `brms::loo_model_weights(loo_list, method =
  decay_weighting)` to get one weight per grid point. Surfaces any Pareto-k warnings from
  individual `loo()` calls through to the final object rather than swallowing them.
- Assembles the combined return object (next section).

**Compute cost is real and should be stated plainly, not hidden**: `decay_grid` at its
5-point default means 5 full MCMC fits — roughly 5× the cost of one `fit_response()` call.
This is exactly why the default grid is coarse (0.2 spacing) rather than the 19-point 0.05
grid used for exploration this session; a finer grid is available by passing a longer
`decay_grid`, at proportional cost, and `file=` caching is the intended mitigation for
repeated calls during development.

---

## Return object — class `c("mrmfit_adstock", "mrmfit")`, inherits `mrmfit`

Deliberately parallel to `mrmfit_tv_snapshot`'s reasoning
(`AGENTS.md`'s "Core S3 Objects" section): this **is** a single static curve — decay is a
fitting-time nuisance parameter being marginalized over, not something the object is a
function of the way `mrmfit_tv` is a function of time — so every existing `mrmfit`
consumption point (`opt_mix()`, `mrms_plot_compare()`, `mrm_plot_response()`) should work on
it with no special-casing beyond what a new `mrm_infer()` branch provides (below).

New fields: `$decay_grid`, `$decay_weights` (named vector, one per grid point), `$decay_summary`
(weighted mean + weighted 5%/95% grid-interval — explicitly documented as a **grid-resolution
approximation**, not a continuous credible interval), `$grid_fits` (named list of the full
per-decay `mrmfit` objects, kept for transparency/diagnostics — e.g. inspecting an individual
grid point's own `print()`/`mrm_plot_response()` directly), `$loo_pareto_k_warnings` (any
flagged observations across grid fits).

**`$response_df`/`$summary`**: each grid fit already computes its own `$response_df` via the
existing `mrm_infer()` call inside `fit_response()` — forcing a shared `infer_xrange`/
`infer_length` across all grid fits (pass explicit values rather than each grid fit's own
default) means their `response_df`s share an x-grid, so the combined curve is simply the
**decay-weighted average** of `center`/`lower`/`upper` across grid fits at each x-grid point
— cheap, and avoids re-deriving inference logic. `mrm_summary()` then runs unmodified against
this combined `response_df`, exactly as it already does for any ordinary `mrmfit`.

**Posterior draws for `opt_mix(method = "posterior")`**: pool draws from each grid fit's
`as_draws_df()`, resampled proportional to `$decay_weights` (mirroring `mrm_tv_snapshot()`'s
`.snapshot_draws` mechanism — [R/mrm_tv_snapshot.R](mrmopt/R/mrm_tv_snapshot.R)'s pattern of
assembling a synthetic `draws_df`-classed object and registering
`as_draws_df.mrmfit_tv_snapshot` via `@exportS3Method`). Register
`as_draws_df.mrmfit_adstock` the same way, so `hlpr_extract_draws()` — and therefore
`opt_mix()`'s full posterior path ([R/opt_mix.R:458](mrmopt/R/opt_mix.R:458)) — needs no
changes to consume a `mrmfit_adstock` object.

---

## S3 methods and a new projection helper

- **`R/mrm_infer.R`**: one new branch, structurally identical to the existing
  `mrmfit_hier_unit` and `mrmfit_tv_snapshot` short-circuits already in this file — for
  default arguments, return the cached `$response_df`; for a custom `xrange`, error with a
  pointer to `$grid_fits` for per-decay-point inspection (there's no single live sampler to
  call `predict()`/`fitted()` against on a pooled object, same reasoning as the existing two
  branches).
- **`R/print.mrmfit_adstock.R`**: reuses `cli_rule()` ([R/print.mrmfit.R:33-44](mrmopt/R/print.mrmfit.R:33)).
  Surfaces `$decay_summary` prominently (weighted mean, grid-resolution interval, explicit
  "approximate, not continuous" caveat — matching the house pattern of putting
  safety/interpretation caveats where they can't be missed, as `mrmfit_tv`'s identifiability
  gate already does), the decay grid and its weights as a small table, any Pareto-k
  warnings, and the usual R²/curve summary. States the total compute cost incurred
  (`length(decay_grid)` fits) so the cost isn't a silent surprise on repeat runs.
- **`R/mrm_adstock_future.R`** (new, exported): `mrm_adstock_future(mrm, future_spend,
  future_dates)` — wraps `hlpr_geo_adstock_future()` using `$decay_summary`'s weighted-mean
  decay (with an option to return one projection per grid point for full uncertainty
  propagation), directly productizing the manual future-spend-projection workflow this
  session built and validated by hand earlier (concatenate history + plan, adstock the
  whole series, keep the future tail) — this closes the loop the session's own "how do I
  adstock planned spend for projection" question left as ad hoc code.
- A small `mrm_plot_adstock_weights()` (bar chart of `$decay_weights` over `$decay_grid`) is
  a reasonable low-effort addition for interpretability, following `opt_plot_posterior()`'s
  existing violin/bar visual pattern — worth including but not blocking the rest.

---

## Tests

- **`tests/testthat/test-hlpr_adstock_transform.R`** (new) — fully deterministic, no brms:
  `hlpr_geo_adstock()` recursion correctness against hand-computed small vectors; `decay = 0`
  is the identity transform; `decay` near 1 doesn't blow up on a short series; single-row
  input; `hlpr_geo_adstock_future()`'s seeded continuation matches manually concatenating
  history+future and adstocking the whole series (this is the exact check this session ran
  by hand — turning it into an automated test directly captures that verified result).
- **`tests/testthat/test-fit_response.R`** (extended) — validation-only additions matching
  the file's existing style: non-logical `adstock` errors; `decay_grid` with a value outside
  `(0,1)` or length < 2 errors; invalid `decay_weighting` via `match.arg` errors; asserts no
  path reaches `brms::brm()` for any of these (same injected-invalid-value-after-the-check
  idiom already used throughout this file to guarantee a fast `stop()`).
- **`tests/testthat/helper-mock.R`** — append `make_mock_mrmfit_adstock()`: a handful of mock
  `mrmfit`s (reusing `make_mock_mrmfit()`) as `$grid_fits`, fixed `$decay_grid`/`$decay_weights`,
  and a mock `as_draws_df.mrmfit_adstock` — same "appended only, existing fixtures untouched"
  discipline already followed for `make_mock_mrmfit_tv()`.
- **`test-mrm_infer.R`**, **`test-print.mrmfit_adstock.R`**, **`test-opt_mix.R`** — one
  addition each, against the new mock fixture: `mrm_infer()`'s new branch behaves like the
  two existing per-unit-view branches; `print()` surfaces `$decay_summary`; `opt_mix()`
  accepts a `mrmfit_adstock` object in its model list (the whole reason for the
  `as_draws_df`/inherits-`mrmfit` design).

---

## Documentation and shipment

1. **roxygen** — `@param` for the three new arguments; `@return` documents the new fields;
   `@details` states plainly (a) this is a grid-resolution approximate posterior, not a
   continuous one, and why (the `stanvar()` limitation found during planning — see below);
   (b) compute cost scales with `length(decay_grid)`; (c) adstock is applied to raw spend
   before scaling and why that ordering matters. `hlpr_*` files get `@keywords internal`.
2. **NAMESPACE** via `devtools::document()`: `export(mrm_adstock_future)`,
   `export(mrm_plot_adstock_weights)`, `S3method(print, mrmfit_adstock)`,
   `S3method(as_draws_df, mrmfit_adstock)` (`@exportS3Method posterior::as_draws_df`,
   matching the existing `mrmfit_tv_snapshot` pattern exactly).
3. **`AGENTS.md`** — a `### mrmfit_adstock` block under "Core S3 Objects," sibling to
   `mrmfit_tv_snapshot`; a short note under the adstock feature recording *why* the
   grid+stacking design was chosen over a continuous joint posterior (the `stanvar()`
   additive-only limitation, confirmed by reading generated Stan code) so a future session
   doesn't have to rediscover this from scratch.
4. **`ROADMAP.md`** — rewrite "Adstock Support" (Medium-Term) to describe the shipped
   grid+stacking design, with a **Known limitations** block (grid-resolution-limited
   uncertainty, not continuous; N× compute cost; decay/curve-shape correlation only
   captured at grid resolution). Add a new small Longer-Term/Exploratory entry, **"Continuous
   Joint Adstock Posterior (R&D, deferred)"**, documenting the exact technical blocker (the
   `C_1[n]` / additive-`stanvar()` finding) and pointing to
   `dev/adstock_feasibility_spike.R` (below) as the starting point if ever picked up — this
   is explicitly *not* part of this implementation.

### `dev/adstock_feasibility_spike.R` — not part of this plan's scope, recorded for later

If the continuous-posterior direction is ever pursued, the starting point is a spike script
that (1) reads brms's actual generated Stan code to confirm the `Intercept_decay` /
`C_k[n]` naming rather than assuming it, (2) injects a custom `functions`-block adstock
function via `stanvar()`, (3) regex-patches the generated code's covariate reference, (4)
compiles/samples directly via `cmdstanr` and reattaches to a `brmsfit` via `brm(...,  empty
= TRUE)`, and (5) checks recovery of a *known* true decay on synthetic data. Six concrete
pass/fail criteria for that spike (naming confirmed, substitution fires, row order
preserved, clean sampling geometry, true decay inside the posterior interval, all existing
mrmopt post-processing still works against the reattached fit) were worked out during this
planning pass and should gate any future decision to build it — this file should be created
only if that R&D work is explicitly picked up, not as part of this implementation.

---

## Verification

Per this project's established practice (see the "verify simulation DGP" discipline this
session's memory notes — parameter recovery isn't meaningful validation unless the
synthetic generator matches the real upstream pipeline's actual characteristics), the
recovery check must **not** use a toy i.i.d. spend series:

```bash
cd /Users/roeh/Documents/roeh/mrmopt && Rscript -e '
devtools::load_all()
# Bootstrap-resample REAL historical spend (preserves actual jaggedness, zero-spend
# weeks, autocorrelation structure) rather than synthesizing idealized spend.
real_spend <- readRDS("data-raw/tv_vs_static_cache/linear_tv_weekly.rds")$spend
true_decay <- 0.6
synthetic <- data.frame(
  date = seq.Date(as.Date("2023-01-01"), by = "week", length.out = length(real_spend)),
  spend = sample(real_spend, length(real_spend), replace = TRUE)
)
adstocked_true <- hlpr_geo_adstock(synthetic$spend, true_decay)
# ... generate kpi from a known logistic curve on adstocked_true + realistic noise ...
fit <- fit_response(synthetic, spend="spend", kpi="kpi", date="date", type="logistic",
                    adstock = TRUE, decay_grid = seq(0.1,0.9,by=0.1))
print(fit)   # confirm true_decay falls inside $decay_summary's weighted interval
'
```

Then the standard package checks:

```bash
cd /Users/roeh/Documents/roeh/mrmopt && Rscript -e 'devtools::document(); devtools::test(); devtools::check()'
```

Finally, a real-data smoke test reusing this session's already-fitted Linear TV data,
confirming the grid+stacking result is directionally consistent with this session's own
manual finding (decay ≈ 0.7–0.75 maximized raw correlation; the shipped weighted-posterior
mean should land in a comparable region, though not necessarily identical since it's now
optimizing predictive LOO performance rather than raw correlation):

```bash
cd /Users/roeh/Documents/roeh/mrmopt && Rscript -e '
devtools::load_all()
weekly <- readRDS("data-raw/tv_vs_static_cache/linear_tv_weekly.rds")
fit <- fit_response(weekly, spend="spend", kpi="kpi", date="date", type="log_logistic",
                    adstock = TRUE)
print(fit)                                    # decay summary, weights, cost note
opt_mix(list(linear_tv = fit), total_budget = 1e6)   # confirms the opt_mix bridge
mrm_adstock_future(fit, future_spend = rep(2e6, 8),
                   future_dates = max(weekly$date) + (1:8)*7)  # projection helper
'
```
