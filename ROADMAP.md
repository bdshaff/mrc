# Roadmap

This page tracks planned features and longer-term research directions for `mrmopt`.
Items are organized by horizon and reflect both near-term engineering work and
more exploratory modeling directions.

Feedback and contributions are welcome — open an issue on
[GitHub](https://github.com/Roeh-Marketing/mrmopt/issues) to discuss any of these.

---

## Recently Shipped

### Within-Channel Hierarchical Response Curves

**New function:** `fit_response_hier()` — **shipped.** See the
[Hierarchical Response Curves](https://roeh-marketing.github.io/mrmopt/articles/hierarchical_models.html)
article for a worked example.

Media channels are rarely homogeneous. TV spend spans broadcast, cable, and
streaming — each with different audience reach and response dynamics. Social
covers multiple partners, tactics, and creatives. Standard response curve
modeling treats the channel as a single unit, which either overfits sparse
sub-channel data or discards granularity altogether.

`fit_response_hier()` fits a single hierarchical model for one channel where
curve parameters are partially pooled across sub-channel groupings:

```r
fit_tv <- fit_response_hier(
  data      = tv_data,
  spend     = "spend",
  kpi       = "conversions",
  date      = "week",
  group     = c("subtype", "station"),   # nested hierarchy (any depth)
  type      = "gompertz"
)
```

The fixed effects represent the channel-level mean curve. Random effects at
each level (subtype → station) are drawn from the level above, so sparse units
(e.g., a small cable station with a handful of weeks) borrow strength from
better-identified peers. The degree of shrinkage is automatic and
data-driven — units with more observations get pulled less toward the group mean.

**Delivered:**

- Channel-level, sub-type-level, and unit-level curves from a single model
  (`mrm_summary_hier()`, `mrm_infer_hier()`)
- All six curve forms, including the log-based forms (midpoint reparameterized
  internally on the log scale for sampling stability)
- Arbitrary-depth nested hierarchies
- Posterior uncertainty correctly reflects data sparsity at each level
- Optimization at any level of the hierarchy via `as_mrmfit_list()` + `opt_mix()`
- Per-unit and shrinkage visualizations via `mrm_plot_hier()`
- Pooling operates on curve shape parameters (`b`, `e`); scale (`d`) is
  allowed to vary more freely to reflect size differences across units

---

### Time-Varying Response Curves

**Experimental.** Statistically validated across three real channels (see below), but the
API and defaults should still be considered subject to change as more channels are tried
against it.

**New function:** `fit_response_tv()` — **shipped.** See the
[Time-Varying Response Curves](https://roeh-marketing.github.io/mrmopt/articles/time_varying_curves.html)
article for a worked example.

Standard response curve fitting treats the whole observation window as a
single stationary period. `fit_response_tv()` instead models one or more of
the curve parameters (`b`, `c`, `d`, `e`) as smooth or Gaussian-process
functions of time, inside `brms`'s existing nonlinear formula interface — no
hand-written Stan required.

```r
fit_tv <- fit_response_tv(
  data    = channel_data,
  spend   = "spend",
  kpi     = "conversions",
  date    = "week",
  type    = "gompertz",
  varying = "e",         # which parameter(s) evolve over time
  method  = "spline"     # s(t); "gp_approx" or "gp" also available
)
```

**Available today — the intermediate formulation already ships too.**
Partial pooling of curve parameters across time buckets needs no new
function: `fit_response_hier()` validates `group` only as a column present
in the data, so a time bucket works exactly like a sub-channel
(`fit_response_hier(group = "quarter", pool = "d")`). It is a reasonable
first thing to try, but is unsafe for shape parameters at real-world bucket
counts — see Known limitations.

**Delivered:**

- `varying` — a subset of `b`/`c`/`d`/`e`; default `"e"`, chosen from
  cross-channel evidence (see below), not an assumption
- `method = "spline"` (default, `s(t)`), `"gp_approx"` (Hilbert-space
  approximate Gaussian process, comparable cost to spline), or `"gp"` (exact
  GP — available, not recommended as a default; see Known limitations)
- A two-layer identifiability gate: a fast pre-fit spend-variability
  heuristic (advisory), and a mandatory post-fit convergence check (R-hat,
  ESS, divergences, treedepth — authoritative), both surfaced by `print()`
- `mrm_tv_snapshot()` extracts a static curve — with full posterior
  uncertainty, not just a point estimate — at any date, which feeds
  `opt_mix()` exactly like an ordinary `fit_response()` fit
- `mrm_plot_tv()`: `type = "trajectory"` (a parameter plotted against time)
  and `type = "evolution"` (a continuous heatmap of the fitted curve
  surface — time on the x-axis, spend on the y-axis — alongside discrete
  curve snapshots colored by date, spend on x / KPI on y)
- Log-based curve forms reparameterize the midpoint on the log scale
  internally (same technique as `fit_response_hier()`), so a time-varying
  midpoint doesn't require `e > 0` to hold pointwise during sampling

**Known limitations:**

- A `mrmfit_tv` does not itself feed `opt_mix()` — its curve is a function
  of time, not a single point. Use `mrm_tv_snapshot()`.
- The pre-fit identifiability heuristic is built from limited evidence (one
  clear failure, two clear successes across three tested channels) and is
  not uniformly predictive — on the same failing channel, letting the
  ceiling alone vary improved with more sampling while letting steepness
  vary did not, even at 6x the warmup/iterations. Treat it as a reason to
  look closely at the post-fit gate, not as a verdict on its own.
- Exact `method = "gp"` is measurably worse than the alternatives, not just
  slower: on one channel, 2,914s (49 minutes) even parallelized across 4
  cores, versus 76s for `"spline"`, *and* 25% of transitions hit max
  treedepth (a real sampler-geometry problem). Reach for it only when its
  smoothness properties are specifically needed.
- `fit_response_hier(group = <time bucket>)` is unsafe for shape parameters
  at real-world bucket counts: pooling the ceiling alone can freeze the
  shape parameters at a population value that fits no individual bucket,
  while pooling all three shape/scale parameters together can fail to
  converge outright when too few buckets remain to estimate multiple
  group-level variances. `fit_response_tv()` avoids both failure modes by
  not bucketing at all.

---

## Medium-Term

### Rolling Window Response Curves (`mrm_rolling()`)

**New function:** `mrm_rolling()`

Standard response curve fitting treats the full observation window as a single
stationary period. In practice, the relationship between spend and KPI shifts
over time — seasonal demand, competitive pressure, and creative fatigue all
cause the effective ceiling and saturation point to vary. `mrm_rolling()` makes
this drift visible by fitting a sequence of response curves across overlapping
windows of the data.

```r
rolling_fits <- mrm_rolling(
  data        = channel_data,
  spend       = "spend",
  kpi         = "conversions",
  date        = "week",
  type        = "gompertz",
  window_size = 13,   # weeks per window
  stride      = 4     # weeks between window starts
)
```

Each window is fit independently using `fit_response()`, producing a list of
`mrmfit` objects anchored to their respective date ranges. A companion plot
shows how each parameter's posterior median and credible interval evolves across
windows, making seasonal drift in the ceiling (`d`) or midpoint (`e`) directly
visible.

**Key properties:**

- No new model architecture — each window calls `fit_response()` directly
- `window_size` controls the timescale of local estimation; `stride` controls
  resolution vs. compute cost
- Returns a named list of `mrmfit` objects compatible with `mrms_plot_compare()`
- A dedicated `mrm_plot_rolling()` will show parameter evolution over time with
  credible ribbons

**Note:** This is a diagnostic and exploratory tool. Because windows are fit
independently there is no formal pooling across them — adjacent windows do not
borrow strength from each other. `fit_response_tv()` (see Recently Shipped,
above) addresses this limitation with a principled shared estimation
framework — a smooth or GP term instead of independent windows.

---

### Adstock Support

A geometric adstock **preprocessing** option for `fit_response()`'s spend
column, via an `adstock` argument — applied the same way `scale_data`/
`scale_method` already transform spend today: a fixed, user-supplied decay,
not something the model estimates.

```r
fit <- fit_response(
  data    = my_data,
  spend   = "tv_spend",
  kpi     = "conversions",
  date    = "week",
  type    = "gompertz",
  adstock = 0.75   # fixed decay, applied to spend before scaling/fitting
)
```

Applied to raw spend (sorted by `date`, since row order is load-bearing for
this transform) before `hlpr_scale_data()` runs — no new sampled parameter,
no new prior, no new return class. A companion `mrm_adstock_future()` helper
projects KPI from planned future spend, seeding the recursion from real
historical carryover rather than resetting to zero at week 1 of a plan.

**Motivating evidence:** On real Linear TV data (103 weekly obs.), raw
spend-vs-KPI correlation is a weak 0.61 — the channel's spend is jagged
(near-zero one week, high the next), while KPI moves smoothly, consistent
with real carryover rather than noise. A geometric-adstock transform of
spend, with decay grid-searched to maximize correlation against KPI, raised
the correlation to 0.92 at decay ≈ 0.75 (half-life ≈ 2.4 weeks) — a clean
single peak in the grid, not a boundary artifact.

**Why decay isn't estimated by the model:** jointly estimating decay (a
recursive, order-dependent transform of the full spend vector) inside
`brms`'s nonlinear formula interface was investigated and ruled out —
`brms`'s `stanvar()` injection mechanism is additive only and cannot retarget
the covariate reference its own generated code uses for the main predictor,
confirmed by reading the actual generated Stan output rather than assumed.
A working approximation (fitting a grid of fixed-decay models and combining
via PSIS-LOO stacking weights) was designed but also set aside as
disproportionate complexity for what should be a simple preprocessing knob.
See `dev/adstock_design_notes.md` for the full research record, including
the ruled-out joint-estimation design, if this is ever revisited.

---

## Longer-Term / Exploratory

### Ground-Up Joint MMM (`fit_mmm()`)

`mrmopt` currently operates as a **response curve layer** — it fits saturation
curves to attributed KPI volumes produced by an upstream MMM. This is a valid
and practical workflow, but it means the saturation parameters are estimated
on data that already has an implicit independence and linearity assumption
baked in by the attribution model.

A ground-up joint MMM would estimate adstock decay, saturation curve
parameters, and channel contribution coefficients *simultaneously* from raw
spend and total KPI data — the approach taken by
[Google Meridian](https://github.com/google/meridian) and
[PyMC-Marketing](https://www.pymc-marketing.io/).

This would be implemented via a hand-written Stan model using
[`CmdStanR`](https://mc-stan.org/cmdstanr/) as the backend, giving full
control over the model structure without the constraints of the `brms`
formula interface. Priors would be aligned with Meridian's published defaults
(`ec ~ TruncatedNormal(0.8, 0.8, 0.1, 10)` on normalized spend,
`decay ~ Uniform(0, 1)`, `beta ~ HalfNormal(5)`).

**Key design:** A new `fit_mmm()` function returning a `mrmfit_joint` S3
class that feeds into the existing `opt_mix()` optimization infrastructure
via the same draw-matrix interface used by the posterior optimization path.

This is a substantial modeling and engineering undertaking and will be
developed incrementally. Single-market models without geo-level hierarchy
are the initial target scope.
