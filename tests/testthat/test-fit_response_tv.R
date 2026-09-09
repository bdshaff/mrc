# fit_response_tv() input validation tests -- no MCMC calls.
# All tests exercise the early-exit error/warning paths only, matching the
# convention in test-fit_response.R.

make_valid_tv_data <- function(n = 60) {
  set.seed(101)
  data.frame(
    spend = runif(n, 1e4, 1e6),
    kpi   = runif(n, 100, 1000),
    date  = as.Date("2023-01-02") + (seq_len(n) - 1) * 7
  )
}

test_that("errors when spend/kpi/date are NULL", {
  d <- make_valid_tv_data()
  expect_error(fit_response_tv(d, kpi = "kpi", date = "date"), "must all be specified")
  expect_error(fit_response_tv(d, spend = "spend", date = "date"), "must all be specified")
  expect_error(fit_response_tv(d, spend = "spend", kpi = "kpi"), "must all be specified")
})

test_that("errors when a named column is missing", {
  d <- make_valid_tv_data()
  expect_error(
    fit_response_tv(d, spend = "nope", kpi = "kpi", date = "date"),
    "not found in data"
  )
})

test_that("errors when varying is NULL or empty", {
  d <- make_valid_tv_data()
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", varying = NULL),
    "at least one of"
  )
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", varying = character(0)),
    "at least one of"
  )
})

test_that("errors when varying contains an unrecognized name", {
  d <- make_valid_tv_data()
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", varying = "z"),
    "unrecognized parameter name"
  )
})

test_that("warns (does not error) when varying includes 'c'", {
  d <- make_valid_tv_data()
  d$kpi[1] <- -1  # guaranteed stop() shortly after, before any brm() call
  expect_warning(
    tryCatch(
      fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", varying = c("c", "e")),
      error = function(e) NULL
    ),
    "rarely drifts"
  )
})

test_that("errors on an invalid method via match.arg", {
  d <- make_valid_tv_data()
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", method = "bogus"),
    "should be one of"
  )
})

test_that("warns when method = 'gp' is combined with an explicit k", {
  d <- make_valid_tv_data()
  d$kpi[1] <- -1  # guaranteed stop() shortly after, before any brm() call
  expect_warning(
    tryCatch(
      fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date",
                      method = "gp", k = 20),
      error = function(e) NULL
    ),
    "ignores `k`"
  )
})

test_that("errors when date has fewer than 2 distinct values", {
  d <- make_valid_tv_data()
  d$date <- as.Date("2023-01-02")
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date"),
    "at least"
  )
})

test_that("errors on negative kpi", {
  d <- make_valid_tv_data()
  d$kpi[1] <- -5
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date"),
    "negative"
  )
})

test_that("errors when a units column contains a zero", {
  d <- make_valid_tv_data()
  d$units <- 100
  d$units[1] <- 0
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", units = "units"),
    "zero values"
  )
})

test_that("errors when both raw prior and simplified prior args are supplied", {
  d <- make_valid_tv_data()
  raw_prior <- brms::prior(normal(-4, 10), nlpar = "b", lb = -10, ub = 0)
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date",
                    prior = raw_prior, ceiling_max = 3),
    "Cannot specify both"
  )
})

test_that("errors when auto = FALSE, scale_data = FALSE and no prior is provided", {
  d <- make_valid_tv_data()
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date",
                    auto = FALSE, scale_data = FALSE),
    "must be provided"
  )
})

test_that("errors when auto = FALSE, scale_data = FALSE and the prior is not a brmsprior", {
  d <- make_valid_tv_data()
  expect_error(
    fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date",
                    auto = FALSE, scale_data = FALSE, prior = "not a prior"),
    "not of class 'brmsprior'"
  )
})

test_that("warns and halves warmup when warmup >= iter", {
  d <- make_valid_tv_data()
  d$kpi[1] <- -1  # guaranteed stop() shortly after, before any brm() call
  expect_warning(
    tryCatch(
      fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date",
                      iter = 100, warmup = 100),
      error = function(e) NULL
    ),
    "must be less than"
  )
})

test_that("warns when the pre-fit identifiability heuristic flags low spend variability", {
  d <- make_valid_tv_data()
  d$spend <- 1000 + runif(nrow(d), -1, 1)  # ratio ~1
  d$kpi[1] <- -1  # guaranteed stop() shortly after, before any brm() call
  expect_warning(
    tryCatch(
      fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date", varying = "e"),
      error = function(e) NULL
    ),
    "Spend variability looks low"
  )
})

test_that("identifiability_check = FALSE skips the pre-fit warning", {
  d <- make_valid_tv_data()
  d$spend <- 1000 + runif(nrow(d), -1, 1)
  d$kpi[1] <- -1  # guaranteed stop() shortly after, before any brm() call
  expect_warning(
    tryCatch(
      fit_response_tv(d, spend = "spend", kpi = "kpi", date = "date",
                      varying = "e", identifiability_check = FALSE),
      error = function(e) NULL
    ),
    NA  # no identifiability warning; the negative-kpi error still fires silently via tryCatch
  )
})
