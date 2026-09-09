# tests/testthat/test-opt_mix.R
# Input validation and constraint helper tests (no MCMC)

# =========================================================================
# opt_mix input validation
# =========================================================================

test_that("opt_mix errors when mrms is not a list of mrmfit objects", {
  expect_error(opt_mix(list(1, 2, 3)), "named list of mrmfit")
  expect_error(opt_mix("not_a_list"), "named list of mrmfit")
})

test_that("opt_mix errors when mrms has no names", {
  m <- make_mock_mrmfit("gompertz")
  expect_error(opt_mix(list(m, m)), "named list")
})

test_that("opt_mix errors when mrms has empty names", {
  m <- make_mock_mrmfit("gompertz")
  mrms <- list(m, m)
  names(mrms) <- c("a", "")
  expect_error(opt_mix(mrms), "named list")
})

test_that("opt_mix rejects invalid method", {
  m <- make_mock_mrmfit("gompertz")
  expect_error(opt_mix(list(ch = m), method = "invalid"), "should be one of")
})

test_that("opt_mix accepts a mrm_tv_snapshot() output alongside ordinary mrmfits", {
  snap <- make_mock_mrmfit_tv_snapshot("gompertz")
  m <- make_mock_mrmfit("gompertz")
  mrms <- list(streaming = snap, search = m)
  # Passing the class/named-list validation -- the error that fires below is
  # from `method = "invalid"`, not "must be a named list of mrmfit objects",
  # confirming the snapshot's class satisfies that check. Its field contract
  # satisfying opt_mix()'s downstream reads is verified end-to-end against a
  # real fit; see test-mrm_tv_snapshot.R's header comment.
  expect_true(all(vapply(mrms, inherits, logical(1), "mrmfit")))
  expect_error(opt_mix(mrms, method = "invalid"), "should be one of")
})


# =========================================================================
# hlpr_auto_constraints
# =========================================================================

test_that("hlpr_auto_constraints returns correct structure", {
  m1 <- make_mock_mrmfit("gompertz")
  m2 <- make_mock_mrmfit("weibull")
  mrms <- list(search = m1, display = m2)

  constr <- hlpr_auto_constraints(mrms, weekly_budget = 500000)

  expect_type(constr, "list")
  expect_length(constr$lb, 2)
  expect_length(constr$ub, 2)
  expect_length(constr$x0, 2)
})

test_that("hlpr_auto_constraints bounds are feasible", {
  m1 <- make_mock_mrmfit("gompertz")
  mrms <- list(ch = m1)

  constr <- hlpr_auto_constraints(mrms, weekly_budget = 500000)

  expect_true(all(constr$lb >= 0))
  expect_true(all(constr$lb <= constr$x0))
  expect_true(all(constr$x0 <= constr$ub))
})


# =========================================================================
# hlpr_parse_constraints
# =========================================================================

test_that("hlpr_parse_constraints errors on missing columns", {
  bad_df <- data.frame(channel = "a", min_spend = 100)
  expect_error(
    hlpr_parse_constraints(bad_df, "a", 1000),
    "max_spend"
  )
})

test_that("hlpr_parse_constraints errors on missing channels", {
  df <- data.frame(channel = "a", min_spend = 100, max_spend = 1000)
  expect_error(
    hlpr_parse_constraints(df, c("a", "b"), 1000),
    "No constraints found"
  )
})

test_that("hlpr_parse_constraints reorders to match model order", {
  df <- data.frame(
    channel = c("b", "a"),
    min_spend = c(200, 100),
    max_spend = c(2000, 1000)
  )
  constr <- hlpr_parse_constraints(df, c("a", "b"), 1000)

  expect_equal(constr$lb, c(100, 200))
  expect_equal(constr$ub, c(1000, 2000))
})

test_that("share-based constraints tighten bounds", {
  df <- data.frame(
    channel = c("a", "b"),
    min_spend = c(0, 0),
    max_spend = c(10000, 10000),
    min_share = c(0.3, 0.2),
    max_share = c(0.6, 0.5)
  )
  budget <- 10000

  constr <- hlpr_parse_constraints(df, c("a", "b"), budget)

  # min_share * budget = 3000, 2000
  expect_equal(constr$lb, c(3000, 2000))
  # max_share * budget = 6000, 5000 (tighter than 10000)
  expect_equal(constr$ub, c(6000, 5000))
})

test_that("share-based constraints error when min_share > 1", {
  df <- data.frame(
    channel = "a", min_spend = 0, max_spend = 10000, min_share = 1.5
  )
  expect_error(hlpr_parse_constraints(df, "a", 10000), "between 0 and 1")
})

test_that("share-based constraints error when sum of min_share > 1", {
  df <- data.frame(
    channel = c("a", "b"),
    min_spend = c(0, 0),
    max_spend = c(10000, 10000),
    min_share = c(0.6, 0.5)
  )
  expect_error(hlpr_parse_constraints(df, c("a", "b"), 10000), "infeasible")
})

test_that("share-based constraints error when min_share > max_share", {
  df <- data.frame(
    channel = "a", min_spend = 0, max_spend = 10000,
    min_share = 0.7, max_share = 0.3
  )
  expect_error(hlpr_parse_constraints(df, "a", 10000), "min_share must be <= max_share")
})

test_that("fixed channels set lb = ub = min_spend", {
  df <- data.frame(
    channel = c("a", "b"),
    min_spend = c(5000, 0),
    max_spend = c(10000, 8000),
    fixed = c(TRUE, FALSE)
  )
  constr <- hlpr_parse_constraints(df, c("a", "b"), 10000)

  expect_equal(constr$lb[1], 5000)
  expect_equal(constr$ub[1], 5000)
  expect_equal(constr$x0[1], 5000)
  # Non-fixed channel unchanged
  expect_equal(constr$lb[2], 0)
  expect_equal(constr$ub[2], 8000)
})
