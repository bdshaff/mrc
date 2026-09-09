# hlpr_tv_identifiability() -- fully deterministic, no MCMC.

test_that("flags a narrow spend ratio at the single-parameter threshold", {
  spend <- c(rep(100, 50), rep(105, 50))  # ratio ~1.05
  r <- hlpr_tv_identifiability(spend, varying = "e")
  expect_true(r$flag)
  expect_equal(r$threshold_used, 3)
  expect_false(r$risky_request)
})

test_that("does not flag a wide spend ratio", {
  spend <- c(rep(100, 50), rep(10000, 50))  # ratio ~100
  r <- hlpr_tv_identifiability(spend, varying = "e")
  expect_false(r$flag)
})

test_that("risky_request is TRUE when 'b' is in varying", {
  spend <- runif(100, 100, 500)
  r <- hlpr_tv_identifiability(spend, varying = "b")
  expect_true(r$risky_request)
  expect_equal(r$threshold_used, 8)
})

test_that("risky_request is TRUE when more than one parameter varies", {
  spend <- runif(100, 100, 500)
  r <- hlpr_tv_identifiability(spend, varying = c("d", "e"))
  expect_true(r$risky_request)
  expect_equal(r$threshold_used, 8)
})

test_that("risky_request is FALSE for a single non-b parameter", {
  spend <- runif(100, 100, 500)
  r <- hlpr_tv_identifiability(spend, varying = "d")
  expect_false(r$risky_request)
  expect_equal(r$threshold_used, 3)
})

test_that("the stricter multi threshold actually applies (flags where the single one would not)", {
  spend <- c(rep(100, 50), rep(500, 50))  # ratio = 5: passes threshold 3, fails threshold 8
  single <- hlpr_tv_identifiability(spend, varying = "d")
  multi  <- hlpr_tv_identifiability(spend, varying = "b")
  expect_false(single$flag)
  expect_true(multi$flag)
})

test_that("min_spend_ratio = 0 disables the single-parameter threshold", {
  spend <- c(rep(100, 50), rep(105, 50))
  r <- hlpr_tv_identifiability(spend, varying = "e", min_spend_ratio = 0)
  expect_false(r$flag)
})

test_that("min_spend_ratio_multi = 0 disables the multi-parameter threshold", {
  spend <- c(rep(100, 50), rep(105, 50))
  r <- hlpr_tv_identifiability(spend, varying = "b", min_spend_ratio_multi = 0)
  expect_false(r$flag)
})

test_that("a zero or negative 5th-percentile spend does not flag (ratio undefined, not risky)", {
  spend <- c(rep(0, 10), runif(90, 100, 1000))
  r <- hlpr_tv_identifiability(spend, varying = "e")
  expect_false(r$flag)
  expect_true(is.infinite(r$spend_ratio))
})
