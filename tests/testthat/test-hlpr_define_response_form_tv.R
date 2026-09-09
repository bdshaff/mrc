# hlpr_define_response_form_tv() -- bf() just builds an object, no sampling,
# so this is fully deterministic and fast.

test_that("varying parameter gets a spline term, others get ~ 1", {
  f <- hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t", varying = "d")
  forms <- f$pforms
  expect_match(deparse(forms$d), "s\\(t, k = 10\\)")
  expect_match(deparse(forms$b), "~ 1")
  expect_match(deparse(forms$c), "~ 1")
  expect_match(deparse(forms$e), "~ 1")
})

test_that("method = 'gp_approx' emits gp(t, k=, c=5/4)", {
  f <- hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t",
                                    varying = "e", method = "gp_approx", k = 15)
  expect_match(deparse(f$pforms$e), "gp\\(t, k = 15, c = 5/4\\)")
})

test_that("method = 'gp' emits exact gp(t) with no k", {
  f <- hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t",
                                    varying = "e", method = "gp")
  expect_match(deparse(f$pforms$e), "gp\\(t\\)")
  expect_no_match(deparse(f$pforms$e), "k =")
})

test_that("multiple varying parameters each get the time term", {
  f <- hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t", varying = c("d", "e"))
  expect_match(deparse(f$pforms$d), "s\\(t")
  expect_match(deparse(f$pforms$e), "s\\(t")
  expect_match(deparse(f$pforms$b), "~ 1")
})

test_that("log-form types reparameterize e as le, unconditionally", {
  # e NOT in varying -- le reparam still applies, matching hier's behavior.
  f <- hlpr_define_response_form_tv("log_logistic", "spend", "kpi", t = "t", varying = "d")
  expect_true("le" %in% names(f$pforms))
  expect_false("e" %in% names(f$pforms))
  expect_match(deparse(f$pforms$le), "~ 1")
})

test_that("log-form types with e varying put the smooth term on le", {
  f <- hlpr_define_response_form_tv("log_logistic", "spend", "kpi", t = "t", varying = "e")
  expect_match(deparse(f$pforms$le), "s\\(t")
})

test_that("non-log forms use e directly, no le", {
  f <- hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t", varying = "e")
  expect_true("e" %in% names(f$pforms))
  expect_false("le" %in% names(f$pforms))
})

test_that("errors when x or y is NULL", {
  expect_error(hlpr_define_response_form_tv("gompertz", NULL, "kpi", t = "t", varying = "e"),
              "cannot be NULL")
  expect_error(hlpr_define_response_form_tv("gompertz", "spend", NULL, t = "t", varying = "e"),
              "cannot be NULL")
})

test_that("errors when t is NULL", {
  expect_error(hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = NULL, varying = "e"),
              "'t'")
})

test_that("errors when varying is empty", {
  expect_error(hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t", varying = character(0)),
              "at least one parameter")
})

test_that("errors on an unknown method", {
  expect_error(
    hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t", varying = "e", method = "bogus"),
    "Unknown `method`"
  )
})

test_that("nl = TRUE is set on the returned formula", {
  f <- hlpr_define_response_form_tv("gompertz", "spend", "kpi", t = "t", varying = "e")
  expect_true(f$formula |> attr("nl"))
})
