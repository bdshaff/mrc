# mrm_tv_snapshot() -- validation-only, no MCMC.
#
# The construction path itself (posterior_linpred() at a target date,
# posterior::as_draws_df(), predict()/fitted() via hlpr_infer_tv_snapshot())
# genuinely needs a real posterior on all four of those brms/posterior
# generics to exercise meaningfully; mocking all four risks its own subtle
# bugs (e.g. predict()'s column/Est.Error format) without adding confidence
# beyond what real-data testing already gives. That path was verified
# end-to-end against a real fit_response_tv() fit during development:
# print(), mrm_summary_tv(), both mrm_plot_tv() types, mrm_tv_snapshot()'s
# full field contract, and opt_mix() with both method = "point" and
# method = "posterior" (confirming the .snapshot_draws / as_draws_df bridge),
# plus confirming opt_mix() correctly rejects an un-snapshotted mrmfit_tv.

test_that("errors when mrm is not a mrmfit_tv object", {
  expect_error(mrm_tv_snapshot(list(a = 1)), "fit_response_tv")
})

test_that("errors on an invalid `at` via match.arg", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  expect_error(mrm_tv_snapshot(mock, at = "bogus"), "should be one of")
})
