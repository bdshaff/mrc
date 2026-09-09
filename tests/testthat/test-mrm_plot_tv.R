# mrm_plot_tv() -- exercised against make_mock_mrmfit_tv(), no MCMC.

test_that("type = 'trajectory', param = 'all' returns a patchwork for multiple varying params", {
  mock <- make_mock_mrmfit_tv(varying = c("d", "e"))
  p <- mrm_plot_tv(mock, type = "trajectory")
  expect_s3_class(p, "patchwork")
})

test_that("type = 'trajectory' with a single varying param and param = 'all' returns one ggplot", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p <- mrm_plot_tv(mock, type = "trajectory")
  expect_s3_class(p, "patchwork")
})

test_that("type = 'trajectory', param = <specific> returns a single ggplot", {
  mock <- make_mock_mrmfit_tv(varying = c("d", "e"))
  p <- mrm_plot_tv(mock, type = "trajectory", param = "e")
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("trajectory plot has a ribbon by default and drops it when interval = 'none'", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p_ribbon <- mrm_plot_tv(mock, type = "trajectory", param = "e", interval = "credible")
  p_none   <- mrm_plot_tv(mock, type = "trajectory", param = "e", interval = "none")
  geoms_ribbon <- vapply(p_ribbon$layers, function(l) class(l$geom)[1], character(1))
  geoms_none   <- vapply(p_none$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% geoms_ribbon)
  expect_false("GeomRibbon" %in% geoms_none)
})

test_that("trajectory plot x-axis is a date scale", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p <- mrm_plot_tv(mock, type = "trajectory", param = "e")
  b <- ggplot2::ggplot_build(p)
  expect_true(inherits(b$plot$scales$get_scales("x"), "ScaleContinuousDate"))
})

test_that("type = 'evolution' returns a patchwork built from GeomRaster + GeomContour + GeomLine", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p <- mrm_plot_tv(mock, type = "evolution")
  expect_s3_class(p, "patchwork")
})

test_that("evolution heatmap panel: TIME on x-axis, SPEND on y-axis", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p <- mrm_plot_tv_evolution(mock)
  heat <- p[[1]]
  b <- ggplot2::ggplot_build(heat)
  expect_true(inherits(b$plot$scales$get_scales("x"), "ScaleContinuousDate"))
  x_scale <- b$plot$scales$get_scales("x")
  y_scale <- b$plot$scales$get_scales("y")
  expect_false(inherits(y_scale, "ScaleContinuousDate"))
  geoms <- vapply(heat$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRaster" %in% geoms)
  expect_true("GeomContour" %in% geoms)
})

test_that("evolution snapshot panel: spend on x, KPI on y (unchanged response-curve convention)", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p <- mrm_plot_tv_evolution(mock)
  snap <- p[[2]]
  geoms <- vapply(snap$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomLine" %in% geoms)
  b <- ggplot2::ggplot_build(snap)
  expect_false(inherits(b$plot$scales$get_scales("x"), "ScaleContinuousDate"))
})

test_that("errors when mrm is not a mrmfit_tv object", {
  expect_error(mrm_plot_tv(list(a = 1)), "fit_response_tv")
})

test_that("errors on an invalid type via match.arg", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  expect_error(mrm_plot_tv(mock, type = "bogus"), "should be one of")
})

test_that("plot.mrmfit_tv dispatches to mrm_plot_tv", {
  mock <- make_mock_mrmfit_tv(varying = "e")
  p1 <- plot(mock, type = "trajectory", param = "e")
  p2 <- mrm_plot_tv(mock, type = "trajectory", param = "e")
  expect_s3_class(p1, "ggplot")
  expect_equal(class(p1), class(p2))
})
