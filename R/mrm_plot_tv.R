#' Plot a time-varying response curve fit
#'
#' Visualizes how a fitted \code{\link{fit_response_tv}} model's response
#' curve evolves over time, in two complementary forms.
#'
#' @param mrm An \code{mrmfit_tv} object returned by
#'   \code{\link{fit_response_tv}}.
#' @param type One of \code{"evolution"} (default) -- a two-panel view of the
#'   fitted curve \code{f(spend, t)} itself -- or \code{"trajectory"} -- the
#'   simpler view of one or more curve parameters plotted against time.
#' @param param For \code{type = "trajectory"}: \code{"all"} (default,
#'   faceted) or one of \code{"b"}, \code{"c"}, \code{"d"}, \code{"e"}.
#' @param n_snapshots Number of discrete curve snapshots to overlay in the
#'   \code{"evolution"} view's second panel. Default \code{5}.
#' @param interval \code{"credible"} (default) draws the 95% credible ribbon;
#'   \code{"none"} omits it.
#' @return A \code{ggplot} object for a single \code{type = "trajectory"}
#'   parameter, or a \code{patchwork} object otherwise.
#'
#' @details
#' The \code{"evolution"} view combines a continuous heatmap of the fitted
#' curve's surface -- **time on the x-axis, spend on the y-axis**, reading
#' left-to-right like an ordinary time series -- with a handful of discrete
#' curve snapshots at evenly spaced dates, overlaid on shared axes (spend on
#' x, KPI on y -- the standard response-curve convention used throughout this
#' package, e.g. \code{\link{opt_plot_curves}}) and colored by date. Together
#' they show both the continuous evolution and precisely comparable
#' before/after curve shapes in one static figure.
#'
#' Every fit shown here carries the same identifiability/convergence caveats
#' documented in \code{\link{fit_response_tv}} -- check \code{mrm$diagnostics}
#' before treating any trajectory as more than exploratory when it is
#' flagged.
#'
#' @seealso \code{\link{fit_response_tv}}, \code{\link{mrm_tv_snapshot}}
#' @import ggplot2
#' @importFrom rlang .data
#' @export

mrm_plot_tv <- function(mrm, type = c("evolution", "trajectory"),
                        param = c("all", "b", "c", "d", "e"),
                        n_snapshots = 5, interval = c("credible", "none")) {

  if (!inherits(mrm, "mrmfit_tv")) {
    stop("mrm must be a fitted model object created by fit_response_tv()", call. = FALSE)
  }
  type <- match.arg(type)
  param <- match.arg(param)
  interval <- match.arg(interval)

  if (type == "trajectory") {
    return(mrm_plot_tv_trajectory(mrm, param = param, interval = interval))
  }
  mrm_plot_tv_evolution(mrm, n_snapshots = n_snapshots)
}


#' @rdname mrm_plot_tv
#' @param x An \code{mrmfit_tv} object (identical to \code{mrm}; \code{plot()}
#'   dispatches through this argument name by S3 convention).
#' @param ... Passed through to \code{mrm_plot_tv()}.
#' @export
plot.mrmfit_tv <- function(x, ...) {
  mrm_plot_tv(x, ...)
}


# --- type = "trajectory" ---------------------------------------------------

mrm_plot_tv_trajectory <- function(mrm, param = "all", interval = "credible") {

  pal <- mrmopt_palette()
  labels <- list(b = "Growth Rate (b)", c = "Floor (c)",
                 d = "Ceiling (d)", e = "Midpoint (e)")
  y_scale_for <- function(p) {
    switch(p,
      b = ggplot2::scale_y_continuous(labels = scales::label_scientific()),
      e = ggplot2::scale_y_continuous(labels = scales::dollar_format()),
      ggplot2::scale_y_continuous(labels = scales::comma)
    )
  }

  panel_for <- function(p) {
    d <- mrm$trajectory[mrm$trajectory$param == p, ]
    d <- d[order(d$date), ]
    g <- ggplot2::ggplot(d, ggplot2::aes(x = .data$date, y = .data$center))
    if (interval == "credible") {
      g <- g + ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        fill = pal[["ci_band"]], alpha = 0.5, colour = NA)
    }
    g + ggplot2::geom_line(colour = pal[["response"]], linewidth = 0.8) +
      ggplot2::geom_point(colour = pal[["response"]], size = 1.4) +
      ggplot2::scale_x_date(date_labels = "%b '%y") +
      y_scale_for(p) +
      ggplot2::labs(x = NULL, y = NULL, title = labels[[p]]) +
      ggplot2::theme_minimal()
  }

  if (param != "all") {
    return(panel_for(param))
  }

  panels <- lapply(mrm$varying, panel_for)
  patchwork::wrap_plots(panels, ncol = min(2, length(panels))) +
    patchwork::plot_annotation(
      title = paste0(mrm$spend_col, " \u2014 ", mrm$rc_type, " (time-varying)"),
      subtitle = paste0("method = ", mrm$method, " | varying: ",
                        paste(mrm$varying, collapse = ", ")),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 14))
    )
}


# --- type = "evolution" -----------------------------------------------------

mrm_plot_tv_evolution <- function(mrm, n_snapshots = 5, n_x = 80, n_t = 60) {

  sv <- mrm$scale_values
  x_col <- names(mrm$data)[1]
  y_col <- names(mrm$data)[2]

  x_range_s <- range(mrm$data[[x_col]])
  grid <- tidyr::expand_grid(
    xs = seq(x_range_s[1], x_range_s[2], length.out = n_x),
    t  = seq(0, 1, length.out = n_t)
  )
  names(grid)[1] <- x_col
  grid[[y_col]] <- 0
  grid$date <- mrm$date_range[1] + grid$t * as.numeric(mrm$date_range[2] - mrm$date_range[1])

  ep <- brms::posterior_epred(mrm, newdata = grid)
  grid$center_s <- apply(ep, 2, stats::median)

  x_range <- if (!is.null(sv$x_min) && !is.null(sv$x_max)) sv$x_max - sv$x_min else 1
  y_range <- if (!is.null(sv$y_min) && !is.null(sv$y_max)) sv$y_max - sv$y_min else 1
  grid$spend_orig <- grid[[x_col]] * x_range + (sv$x_min %||% 0) - (sv$x_offset %||% 0)
  grid$kpi_orig   <- grid$center_s * y_range + (sv$y_min %||% 0)

  # Heatmap panel: TIME on x, SPEND on y (reads left-to-right like a time
  # series) -- flipped from spend-on-x per user preference during development.
  p_heat <- ggplot2::ggplot(grid, ggplot2::aes(x = .data$date, y = .data$spend_orig,
                                               fill = .data$kpi_orig)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::geom_contour(ggplot2::aes(z = .data$kpi_orig), colour = "white",
                          alpha = 0.35, linewidth = 0.3) +
    ggplot2::scale_fill_viridis_c(labels = scales::comma, name = "Predicted\nKPI") +
    ggplot2::scale_x_date(date_labels = "%b '%y") +
    ggplot2::scale_y_continuous(labels = scales::dollar_format()) +
    ggplot2::labs(title = "Continuous evolution", x = NULL, y = "Weekly spend") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "right")

  # Snapshot panel: spend on x, KPI on y -- the standard response-curve
  # convention (unchanged; this genuinely is a response curve).
  snap_dates <- seq(mrm$date_range[1], mrm$date_range[2], length.out = n_snapshots)
  snaps <- purrr::map_dfr(snap_dates, function(sd) {
    tt <- as.numeric(sd - mrm$date_range[1]) / as.numeric(mrm$date_range[2] - mrm$date_range[1])
    t_grid <- unique(grid$t)
    nearest_t <- t_grid[which.min(abs(t_grid - tt))]
    grid[grid$t == nearest_t, ] |> transform(snapshot_date = sd)
  })

  p_snap <- ggplot2::ggplot(snaps, ggplot2::aes(x = .data$spend_orig, y = .data$kpi_orig,
                                                colour = .data$snapshot_date,
                                                group = .data$snapshot_date)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_colour_viridis_c(trans = "date",
                                    labels = scales::date_format("%b '%y"), name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(title = paste0(n_snapshots, " snapshots"),
                 x = "Weekly spend", y = "Predicted KPI") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "right")

  patchwork::wrap_plots(p_heat, p_snap, nrow = 1, widths = c(1.1, 1)) +
    patchwork::plot_annotation(
      title = paste0(mrm$spend_col, " \u2014 response curve evolution (", mrm$method, ")"),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13))
    )
}
