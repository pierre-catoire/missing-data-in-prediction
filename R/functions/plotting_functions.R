################################################################################
## Plotting functions for the validation analysis
##
## Two figures are produced:
##   plot_theoretical_risks(): one 5-panel figure (one panel per missingness
##     scenario) showing all six theoretical risks (risk_mu_op/cp/full,
##     risk_mc_op/cp/full) as a function of the missingness proportion.
##   plot_validation_consistency(): for a given family ("mu" or "mc"), a
##     5-panel figure showing the three relevant theoretical risks (op/cp/
##     full for that family) as reference lines, overlaid with the five
##     empirical validation-method series (riskPooling/predictionsPooling x
##     optimal/estimated imputation, plus CCVal), each rendered as raw
##     (semi-transparent) scatter points across the dense missingness grid
##     plus a LOESS-smoothed trend (reusing loess_smooth_series() from
##     performance_metrics.R) -- the smoothed trend is what visually carries
##     the "this validation method is/isn't consistent for this target risk"
##     claim, since each grid point is a single simulated dataset.
################################################################################

## Fixed colour/line-type scheme kept identical across both figures / calls
## so the same quantity always renders the same way.
.theoretical_style = list(
  risk_mu_op   = list(col = "#1b9e77", lty = 1),
  risk_mu_cp   = list(col = "#1b9e77", lty = 2),
  risk_mu_full = list(col = "#1b9e77", lty = 3),
  risk_mc_op   = list(col = "#d95f02", lty = 1),
  risk_mc_cp   = list(col = "#d95f02", lty = 2),
  risk_mc_full = list(col = "#d95f02", lty = 3)
)

.empirical_style = list(
  riskPooling_optimal          = list(col = "#7570b3", pch = 16),
  predictionsPooling_optimal   = list(col = "#e7298a", pch = 16),
  riskPooling_estimated        = list(col = "#7570b3", pch = 1),
  predictionsPooling_estimated = list(col = "#e7298a", pch = 1),
  ccval_fitted                 = list(col = "#666666", pch = 17),
  ccval_optimal                = list(col = "#a6761d", pch = 2)
)

## Same colour pairing as .empirical_style's riskPooling/predictionsPooling
## entries (purple/pink), reused for the "_withY" series so a figure can be
## compared panel-by-panel against the ordinary (Y-free) one at a glance.
.empirical_style_withY = list(
  riskPooling_optimal_withY          = list(col = "#7570b3", pch = 16),
  predictionsPooling_optimal_withY   = list(col = "#e7298a", pch = 16),
  riskPooling_estimated_withY        = list(col = "#7570b3", pch = 1),
  predictionsPooling_estimated_withY = list(col = "#e7298a", pch = 1)
)

.transparent = function(col, alpha = 0.25) {
  rgb_val = grDevices::col2rgb(col) / 255
  grDevices::rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha = alpha)
}

#' Plot the six theoretical risks for every scenario
#'
#' Produces one figure with one panel per missingness scenario, each
#' showing all six theoretical risks (\code{risk_mu_op}, \code{risk_mu_cp},
#' \code{risk_mu_full}, \code{risk_mc_op}, \code{risk_mc_cp},
#' \code{risk_mc_full}) as a function of the missingness proportion.
#'
#' @param theoretical_df Data frame with columns \code{scenario},
#'   \code{missingness_target} (or \code{missingness_proportion}), and the
#'   six \code{risk_*} columns, as accumulated by the driver script from
#'   repeated calls to \code{compute_theoretical_risks()}.
#' @param file Optional character scalar. If provided, the figure is
#'   written to this path (PNG) instead of / in addition to the current
#'   graphics device.
#' @param scenarios Character vector of scenario identifiers to plot, in
#'   panel order. Defaults to \code{missingness_scenarios} from config.R
#'   if that object exists in the calling environment, else the unique
#'   scenarios found in \code{theoretical_df}.
#'
#' @return Invisibly returns \code{TRUE}.
plot_theoretical_risks = function(theoretical_df, file = NULL, scenarios = NULL) {
  if (is.null(scenarios)) {
    scenarios = if (exists("missingness_scenarios")) missingness_scenarios else sort(unique(theoretical_df$scenario))
  }
  x_col = if ("missingness_proportion" %in% names(theoretical_df)) "missingness_proportion" else "missingness_target"

  risk_cols = c("risk_mu_op", "risk_mu_cp", "risk_mu_full",
               "risk_mc_op", "risk_mc_cp", "risk_mc_full")

  plot_fun = function() {
    op = par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    on.exit(par(op))

    for (sc in scenarios) {
      df_sc = theoretical_df[theoretical_df$scenario == sc, ]
      df_sc = df_sc[order(df_sc[[x_col]]), ]

      yr = range(unlist(df_sc[, risk_cols]), na.rm = TRUE)

      plot(df_sc[[x_col]], df_sc[[risk_cols[1]]], type = "n",
          xlab = "Missingness proportion", ylab = "Risk (MSE)",
          ylim = yr, main = sc)

      for (rc in risk_cols) {
        st = .theoretical_style[[rc]]
        lines(df_sc[[x_col]], df_sc[[rc]], col = st$col, lty = st$lty, lwd = 2)
      }
    }

    plot.new()
    legend("center",
          legend = c("MU - op", "MU - cp", "MU - full", "MC - op", "MC - cp", "MC - full"),
          col = sapply(risk_cols, function(rc) .theoretical_style[[rc]]$col),
          lty = sapply(risk_cols, function(rc) .theoretical_style[[rc]]$lty),
          lwd = 2, bty = "n", title = "Theoretical risk")
  }

  if (!is.null(file)) {
    grDevices::png(file, width = 1400, height = 900, res = 130)
    plot_fun()
    grDevices::dev.off()
  } else {
    plot_fun()
  }
  invisible(TRUE)
}

#' Plot empirical validation-method consistency against theoretical targets
#'
#' For the given \code{family} ("mu" or "mc"), produces a 5-panel figure
#' (one panel per missingness scenario) showing the three relevant
#' theoretical risks as reference lines (op/cp/full for that family), and
#' the five empirical validation-method estimates as raw scatter points
#' (semi-transparent, one point per grid point) plus a LOESS-smoothed trend
#' line each.
#'
#' @param theoretical_df Data frame as produced by repeated calls to
#'   \code{compute_theoretical_risks()} (see \code{plot_theoretical_risks()}).
#' @param empirical_df Data frame as accumulated from repeated calls to
#'   \code{validate_one_point()} for the given \code{family}: one row per
#'   (scenario, missingness_target) with columns \code{riskPooling_optimal},
#'   \code{predictionsPooling_optimal}, \code{riskPooling_estimated},
#'   \code{predictionsPooling_estimated}, \code{ccval_fitted},
#'   \code{ccval_optimal}.
#' @param family One of \code{"mu"} or \code{"mc"}.
#' @param file Optional character scalar. PNG output path.
#' @param scenarios Character vector of scenario identifiers to plot, in
#'   panel order. Defaults as in \code{plot_theoretical_risks()}.
#' @param loess_span,loess_trim,n_loess Passed to \code{loess_smooth_series()}.
#'
#' @return Invisibly returns \code{TRUE}.
plot_validation_consistency = function(theoretical_df, empirical_df,
                                       family = c("mu", "mc"),
                                       file = NULL, scenarios = NULL,
                                       loess_span = 0.5,
                                       loess_trim = c(0.02, 0.98),
                                       n_loess = 700) {
  family = match.arg(family)
  if (is.null(scenarios)) {
    scenarios = if (exists("missingness_scenarios")) missingness_scenarios else sort(unique(empirical_df$scenario))
  }

  theo_cols = paste0("risk_", family, "_", c("op", "cp", "full"))
  emp_cols = c("riskPooling_optimal", "predictionsPooling_optimal",
              "riskPooling_estimated", "predictionsPooling_estimated",
              "ccval_fitted", "ccval_optimal")

  theo_x_col = if ("missingness_proportion" %in% names(theoretical_df)) "missingness_proportion" else "missingness_target"

  plot_fun = function() {
    op = par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    on.exit(par(op))

    for (sc in scenarios) {
      theo_sc = theoretical_df[theoretical_df$scenario == sc, ]
      theo_sc = theo_sc[order(theo_sc[[theo_x_col]]), ]

      emp_sc = empirical_df[empirical_df$scenario == sc, ]
      emp_sc = emp_sc[order(emp_sc$missingness_proportion), ]

      yr = range(c(unlist(theo_sc[, theo_cols]), unlist(emp_sc[, emp_cols])), na.rm = TRUE)

      plot(theo_sc[[theo_x_col]], theo_sc[[theo_cols[1]]], type = "n",
          xlab = "Missingness proportion", ylab = "Risk (MSE)",
          ylim = yr, main = sprintf("%s (%s)", sc, toupper(family)))

      for (tc in theo_cols) {
        rc = sub(paste0("^risk_", family, "_"), "", tc)
        lty = switch(rc, op = 1, cp = 2, full = 3)
        lines(theo_sc[[theo_x_col]], theo_sc[[tc]], col = "black", lty = lty, lwd = 2)
      }

      for (ec in emp_cols) {
        st = .empirical_style[[ec]]
        points(emp_sc$missingness_proportion, emp_sc[[ec]],
              col = .transparent(st$col), pch = st$pch, cex = 0.6)

        smoothed = tryCatch(
          loess_smooth_series(emp_sc$missingness_proportion, emp_sc[[ec]],
                              xloess = theo_sc[[theo_x_col]],
                              span = loess_span, trim = loess_trim),
          error = function(e) rep(NA_real_, length(theo_sc[[theo_x_col]]))
        )
        lines(theo_sc[[theo_x_col]], smoothed, col = st$col, lwd = 2)
      }
    }

    plot.new()
    legend("center",
          legend = c(sprintf("%s theoretical: op", toupper(family)),
                    sprintf("%s theoretical: cp", toupper(family)),
                    sprintf("%s theoretical: full", toupper(family)),
                    "RiskPooling - optimal imp.",
                    "PredictionsPooling - optimal imp.",
                    "RiskPooling - estimated imp.",
                    "PredictionsPooling - estimated imp.",
                    "CCVal - fitted model",
                    "CCVal - optimal (oracle) model"),
          col = c("black", "black", "black",
                 sapply(emp_cols, function(ec) .empirical_style[[ec]]$col)),
          lty = c(1, 2, 3, NA, NA, NA, NA, NA, NA),
          pch = c(NA, NA, NA, sapply(emp_cols, function(ec) .empirical_style[[ec]]$pch)),
          lwd = 2, bty = "n", cex = 0.85)
  }

  if (!is.null(file)) {
    grDevices::png(file, width = 1400, height = 900, res = 130)
    plot_fun()
    grDevices::dev.off()
  } else {
    plot_fun()
  }
  invisible(TRUE)
}

#' Plot "with outcome" validation-time imputation vs. theoretical targets
#'
#' Companion to \code{plot_validation_consistency()}: same theoretical
#' op/cp/full reference lines for the given \code{family}, but overlaid
#' with the four \code{_withY} risk-/predictions-pooling series (optimal
#' and estimated imputation, both now allowed to use the true outcome Y at
#' validation time) instead of the ordinary, deployment-realistic ones.
#' Comparing this figure panel-by-panel against
#' \code{plot_validation_consistency()}'s isolates how much of the
#' optimal-vs-estimated imputation gap is attributable to withholding Y.
#'
#' @inheritParams plot_validation_consistency
#' @param empirical_df Data frame as accumulated from repeated calls to
#'   \code{validate_one_point()} for the given \code{family}: must contain
#'   columns \code{riskPooling_optimal_withY},
#'   \code{predictionsPooling_optimal_withY},
#'   \code{riskPooling_estimated_withY},
#'   \code{predictionsPooling_estimated_withY} (i.e.
#'   \code{include_outcome_variant = TRUE} was used when producing it).
#'
#' @return Invisibly returns \code{TRUE}.
plot_validation_consistency_withY = function(theoretical_df, empirical_df,
                                             family = c("mu", "mc"),
                                             file = NULL, scenarios = NULL,
                                             loess_span = 0.5,
                                             loess_trim = c(0.02, 0.98),
                                             n_loess = 700) {
  family = match.arg(family)
  if (is.null(scenarios)) {
    scenarios = if (exists("missingness_scenarios")) missingness_scenarios else sort(unique(empirical_df$scenario))
  }

  theo_cols = paste0("risk_", family, "_", c("op", "cp", "full"))
  emp_cols = c("riskPooling_optimal_withY", "predictionsPooling_optimal_withY",
              "riskPooling_estimated_withY", "predictionsPooling_estimated_withY")

  theo_x_col = if ("missingness_proportion" %in% names(theoretical_df)) "missingness_proportion" else "missingness_target"

  plot_fun = function() {
    op = par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    on.exit(par(op))

    for (sc in scenarios) {
      theo_sc = theoretical_df[theoretical_df$scenario == sc, ]
      theo_sc = theo_sc[order(theo_sc[[theo_x_col]]), ]

      emp_sc = empirical_df[empirical_df$scenario == sc, ]
      emp_sc = emp_sc[order(emp_sc$missingness_proportion), ]

      yr = range(c(unlist(theo_sc[, theo_cols]), unlist(emp_sc[, emp_cols])), na.rm = TRUE)

      plot(theo_sc[[theo_x_col]], theo_sc[[theo_cols[1]]], type = "n",
          xlab = "Missingness proportion", ylab = "Risk (MSE)",
          ylim = yr, main = sprintf("%s (%s, imputation incl. Y)", sc, toupper(family)))

      for (tc in theo_cols) {
        rc = sub(paste0("^risk_", family, "_"), "", tc)
        lty = switch(rc, op = 1, cp = 2, full = 3)
        lines(theo_sc[[theo_x_col]], theo_sc[[tc]], col = "black", lty = lty, lwd = 2)
      }

      for (ec in emp_cols) {
        st = .empirical_style_withY[[ec]]
        points(emp_sc$missingness_proportion, emp_sc[[ec]],
              col = .transparent(st$col), pch = st$pch, cex = 0.6)

        smoothed = tryCatch(
          loess_smooth_series(emp_sc$missingness_proportion, emp_sc[[ec]],
                              xloess = theo_sc[[theo_x_col]],
                              span = loess_span, trim = loess_trim),
          error = function(e) rep(NA_real_, length(theo_sc[[theo_x_col]]))
        )
        lines(theo_sc[[theo_x_col]], smoothed, col = st$col, lwd = 2)
      }
    }

    plot.new()
    legend("center",
          legend = c(sprintf("%s theoretical: op", toupper(family)),
                    sprintf("%s theoretical: cp", toupper(family)),
                    sprintf("%s theoretical: full", toupper(family)),
                    "RiskPooling - optimal imp. (+Y)",
                    "PredictionsPooling - optimal imp. (+Y)",
                    "RiskPooling - estimated imp. (+Y)",
                    "PredictionsPooling - estimated imp. (+Y)"),
          col = c("black", "black", "black",
                 sapply(emp_cols, function(ec) .empirical_style_withY[[ec]]$col)),
          lty = c(1, 2, 3, NA, NA, NA, NA),
          pch = c(NA, NA, NA, sapply(emp_cols, function(ec) .empirical_style_withY[[ec]]$pch)),
          lwd = 2, bty = "n", cex = 0.85)
  }

  if (!is.null(file)) {
    grDevices::png(file, width = 1400, height = 900, res = 130)
    plot_fun()
    grDevices::dev.off()
  } else {
    plot_fun()
  }
  invisible(TRUE)
}
