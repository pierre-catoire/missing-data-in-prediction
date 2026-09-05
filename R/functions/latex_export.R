################################################################################
## LaTeX/pgfplots-ready CSV export for the validation analysis
##
## Mirrors the existing convention already used for the main analysis
## (performance_metrics.R::write_results_tables(), consumed by
## latex/figures/figure_main.tex via pgfplots' `table[col sep=comma] {...}`):
## one "points" CSV (raw, one row per grid point) and one "loess" CSV
## (smoothed onto a common grid via loess_smooth_series()) per scenario,
## with short, punctuation-free lowercase column names (pgfplots-safe).
################################################################################

#' Write per-scenario points/loess CSVs for the six theoretical risks
#'
#' Produces, for each scenario, \code{<scenario>_theoretical_points.csv} and
#' \code{<scenario>_theoretical_loess.csv} under \code{out_dir}, with columns
#' \code{observedmissingness}, \code{riskmuop}, \code{riskmucp},
#' \code{riskmufull}, \code{riskmcop}, \code{riskmccp}, \code{riskmcfull}.
#'
#' @param theoretical_df Data frame as accumulated by the driver script from
#'   repeated calls to \code{compute_theoretical_risks()}: one row per
#'   (scenario, missingness_target) with columns \code{scenario},
#'   \code{missingness_proportion}, and the six \code{risk_*} columns.
#' @param out_dir Character scalar. Directory to write the CSVs into
#'   (typically the "tables" output directory).
#' @param loess_span,loess_trim,n_loess Passed to \code{loess_smooth_series()}.
#'
#' @return Invisibly returns \code{TRUE}.
write_theoretical_risks_latex_tables = function(theoretical_df, out_dir,
                                                loess_span = 0.5,
                                                loess_trim = c(0.02, 0.98),
                                                n_loess = 700) {
  check_data_frame_path(out_dir)

  risk_cols = c("risk_mu_op", "risk_mu_cp", "risk_mu_full",
               "risk_mc_op", "risk_mc_cp", "risk_mc_full")
  out_names = c("riskmuop", "riskmucp", "riskmufull",
               "riskmcop", "riskmccp", "riskmcfull")

  for (sc in unique(theoretical_df$scenario)) {
    df_sc = theoretical_df[theoretical_df$scenario == sc, ]
    df_sc = df_sc[order(df_sc$missingness_proportion), ]

    points_df = data.frame(observedmissingness = df_sc$missingness_proportion)
    for (i in seq_along(risk_cols)) points_df[[out_names[i]]] = df_sc[[risk_cols[i]]]

    write.csv(points_df,
             file.path(out_dir, sprintf("%s_theoretical_points.csv", sc)),
             row.names = FALSE)

    xloess = seq(min(df_sc$missingness_proportion, na.rm = TRUE),
                max(df_sc$missingness_proportion, na.rm = TRUE),
                length.out = n_loess)
    loess_df = data.frame(observedmissingness = xloess)
    for (i in seq_along(risk_cols)) {
      loess_df[[out_names[i]]] = loess_smooth_series(
        df_sc$missingness_proportion, df_sc[[risk_cols[i]]], xloess,
        span = loess_span, trim = loess_trim
      )
    }

    write.csv(loess_df,
             file.path(out_dir, sprintf("%s_theoretical_loess.csv", sc)),
             row.names = FALSE)
  }

  invisible(TRUE)
}

#' Write per-scenario points/loess CSVs for the empirical validation analysis
#'
#' Produces, for each scenario, \code{<scenario>_<family>_points.csv} and
#' \code{<scenario>_<family>_loess.csv} under \code{out_dir}. The points file
#' has columns \code{observedmissingness}, \code{riskpoolingoptimal},
#' \code{predictionspoolingoptimal}, \code{riskpoolingestimated},
#' \code{predictionspoolingestimated}, \code{ccvalfitted},
#' \code{ccvaloptimal}. The loess file has the same (smoothed) columns plus
#' the three theoretical reference curves for that family, smoothed onto
#' the same grid: \code{riskop}, \code{riskcp}, \code{riskfull} -- so a
#' pgfplots figure can read everything it needs (the empirical series and
#' their theoretical targets) from a single file.
#'
#' \code{ccvalfitted} is complete-case validation of the actually fitted
#' (trained-via-MI) model; \code{ccvaloptimal} is complete-case validation
#' of the true oracle predictor (\eqn{E[Y\mid X]} for MU,
#' \eqn{E[Y\mid X,M=0]} for MC) -- see \code{compute_ccval()}. The two
#' coincide only when the training-time imputation model is itself
#' consistent (e.g. they diverge under M3/M4-style self-dependent
#' missingness).
#'
#' @param theoretical_df As in \code{write_theoretical_risks_latex_tables()}.
#' @param empirical_df Data frame as accumulated from repeated calls to
#'   \code{validate_one_point()} for the given \code{family}.
#' @param family One of \code{"mu"} or \code{"mc"}.
#' @param out_dir Character scalar. Output directory.
#' @param loess_span,loess_trim,n_loess Passed to \code{loess_smooth_series()}.
#'
#' @return Invisibly returns \code{TRUE}.
write_validation_consistency_latex_tables = function(theoretical_df, empirical_df,
                                                      family = c("mu", "mc"),
                                                      out_dir,
                                                      loess_span = 0.5,
                                                      loess_trim = c(0.02, 0.98),
                                                      n_loess = 700) {
  family = match.arg(family)
  check_data_frame_path(out_dir)

  emp_cols = c("riskPooling_optimal", "predictionsPooling_optimal",
              "riskPooling_estimated", "predictionsPooling_estimated",
              "ccval_fitted", "ccval_optimal")
  emp_out_names = c("riskpoolingoptimal", "predictionspoolingoptimal",
                    "riskpoolingestimated", "predictionspoolingestimated",
                    "ccvalfitted", "ccvaloptimal")

  theo_cols = paste0("risk_", family, "_", c("op", "cp", "full"))
  theo_out_names = c("riskop", "riskcp", "riskfull")

  for (sc in unique(empirical_df$scenario)) {
    emp_sc = empirical_df[empirical_df$scenario == sc, ]
    emp_sc = emp_sc[order(emp_sc$missingness_proportion), ]

    theo_sc = theoretical_df[theoretical_df$scenario == sc, ]
    theo_sc = theo_sc[order(theo_sc$missingness_proportion), ]

    points_df = data.frame(observedmissingness = emp_sc$missingness_proportion)
    for (i in seq_along(emp_cols)) points_df[[emp_out_names[i]]] = emp_sc[[emp_cols[i]]]

    write.csv(points_df,
             file.path(out_dir, sprintf("%s_%s_points.csv", sc, family)),
             row.names = FALSE)

    xloess = seq(min(emp_sc$missingness_proportion, na.rm = TRUE),
                max(emp_sc$missingness_proportion, na.rm = TRUE),
                length.out = n_loess)
    loess_df = data.frame(observedmissingness = xloess)

    for (i in seq_along(emp_cols)) {
      loess_df[[emp_out_names[i]]] = loess_smooth_series(
        emp_sc$missingness_proportion, emp_sc[[emp_cols[i]]], xloess,
        span = loess_span, trim = loess_trim
      )
    }
    for (i in seq_along(theo_cols)) {
      loess_df[[theo_out_names[i]]] = loess_smooth_series(
        theo_sc$missingness_proportion, theo_sc[[theo_cols[i]]], xloess,
        span = loess_span, trim = loess_trim
      )
    }

    write.csv(loess_df,
             file.path(out_dir, sprintf("%s_%s_loess.csv", sc, family)),
             row.names = FALSE)
  }

  invisible(TRUE)
}

#' Write per-scenario points/loess CSVs for the "with outcome" imputation variant
#'
#' Companion to \code{write_validation_consistency_latex_tables()}: same
#' file/column-naming convention and same theoretical reference curves, but
#' for the \code{_withY} risk-/predictions-pooling series -- the
#' (illustrative, non-deployment-realistic) variant in which the
#' validation-time imputation model, both optimal and estimated, is also
#' given the true outcome Y (see \code{build_imputed_test_sets(...,
#' include_outcome = TRUE)}).
#'
#' Produces, for each scenario, \code{<scenario>_<family>_withy_points.csv}
#' and \code{<scenario>_<family>_withy_loess.csv} under \code{out_dir}. The
#' points file has columns \code{observedmissingness},
#' \code{riskpoolingoptimalwithy}, \code{predictionspoolingoptimalwithy},
#' \code{riskpoolingestimatedwithy}, \code{predictionspoolingestimatedwithy}.
#' The loess file adds the same three theoretical reference columns as
#' \code{write_validation_consistency_latex_tables()} (\code{riskop},
#' \code{riskcp}, \code{riskfull}), so the two figures (with/without Y in
#' the imputation model) can be built with an identical pgfplots template,
#' just pointed at the \code{_withy_} files instead.
#'
#' @inheritParams write_validation_consistency_latex_tables
#' @param empirical_df Data frame as accumulated from repeated calls to
#'   \code{validate_one_point()} for the given \code{family}, with
#'   \code{include_outcome_variant = TRUE} (so the \code{_withY} columns
#'   are populated rather than \code{NA}).
#'
#' @return Invisibly returns \code{TRUE}.
write_validation_consistency_withY_latex_tables = function(theoretical_df, empirical_df,
                                                            family = c("mu", "mc"),
                                                            out_dir,
                                                            loess_span = 0.5,
                                                            loess_trim = c(0.02, 0.98),
                                                            n_loess = 700) {
  family = match.arg(family)
  check_data_frame_path(out_dir)

  emp_cols = c("riskPooling_optimal_withY", "predictionsPooling_optimal_withY",
              "riskPooling_estimated_withY", "predictionsPooling_estimated_withY")
  emp_out_names = c("riskpoolingoptimalwithy", "predictionspoolingoptimalwithy",
                    "riskpoolingestimatedwithy", "predictionspoolingestimatedwithy")

  theo_cols = paste0("risk_", family, "_", c("op", "cp", "full"))
  theo_out_names = c("riskop", "riskcp", "riskfull")

  for (sc in unique(empirical_df$scenario)) {
    emp_sc = empirical_df[empirical_df$scenario == sc, ]
    emp_sc = emp_sc[order(emp_sc$missingness_proportion), ]

    theo_sc = theoretical_df[theoretical_df$scenario == sc, ]
    theo_sc = theo_sc[order(theo_sc$missingness_proportion), ]

    points_df = data.frame(observedmissingness = emp_sc$missingness_proportion)
    for (i in seq_along(emp_cols)) points_df[[emp_out_names[i]]] = emp_sc[[emp_cols[i]]]

    write.csv(points_df,
             file.path(out_dir, sprintf("%s_%s_withy_points.csv", sc, family)),
             row.names = FALSE)

    xloess = seq(min(emp_sc$missingness_proportion, na.rm = TRUE),
                max(emp_sc$missingness_proportion, na.rm = TRUE),
                length.out = n_loess)
    loess_df = data.frame(observedmissingness = xloess)

    for (i in seq_along(emp_cols)) {
      loess_df[[emp_out_names[i]]] = loess_smooth_series(
        emp_sc$missingness_proportion, emp_sc[[emp_cols[i]]], xloess,
        span = loess_span, trim = loess_trim
      )
    }
    for (i in seq_along(theo_cols)) {
      loess_df[[theo_out_names[i]]] = loess_smooth_series(
        theo_sc$missingness_proportion, theo_sc[[theo_cols[i]]], xloess,
        span = loess_span, trim = loess_trim
      )
    }

    write.csv(loess_df,
             file.path(out_dir, sprintf("%s_%s_withy_loess.csv", sc, family)),
             row.names = FALSE)
  }

  invisible(TRUE)
}
