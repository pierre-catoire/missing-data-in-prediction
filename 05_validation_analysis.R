################################################################################
## 05. Validation analysis
##
## Fresh (not based on 04_validation.R, which is outdated) analysis of:
##   (1) the six theoretical risks (MU/MC x op/cp/full) as a function of the
##       missingness proportion, for each of the 5 missingness scenarios;
##   (2) empirical validation-method consistency: for MU and MC forecasters
##       trained by multiple imputation, whether risk-pooling /
##       predictions-pooling validation (with optimal true-distribution or
##       estimated mice-fit imputation at validation time), and complete-
##       case validation, converge to the theoretically expected target risk
##       as the dense (701-point) missingness grid is traversed.
##
## Reads the pre-generated simulation objects under output/main/raw/ (one
## .rds per scenario x missingness_target_X1); does NOT retrain or
## regenerate any of that data.
##
## Set `run_mode` below to control the grid density / Monte Carlo precision
## trade-off:
##   "quick"  - tiny smoke test (2% of the grid, low precision): just to
##              check the pipeline runs end-to-end without error, in ~1 min.
##   "medium" - the FULL 701-point grid, at reduced Monte Carlo/SIR
##              precision, calibrated (see the comment above the settings
##              below) to take roughly 30 min. Since the whole grid is
##              covered at its real (final) density, the resulting tables
##              and figures have the same *shape* as the eventual full-
##              precision run -- just a bit noisier per point -- so they
##              are good enough to draft thesis conclusions from while the
##              full run proceeds separately.
##   "full"   - the full 701-point grid at full precision; expected to take
##              hours (this is what "quick"/"medium" let you sanity-check
##              before committing to).
## "medium" and "full" both use the whole grid and write to their own
## output directory, so they can be run at the same time (e.g. in two
## separate terminals) without interfering with each other -- though
## running both at once will make each slower, since they compete for the
## same CPU cores.
##
## CHECKPOINTING / RESUME: results are saved to disk every `checkpoint_every`
## grid points (not just once at the very end), and on startup the script
## looks for a previous (possibly interrupted) run's output in the same
## output directory and skips any (scenario, missingness_target) pair
## already computed there. This means the process can safely be killed
## (accidentally, or on purpose, e.g. to change a setting) and restarted
## with the same script: at most `checkpoint_every` points of work are
## lost, not the whole run. Each `run_mode` writes to its own output
## directory precisely so different precision levels can never be mistaken
## for each other, or silently merged into the same checkpoints.
################################################################################

source("R/functions/utils.R")
source("R/functions/logging_utils.R")
source("R/functions/training_procedures.R")
source("R/functions/reference_probabilities.R")
source("R/functions/performance_metrics.R")
source("R/functions/validation_functions.R")
source("R/functions/validation_analysis.R")
source("R/functions/plotting_functions.R")
source("R/functions/latex_export.R")
source("R/config/config.R")

library(mice)

## =============================================================================
## 0. Run-mode toggle
## =============================================================================

run_mode = "full"  # one of "quick", "medium", "full"

## If FALSE, skips the "_withY" risk-/predictions-pooling computations
## entirely (validation-time imputation, both optimal and estimated, that
## is also given the true outcome Y -- illustrative only, never deployment-
## realistic). This roughly doubles the imputation work per grid point, so
## set to FALSE for a faster run if you only need the ordinary (Y-free)
## comparison and the fitted-vs-optimal CCVal split.
include_outcome_variant = TRUE

if (run_mode == "quick") {
  grid_frac      = 0.02   # fraction of the missingness grid to process
  N_theoretical  = 2000   # population size for compute_theoretical_risks()
  B_theoretical  = 300    # MC draws per unit for the MC theoretical risks
  K_sir          = 300    # SIR candidates for sample_x1_mc()
  B_sir          = 100    # MC draws per SIR candidate
  m_imputations  = 3      # number of multiple imputations

} else if (run_mode == "medium") {
  ## Calibrated from the ~8s/point observed for "full" settings on the
  ## first point of your actual run: dividing N/B/K/B_sir each by 4 cuts
  ## the two dominant cost terms (N*B for the theoretical MC integration,
  ## K*B_sir for the SIR imputation sampler) by ~16x each, targeting
  ## roughly 8s/16 = 0.5s/point, i.e. ~30 min for the full 3505-point grid.
  ## This is a rough extrapolation from a single timing data point on your
  ## machine, not a guarantee -- watch the live ETA after the first ~20-50
  ## points (it updates every 10) and kill/restart if it's tracking far
  ## from 30 min; the checkpoint/resume logic means nothing is lost by
  ## doing so.
  ##
  ## NOTE: with include_outcome_variant = TRUE (the default), each point
  ## now does roughly twice the validation-time-imputation work (the
  ## ordinary Y-free RP/PP plus the "_withY" RP/PP), plus a CCVal-optimal
  ## computation (an ey_given_x1x2_m() call restricted to the complete
  ## cases for the MC family). Expect this "medium" run to take more like
  ## 45-75 min rather than 30; set include_outcome_variant = FALSE above
  ## to recover roughly the original timing.
  grid_frac      = 1       # the whole 701-point grid, same density as "full"
  N_theoretical  = 25000
  B_theoretical  = 500
  K_sir          = 250
  B_sir          = 50
  m_imputations  = 5

} else if (run_mode == "full") {
  grid_frac      = 1
  N_theoretical  = 1e5
  B_theoretical  = 2000
  K_sir          = 1000
  B_sir          = 200
  m_imputations  = 5

} else {
  stop("`run_mode` must be one of \"quick\", \"medium\", \"full\".", call. = FALSE)
}

checkpoint_every = 20  # save progress to disk every N grid points processed

base_seed = 314159
set.seed(base_seed)

raw_dir = "output/main/raw"

## Each run_mode writes to its own output directory, so different precision
## levels can never collide with, or be mistaken for, one another.
out_dir = switch(run_mode,
                 quick  = "output/validation_analysis_quicktest",
                 medium = "output/validation_analysis_medium",
                 full   = "output/validation_analysis")
out_raw    = file.path(out_dir, "raw")
out_tables = file.path(out_dir, "tables")
out_figs   = file.path(out_dir, "figures")
dir.create(out_raw,    recursive = TRUE, showWarnings = FALSE)
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figs,   recursive = TRUE, showWarnings = FALSE)

theo_path = file.path(out_raw, "theoretical_risks.rds")
mu_path   = file.path(out_raw, "empirical_validation_mu.rds")
mc_path   = file.path(out_raw, "empirical_validation_mc.rds")

## =============================================================================
## 1. Select the grid points to process
## =============================================================================

#' Select an evenly-spaced subset of a grid
#'
#' @param grid Numeric vector, assumed sorted.
#' @param frac Numeric in (0, 1]. Fraction of points to keep.
#'
#' @return Numeric vector, a subset of \code{grid} covering its full range.
select_grid_points = function(grid, frac) {
  if (frac >= 1) return(grid)
  n_keep = max(2, round(length(grid) * frac))
  idx = unique(round(seq(1, length(grid), length.out = n_keep)))
  grid[idx]
}

selected_grid = select_grid_points(missingness_grid, grid_frac)

## =============================================================================
## 2. Resume support: load any previous (possibly interrupted) run's output
## =============================================================================

point_key = function(scenario, target) paste(scenario, sprintf("%.3f", target))

existing_theoretical = NULL
existing_mu = NULL
existing_mc = NULL
done_keys = character(0)

if (file.exists(theo_path) && file.exists(mu_path) && file.exists(mc_path)) {
  existing_theoretical = readRDS(theo_path)
  existing_mu = readRDS(mu_path)
  existing_mc = readRDS(mc_path)

  ## Schema guard: validate_one_point() now returns extra columns (ccval
  ## split into ccval_fitted/ccval_optimal, plus the four "_withY" RP/PP
  ## columns). A checkpoint written by the previous version of this script
  ## is missing them, and rbind()-ing it with new rows would either error
  ## opaquely or (worse) silently misalign columns. Fail loudly instead and
  ## tell the user how to proceed, rather than guessing.
  expected_mu_cols = c("scenario", "missingness_target", "missingness_proportion", "family",
                       "riskPooling_optimal", "predictionsPooling_optimal",
                       "riskPooling_estimated", "predictionsPooling_estimated",
                       "riskPooling_optimal_withY", "predictionsPooling_optimal_withY",
                       "riskPooling_estimated_withY", "predictionsPooling_estimated_withY",
                       "ccval_fitted", "ccval_optimal")
  missing_cols = setdiff(expected_mu_cols, names(existing_mu))
  if (length(missing_cols) > 0) {
    stop(sprintf(paste0(
      "Existing checkpoint in %s was written by an older version of this script ",
      "(missing column(s): %s). Its schema is incompatible with the current ",
      "validate_one_point()/compute_ccval() output. Move or delete %s (and the ",
      "matching theoretical_risks.rds/empirical_validation_*.rds files) before ",
      "re-running, or restore an older copy of this script to resume it as-is."
    ), out_raw, paste(missing_cols, collapse = ", "), out_raw), call. = FALSE)
  }

  keys_theo = point_key(existing_theoretical$scenario, existing_theoretical$missingness_target)
  keys_mu   = point_key(existing_mu$scenario, existing_mu$missingness_target)
  keys_mc   = point_key(existing_mc$scenario, existing_mc$missingness_target)
  ## Only skip a point if it is present in all three (theoretical + both
  ## families); otherwise a partial/interrupted write for one of the three
  ## could silently leave a gap.
  done_keys = Reduce(intersect, list(keys_theo, keys_mu, keys_mc))

  log_step(sprintf(
    "Resuming from existing output in %s: %d grid points already completed and will be skipped.",
    out_raw, length(done_keys)
  ))
}

## =============================================================================
## 3. Save-progress helper (used both for periodic checkpoints and the final save)
## =============================================================================

#' Combine existing + newly computed results and persist them to disk
#'
#' Called periodically during the main loop (checkpointing) and once more
#' after it finishes. Writes the combined raw .rds and .csv tables, and the
#' pgfplots-ready per-scenario CSVs, but does NOT redraw the (slower) PNG
#' figures -- those are only redrawn at the very end, via `make_figures()`.
#'
#' @return Invisibly returns a list with the three combined data frames
#'   (\code{theoretical_df}, \code{mu_df}, \code{mc_df}), so the caller can
#'   reuse them for the final figures without recomputing the rbind.
save_progress = function() {
  theoretical_df = rbind(existing_theoretical, do.call(rbind, theoretical_list))
  mu_df = rbind(existing_mu, do.call(rbind, mu_list))
  mc_df = rbind(existing_mc, do.call(rbind, mc_list))

  saveRDS(theoretical_df, theo_path)
  saveRDS(mu_df, mu_path)
  saveRDS(mc_df, mc_path)

  write.csv(theoretical_df, file.path(out_tables, "theoretical_risks.csv"), row.names = FALSE)
  write.csv(mu_df,          file.path(out_tables, "empirical_validation_mu.csv"), row.names = FALSE)
  write.csv(mc_df,          file.path(out_tables, "empirical_validation_mc.csv"), row.names = FALSE)

  write_theoretical_risks_latex_tables(theoretical_df, out_tables)
  write_validation_consistency_latex_tables(theoretical_df, mu_df, family = "mu", out_dir = out_tables)
  write_validation_consistency_latex_tables(theoretical_df, mc_df, family = "mc", out_dir = out_tables)

  if (include_outcome_variant) {
    write_validation_consistency_withY_latex_tables(theoretical_df, mu_df, family = "mu", out_dir = out_tables)
    write_validation_consistency_withY_latex_tables(theoretical_df, mc_df, family = "mc", out_dir = out_tables)
  }

  invisible(list(theoretical_df = theoretical_df, mu_df = mu_df, mc_df = mc_df))
}

## =============================================================================
## 4. Main loop: theoretical risks + MU/MC empirical validation, per point
## =============================================================================

log_step(sprintf(
  "Starting validation analysis | run_mode = %s | %d scenarios x %d grid points (of %d, %d already done) | m = %d | checkpoint every %d points",
  run_mode, length(missingness_scenarios), length(selected_grid), length(missingness_grid),
  length(done_keys), m_imputations, checkpoint_every
))

theoretical_list = list()
mu_list  = list()
mc_list  = list()

total_points = length(missingness_scenarios) * length(selected_grid)
point_i = 0
new_point_i = 0
loop_start = Sys.time()

for (scenario in missingness_scenarios) {

  for (missingness_target_X1 in selected_grid) {

    point_i = point_i + 1

    if (point_key(scenario, missingness_target_X1) %in% done_keys) next

    path_simulation_object = sprintf(
      "%s/%s/missingness_target_X1_%.3f/simulation_%s_%.3f.rds",
      raw_dir, scenario, missingness_target_X1, scenario, missingness_target_X1
    )

    if (!file.exists(path_simulation_object)) {
      log_step(sprintf("  [skip] missing file: %s", path_simulation_object), indent = 1)
      next
    }

    simulation_object = readRDS(path_simulation_object)
    beta_phi = simulation_object[["beta_phi"]]

    ## --- theoretical risks ---
    theo = compute_theoretical_risks(theta, beta_phi,
                                     N = N_theoretical, B = B_theoretical)
    theo_row = as.data.frame(theo)
    theo_row$scenario = scenario
    theo_row$missingness_target = missingness_target_X1
    theo_row$missingness_proportion = mean(simulation_object[["data"]][["train"]][["MX1"]])
    theoretical_list[[length(theoretical_list) + 1]] = theo_row

    ## --- empirical validation: MU ---
    mu_row = validate_one_point(simulation_object, family = "mu", theta = theta,
                                m = m_imputations, K = K_sir, B_inner = B_sir,
                                include_outcome_variant = include_outcome_variant,
                                verbose = FALSE)
    mu_list[[length(mu_list) + 1]] = mu_row

    ## --- empirical validation: MC ---
    mc_row = validate_one_point(simulation_object, family = "mc", theta = theta,
                                m = m_imputations, K = K_sir, B_inner = B_sir,
                                include_outcome_variant = include_outcome_variant,
                                verbose = FALSE)
    mc_list[[length(mc_list) + 1]] = mc_row

    new_point_i = new_point_i + 1

    ## --- progress / ETA (over the whole run, resumed points included) ---
    if (new_point_i == 1 || point_i %% 10 == 0 || point_i == total_points) {
      log_progress(point_i, total_points, loop_start,
                   label = sprintf("%s @ %.3f", scenario, missingness_target_X1))
    }

    ## --- checkpoint: persist progress so at most `checkpoint_every` points
    ##     of work are ever at risk if the process is killed or crashes ---
    if (new_point_i %% checkpoint_every == 0) {
      save_progress()
      log_step(sprintf("Checkpoint saved (%d/%d points done overall).", point_i, total_points), indent = 1)
    }
  }
}

log_step(sprintf("Main loop finished in %s | %d new points computed this run",
                 format_duration(as.numeric(difftime(Sys.time(), loop_start, units = "secs"))),
                 new_point_i))

## =============================================================================
## 5. Final save + figures
## =============================================================================

log_step("Saving final results tables...")
combined = save_progress()

log_step(sprintf("Total rows: %d theoretical | %d MU | %d MC",
                 nrow(combined$theoretical_df), nrow(combined$mu_df), nrow(combined$mc_df)))

log_step("Building figures...")

plot_theoretical_risks(combined$theoretical_df, file = file.path(out_figs, "theoretical_risks.png"))
plot_validation_consistency(combined$theoretical_df, combined$mu_df, family = "mu",
                            file = file.path(out_figs, "validation_consistency_mu.png"))
plot_validation_consistency(combined$theoretical_df, combined$mc_df, family = "mc",
                            file = file.path(out_figs, "validation_consistency_mc.png"))

if (include_outcome_variant) {
  plot_validation_consistency_withY(combined$theoretical_df, combined$mu_df, family = "mu",
                                    file = file.path(out_figs, "validation_consistency_mu_withy.png"))
  plot_validation_consistency_withY(combined$theoretical_df, combined$mc_df, family = "mc",
                                    file = file.path(out_figs, "validation_consistency_mc_withy.png"))
}

log_step(sprintf("Done. Outputs written under %s (raw/, tables/, figures/).", out_dir))
