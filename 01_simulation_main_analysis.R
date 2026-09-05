################################################################################
## Main simulation study
## Reproduces all primary results of the paper
##
## CHECKPOINTING / RESUME: each grid point's raw simulation object is saved
## to output/main/raw/<scenario>/.../simulation_<scenario>_<target>.rds as
## soon as it is computed. On top of that, if that file already exists when
## the loop reaches a given (scenario, missingness_target) point -- e.g.
## because a previous run was interrupted (killed, crashed, machine
## restarted) and this script is being re-launched -- the expensive
## simulate/train/reference-probabilities step is skipped and the existing
## object is reloaded from disk instead; only the (cheap) performance
## evaluation is redone, so scenario_tables stays complete regardless of how
## many restarts happened. This mirrors the resume logic already used in
## 05_validation_analysis.R. The process can therefore safely be killed and
## restarted with the same script: only the point actually being computed at
## the moment of interruption is ever at risk of being redone.
################################################################################
## 1. Packages
library(future.apply) # Parallel computation of reference probabilities
library(norm)         # For MLE and MLEMI procedures
library(mice)         # For MI and MIMI procedures

## 2. Reproducibility
base_seed = 314159
set.seed(base_seed)

## 3. Core functions
source("R/functions/data_generation.R")
source("R/functions/performance_metrics.R")
source("R/functions/reference_probabilities.R")
source("R/functions/training_procedures.R")
source("R/functions/utils.R")
source("R/functions/logging_utils.R")

## 4. Global configuration
source("R/config/config.R")

## 5. Output directories
dir.create("output/main/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output/main/tables", recursive = TRUE, showWarnings = FALSE)

## =============================================================================
## Main loop over missingness scenarios 1 to 5
## =============================================================================

total_points = length(missingness_scenarios) * length(missingness_grid)
point_i = 0
loop_start = Sys.time()

log_step(sprintf(
  "Starting main simulation | %d scenarios x %d grid points = %d total points",
  length(missingness_scenarios), length(missingness_grid), total_points
))

for (scenario in missingness_scenarios) {
  log_step(sprintf("Running scenario: %s", scenario))
  scenario_tables = list("overall"    = list(),
                         "complete"   = list(),
                         "incomplete" = list())

  ## ---------------------------------------------------------------------------
  ## Loop over missingness proportion
  ## ---------------------------------------------------------------------------

  for (missingness_target_X1 in missingness_grid) {

    point_i = point_i + 1

    raw_dir = file.path("output/main/raw",
                        scenario,
                        sprintf("missingness_target_X1_%0.3f",
                                missingness_target_X1))
    raw_file = file.path(raw_dir,
                         sprintf("simulation_%s_%0.3f.rds",
                                 scenario,
                                 missingness_target_X1))

    if (file.exists(raw_file)) {
      ## --- RESUME: a previous (possibly interrupted) run already computed
      ## this point -- reload it instead of redoing the expensive simulate/
      ## train/reference-probabilities work below.
      simulation_object = readRDS(raw_file)
      observed_missingness_X1 = mean(simulation_object[["data"]][["train"]][["MX1"]] == 1)

    } else {
      # Tune the intercept of the coefficients of phi
      beta_phi = phi[[scenario]][["beta"]]

      alpha0 =
        tune_missingness_intercept(
          target_missingness = missingness_target_X1,
          beta_phi = beta_phi,
          theta = theta
        )

      beta_phi["(Intercept)"] = alpha0


      # Generate full data with variables and missingness indicators
      data_full = simulate_data(train_size = train_size,
                                test_size = test_size,
                                theta = theta,
                                beta_phi = beta_phi,
                                missingness_target_X1 = missingness_target_X1,
                                missingness_target_Y = missingness_target_Y)

      # generate missing values in X1 and X2
      data_train = mask_data(data_full[["train"]], mask_Y = FALSE)
      data_test  = mask_data(data_full[["test"]], mask_Y = FALSE)
      # data_test_full is required for computing the reference probabilities
      data_test_full = data_full[["test"]]

      # Compute observed missingness proportion of X1 in data_train
      observed_missingness_X1 = mean(data_train$MX1 == 1)

      # Train prediction functions and predict
      predictions = list()

      for (procedure_id in names(training_procedures)) {
        procedure = training_procedures[[procedure_id]][["procedure"]]
        prediction_function = procedure(data_train)
        predictions[[procedure_id]] = predict_Y(object = prediction_function,
                                                newdata = data_test,
                                                procedure_id = procedure_id)
      }

      # Compute the reference probabilities
      reference_probabilities = compute_reference_probabilities(
        dataset = data_test_full,
        theta = theta,
        beta_phi = beta_phi,
        B= monte_carlo_size)

      idx0 = data_test[["MX1"]] == 0
      refMU = ifelse(idx0,
                     reference_probabilities[["EY_X1X2"]],
                     reference_probabilities[["EY_X2"]])
      refOMU = reference_probabilities[["EY_X1X2"]]

      refMC = ifelse(idx0,
                     reference_probabilities[["EY_X1X2MX1"]],
                     reference_probabilities[["EY_X2MX1"]])
      refOMC = reference_probabilities[["EY_X1X2MX1"]]

      # Build simulation object
      simulation_object = list(
        data = list(train = data_train,
                    test = data_test,
                    test_full = data_test_full),
        predictions = predictions,
        reference_probabilities = list("refMU" = refMU,
                                       "refMC" = refMC,
                                       "refOMU" = refOMU,
                                       "refOMC" = refOMC),
        metadata = list(scenario = scenario,
                        missingness_target_X1 = missingness_target_X1),
        beta_phi = beta_phi
      )

      # Archive raw simulation object -- this is the checkpoint: as soon as
      # this write succeeds, the point is durably done and will be skipped
      # (via the file.exists() check above) on any future restart.
      dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(simulation_object,
              file = raw_file)

      # Clean memory (only what was freshly created in this branch)
      rm(data_full,
        data_train,
        data_test,
        data_test_full,
        predictions,
        reference_probabilities)
    }

    # Evaluate performance (always done -- cheap relative to the block above,
    # so it's simplest to just redo it for resumed points too rather than
    # also cache it separately)
    perf = evaluate_performance(simulation_object)

    # Store performance by group and metric
    key = sprintf("%.4f", missingness_target_X1)

    for (group in names(perf)) {
      for (metric in names(perf[[group]])) {

        if (!is.list(scenario_tables[[group]][[metric]])) {
          scenario_tables[[group]][[metric]] = list()
        }

        scenario_tables[[group]][[metric]][[key]] = list(
          performance = perf[[group]][[metric]],
          observed_missingness = observed_missingness_X1
        )
      }
    }

    # Clean memory
    rm(simulation_object,
       perf)
    gc(FALSE)

    if (point_i %% 20 == 0 || point_i == total_points) {
      log_progress(point_i, total_points, loop_start,
                  label = sprintf("%s @ %.3f", scenario, missingness_target_X1))
    }
  }

  ## Save tables of performances (points) and create LOESS regression (loess)
  write_results_tables(
    tables = scenario_tables,
    scenario = scenario,
    out_dir = "output/main/tables",
    loess_span = loess_span
  )
}

log_step("Main simulation finished.")

## Session info
capture.output(
  sessionInfo(),
  file = "output/main/sessionInfoMain.txt"
)
