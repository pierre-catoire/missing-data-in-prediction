################################################################################
## Secondary simulation study
## Compares MLE and MI when trained on all data or subset with observed Y only
##
## CHECKPOINTING / RESUME: as in 01_simulation_main_analysis.R, each grid
## point's raw simulation object is saved to
## output/secondary/raw/<scenario>/.../simulation_<scenario>_<target>.rds as
## soon as it is computed. If that file already exists when the loop reaches
## a given point -- e.g. because a previous run was interrupted -- the
## expensive model-fitting step (MLE via EM, MI via mice, x2 training subsets
## each) is skipped and the existing object is reloaded from disk instead;
## only the (cheap) performance evaluation is redone. The process can
## therefore safely be killed and restarted with the same script.
################################################################################

## 1. Packages
library(future.apply) # Parallel computation of reference probabilities
library(norm)         # For MLE and MLE-MI procedures
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
dir.create("output/secondary/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output/secondary/tables", recursive = TRUE, showWarnings = FALSE)

## 6. Restrict to scenario 5
scenario = "M5"

scenario_tables = list("overall"    = list(),
                       "complete"   = list(),
                       "incomplete" = list())

## Get the files
main_raw_dir      = file.path("output/main/raw", scenario)

sim_files = list.files(main_raw_dir,
                       pattern = "\\.rds$",
                       recursive = TRUE,
                       full.names = TRUE)

## Initiate results
results = list()

total_points = length(sim_files)
point_i = 0
loop_start = Sys.time()

log_step(sprintf(
  "Starting secondary simulation | scenario %s | %d grid points",
  scenario, total_points
))

## ---------------------------------------------------------------------------
## Loop over sim_files
## ---------------------------------------------------------------------------

for (sim_file in sim_files) {

  point_i = point_i + 1

  sim = readRDS(sim_file)
  missingness_target_X1 = sim$metadata$missingness_target_X1

  secondary_raw_dir = file.path("output/secondary/raw",
                      scenario,
                      sprintf("missingness_target_X1_%0.3f",
                              missingness_target_X1))
  secondary_raw_file = file.path(secondary_raw_dir,
                           sprintf("simulation_%s_%0.3f.rds",
                                   scenario,
                                   missingness_target_X1))

  if (file.exists(secondary_raw_file)) {
    ## --- RESUME: a previous (possibly interrupted) run already computed
    ## this point -- reload it instead of refitting MLE/MI on both training
    ## subsets.
    simulation_object = readRDS(secondary_raw_file)
    observed_missingness_X1 = mean(sim$data$train[["MX1"]] == 1)

  } else {
    data_train_all        = mask_data(sim$data$train, mask_Y = TRUE)
    data_train_observed_Y = sim$data$train[sim$data$train[["MY"]] == 0,]
    data_test             = sim$data$test

    observed_missingness_X1 = mean(data_train_all$MX1 == 1)

    ## Train models
    models = list(
      mleall        = train_mle(data_train_all),
      mleobservedY = train_mle(data_train_observed_Y),
      miall         = train_mi(data_train_all),
      miobservedY  = train_mi(data_train_observed_Y)
    )

    predictions = list(
      mleall        = predict_mle(models$mleall, data_test),
      mleobservedY = predict_mle(models$mleobservedY, data_test),
      miall         = predict_mi(models$miall, data_test),
      miobservedY  = predict_mi(models$miobservedY, data_test)
    )

    reference_probabilities = list(refMU = sim$reference_probabilities$refMU,
                                   refMC = sim$reference_probabilities$refMC,
                                   refOMU = sim$reference_probabilities$refOMU,
                                   refOMC = sim$reference_probabilities$refOMC)

    simulation_object = list(data = list(test = data_test),
                             predictions = predictions,
                             reference_probabilities = reference_probabilities,
                             metadata = list(scenario = scenario,
                                             missingness_target_X1 = missingness_target_X1))

    ## Archive raw simulation object -- this is the checkpoint: as soon as
    ## this write succeeds, the point is durably done and will be skipped
    ## (via the file.exists() check above) on any future restart.
    dir.create(secondary_raw_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(simulation_object,
            file = secondary_raw_file)

    rm(data_train_all, data_train_observed_Y, data_test,
       models, predictions, reference_probabilities)
  }

  # Evaluate performance (always done -- cheap relative to the block above,
  # so it's simplest to just redo it for resumed points too rather than
  # also cache it separately)
  perf = evaluate_performance(simulation_object)

  key = sprintf("%.3f", missingness_target_X1)

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

  rm(simulation_object, perf)
  gc(FALSE)

  if (point_i %% 20 == 0 || point_i == total_points) {
    log_progress(point_i, total_points, loop_start,
                label = sprintf("%s @ %.3f", scenario, missingness_target_X1))
  }
}

log_step("Secondary simulation finished.")

write_results_tables(
  tables = scenario_tables,
  scenario = scenario,
  out_dir = "output/secondary/tables",
  loess_span = loess_span
)

log_step("Secondary simulation tables written to output/secondary/tables.")

capture.output(
  sessionInfo(),
  file = "output/secondary/sessionInfoSecondary.txt"
)
