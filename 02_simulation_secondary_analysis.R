################################################################################
## Secondary simulation study
## Compares MLE and MI when trained on all data or subset with observed Y only
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

## ---------------------------------------------------------------------------
## Loop over sim_files
## ---------------------------------------------------------------------------

for (sim_file in sim_files) {
  
  sim = readRDS(sim_file)
  
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
  
  missingness_target_X1 = sim$metadata$missingness_target_X1
  
  simulation_object = list(data = list(test = data_test),
                           predictions = predictions,
                           reference_probabilities = reference_probabilities,
                           metadata = list(scenario = scenario,
                                           missingness_target_X1 = missingness_target_X1))
  
  secondary_raw_dir = file.path("output/secondary/raw",
                      scenario,
                      sprintf("missingness_target_X1_%0.3f",
                              missingness_target_X1))
  dir.create(secondary_raw_dir, recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(simulation_object,
          file = file.path(secondary_raw_dir,
                           sprintf("simulation_%s_%0.3f.rds",
                                   scenario,
                                   missingness_target_X1)))
  
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
}

write_results_tables(
  tables = scenario_tables,
  scenario = scenario,
  out_dir = "output/secondary/tables",
  loess_span = loess_span
)

capture.output(
  sessionInfo(),
  file = "output/secondary/sessionInfoSecondary.txt"
)
