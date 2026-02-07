################################################################################
## Main simulation study
## Reproduces all primary results of the paper
################################################################################
## 1. Packages
library(future.apply) # Parallel computation of reference probabilities
library(norm)         # For MLE and MLE-MI procedures
library(mice)         # For MI and MIMI procedures

## 2. Reproducibility
base_seed = 314159
set.seed(base_seed)

## 3. Core functions
source("R/data_generation.R")
source("R/performance_metrics.R")
source("R/reference_probabilities.R")
source("R/training_procedures.R")
source("R/utils.R")

## 4. Global configuration
source("config.R")

## 5. Output directories
dir.create("output", showWarnings = FALSE)
dir.create("output/raw", showWarnings = FALSE)
dir.create("output/tables", showWarnings = FALSE)

## =============================================================================
## Main loop over missingness scenarios 1 to 5
## =============================================================================

for (scenario in missingness_scenarios) {
  message("Running scenario: ", scenario)
  scenario_tables = list("overall"    = list(),
                         "complete"   = list(),
                         "incomplete" = list())

## ---------------------------------------------------------------------------
  ## Loop over missingness proportion
  ## ---------------------------------------------------------------------------
  
  for (missingness_target_X1 in missingness_grid) {
    message("Missingness target: ", missingness_target_X1)
    
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
                              missingness_target_Y = 0)
    
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
    
    #TODO: refMC seems off ...
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
                      missingness_target_X1 = missingness_target_X1)
    )
    
    # Archive raw simulation object
    raw_dir = file.path("output/raw",
                        scenario,
                        sprintf("missingness_target_X1_%0.2f",
                                missingness_target_X1))
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(simulation_object,
            file = file.path(raw_dir,
                             sprintf("simulation_%s_%0.2f.rds",
                                     scenario,
                                     missingness_target_X1)))
    
    # Evaluate performance
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
       data_full,
       data_train,
       data_test,
       data_test_full,
       predictions,
       reference_probabilities,
       perf)
    gc(FALSE)
  }
  
  ## Save tables of performances (points) and create LOESS regression (loess)
  write_results_tables(
    tables = scenario_tables,
    scenario = scenario,
    out_dir = "output/tables"
  )
}

## Session info
capture.output(
  sessionInfo(),
  file = "output/sessionInfo.txt"
)