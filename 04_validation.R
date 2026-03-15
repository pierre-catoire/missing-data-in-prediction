############################################################
# 0. Setup
############################################################

source("R/functions/training_procedures.R")
source("R/functions/reference_probabilities.R")
source("R/functions/performance_metrics.R")
source("R/functions/validation_functions.R")
source("R/functions/utils.R")
source("R/config/config.R")

dir.create("output/validation/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output/validation/tables", recursive = TRUE, showWarnings = FALSE)

base_seed = 314159
set.seed(base_seed)


library(mice)


############################################################
# 1. VALIDATION PHASE
############################################################

df_results_list = list()

for (scenario in missingness_scenarios) {
  
  df_results = data.frame("MISSPROP" = numeric(),
                          "RPTRAINED" = numeric(),
                          "PPTRAINED" = numeric(),
                          "CCVTRAINED" = numeric(),
                          "RAWTRAINED" = numeric(),
                          "RPBAYES" = numeric(),
                          "PPBAYES" = numeric(),
                          "CCVBAYES" = numeric(),
                          "RAWBAYES" = numeric(),
                          "RISKFULL" = numeric(),
                          "RISKOBS" = numeric(),
                          "RISKFULLUNCONDITIONAL" = numeric())
  i = 1
  for (missingness_target_X1 in missingness_grid) {
    
    message("Scenario: ", scenario, ", missingness target: ", missingness_target_X1)
    
    path_simulation_object = sprintf(
      "output/main/raw/%1$s/missingness_target_X1_%2$.3f/simulation_%1$s_%2$.3f.rds",
      scenario,
      missingness_target_X1
    )
    
    simulation_object = readRDS(path_simulation_object)
    
    data_train = simulation_object[["data"]][["train"]]
    data_test  = simulation_object[["data"]][["test"]]
    y_true     = data_test[["Y"]]
    
    validation_object = list("missingness_proportion_MX1" = mean(data_train[["MX1"]]),
                             "simulation_object" = simulation_object)
    
    # Analysis 1. CCV versus MI-PP vs MI-RP
    validation_results = list("trained" = list(),
                              "bayes-optimal" = list())
    
    # results of a predictor allowing missing predictors at deployment
    raw_results = list("trained" = compute_msd(y_true,
                                               simulation_object[["predictions"]][["mi"]]),
                       "bayes_optimal" = compute_msd(y_true,
                                                     unlist(simulation_object[["reference_probabilities"]][["refMU"]])))
    #TODO: why is the raw application of optimal MU predictor different from the results obtained after MI-PP ???
    # Because the imputation is NOT done with Y!!!!! and so not MAR in 5!!
    
    ## impute the testing set
    pm_test = make.predictorMatrix(data_test[, c("X1","X2","Y")])
    pm_test[,"Y"] = 0
    pm_test["Y",] = 0
    imp_test = mice(data_test[,c("X1","X2","Y")], method = "norm",
                    m = 5,
                    predictorMatrix = pm_test,
                    printFlag = F)
    test_list = complete(imp_test, "all")
    
    ### impute the training set
    imp_train = mice(data_train[,c("X1","X2","Y")], method = "norm", m = 5,
                     printFlag = F)
    
    ### Fit linear model across imputed datasets
    fit_train = with(imp_train, lm(Y ~ X1 + X2))
    coef_train = setNames(pool(fit_train)[,3][,"estimate"],
                          pool(fit_train)[,3][,"term"])
    
    coefList = list("trained" = coef_train,
                    "bayes_optimal" = theta[["Y"]][["beta"]])
    
    for (coefName in names(coefList)) { # iterate between trained and Bayes-optimal forecasters
      coef = coefList[[coefName]]
      
      ### predict Y in each imputed testing set
      pred_list = lapply(test_list, function(dat) {
        X = model.matrix(Y ~ X1 + X2, dat)
        as.vector(X %*% coef)
      })
      
      ### MSE - pool risk
      mse_vec = sapply(pred_list, function(pred) {
        compute_msd(y_true,pred)
      })
      mse_rp = mean(mse_vec)
      
      ### MSE - pool predictions
      pred_matrix = do.call(cbind, pred_list)
      pred_pooled = rowMeans(pred_matrix)
      mse_pp = compute_msd(y_true, pred_pooled)
      
      ### MSE - complete case validation
      data_test_ccv = data_test[complete.cases(data_test[,c("X1","X2")]),]
      pred_ccv = model.matrix(Y ~ X1 + X2, data_test_ccv) %*% coef
      y_ccv = data_test_ccv[["Y"]]
      mse_ccv = compute_msd(y_ccv, pred_ccv)
      
      validation_results[[coefName]][["RP"]] = mse_rp
      validation_results[[coefName]][["PP"]] = mse_pp
      validation_results[[coefName]][["CCV"]] = mse_ccv
    }
    beta_phi = simulation_object[["beta_phi"]]
    risks = unlist(compute_true_risks(theta, beta_phi, N = 1e6))
    df_results[i,] = c(mean(data_train[["MX1"]]),
                       c(unname(unlist(validation_results[["trained"]])),raw_results[["trained"]]),
                       c(unname(unlist(validation_results[["bayes_optimal"]])),raw_results[["trained"]]),
                       risks)
    i = i+1
  }
  df_results_list[[scenario]] = df_results
}

