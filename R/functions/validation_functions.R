#' Perform multiple imputation at validation
#'
#' Performs multiple imputation on a validation dataset using \code{mice()},
#' with flexible control over whether missingness indicators and/or the
#' outcome are included in the imputation model.
#'
#' The predictor matrix is modified so that selected variables are excluded
#' from serving as predictors in the imputation models.
#'
#' @param data_test Validation dataset.
#' @param m Number of multiple imputations (default 5).
#' @param predictors Character vector of predictor names.
#' @param outcome Name of the outcome variable (default "Y").
#' @param miss_inds Character vector of missingness indicator names.
#' @param include_miss_inds Logical; whether missingness indicators are
#' included as predictors in the imputation model.
#' @param include_outcome Logical; whether the outcome is included as a
#' predictor in the imputation model.
#'
#' @return A \code{mids} object from \code{mice()} containing the imputed
#' validation datasets.
#'
#' @details
#' If \code{include_miss_inds = FALSE}, missingness indicators are prevented
#' from being used as predictors in the imputation models.
#'
#' If \code{include_outcome = FALSE}, the outcome is excluded from the
#' imputation predictor matrix.
#'
#' @export
impute_at_validation = function(data_test,
                                m = 5,
                                predictors = c("X1","X2"),
                                outcome = "Y",
                                miss_inds = c("MX1"),
                                include_miss_inds = FALSE,
                                include_outcome = FALSE){
  
  variables  = c(predictors,outcome,miss_inds)
  
  predictor_matrix = make.predictorMatrix(data_test[,variables])
  
  if (!include_miss_inds){
    predictor_matrix[,miss_inds] = 0
  }
  
  if (!include_outcome){
    predictor_matrix[,outcome] = 0
  }
  
  imp = mice(data_test[,variables],
             m = m,
             predictorMatrix = predictor_matrix,
             printFlag = FALSE)
  return(imp)
}

#' Compute predictive performance under different pooling rules
#'
#' Computes the mean squared deviation (MSD) of a prediction function
#' applied to multiply imputed validation datasets, using one of three
#' pooling strategies:
#'
#' \itemize{
#'   \item \code{"complete"}: Evaluate performance on complete cases only.
#'   \item \code{"predictions"}: Pool predictions across imputations,
#'   then compute MSD.
#'   \item \code{"scores"}: Compute MSD within each imputed dataset,
#'   then average the scores.
#' }
#'
#' @param prediction_function Function used to generate predictions.
#' Must accept arguments \code{prediction_model} and \code{newdata}.
#' @param prediction_model Fitted prediction model object.
#' @param imputed_validation_sets A \code{mids} object containing
#' multiply imputed validation datasets.
#' @param y_true True outcome values.
#' @param pooling_method One of \code{"complete"}, \code{"predictions"},
#' or \code{"scores"}.
#'
#' @return The estimated mean squared deviation.
#'
#' @details
#' The function distinguishes between pooling at the prediction level
#' (averaging predicted probabilities across imputations) and pooling
#' at the performance level (averaging MSD across imputations).
#'
#' @export
get_function_performance = function(prediction_function,
                                    prediction_model,
                                    imputed_validation_sets,
                                    y_true,
                                    pooling_method) {
  if (!(pooling_method %in% c("complete", "predictions", "scores"))) {
    stop(pooling_method, " must be one of \"complete\", \"predictions\", \"scores\".")
  }
  
  if (pooling_method == "complete") {
    data_test = imputed_validation_sets[["data"]]
    complete_data_test = data_test[complete.cases(data_test),]
    predictions = prediction_function(prediction_model,
                                      newdata = complete_data_test)
    mse = compute_msd(complete_data_test[["Y"]], predictions)
  } else {
    nrow_data_test     = nrow(imputed_validation_sets[["data"]])
    m_imputed_datasets = imputed_validation_sets[["m"]]
    
    predictions = matrix(NA,
                         nrow = nrow_data_test,
                         ncol = m_imputed_datasets)
    
    for (imputation_index in 1:m_imputed_datasets) {
      imputed_dataset = complete(imputed_validation_sets,
                                 imputation_index)
      
      predictions[,imputation_index] = prediction_function(prediction_model,
                                                           newdata = imputed_dataset)
    }
    
    if (pooling_method == "predictions") {
      pooled_predictions = rowMeans(predictions)
      mse = compute_msd(y_true, pooled_predictions)
    } else if (pooling_method == "scores") {
      pooled_scores = apply(predictions, MARGIN = 2, FUN = compute_msd, reference = y_true)
      mse = mean(pooled_scores)
    }
  }
  
  return(mse)
}

#' Compute reference predictive performance
#'
#' Computes reference (oracle) predictive performance for both
#' MI and MIMI target quantities under different pooling strategies.
#'
#' The reference predictions are obtained via
#' \code{compute_reference_probabilities()}, and mean squared deviation
#' is computed under:
#'
#' \itemize{
#'   \item \code{"complete"}: Complete-case evaluation,
#'   \item \code{"predictions"}: Pool predictions across imputations,
#'   \item \code{"scores"}: Pool performance across imputations.
#' }
#'
#' @param imputed_validation_sets A \code{mids} object containing
#' multiply imputed validation datasets.
#' @param y_true True outcome values.
#' @param scenario Scenario index used to retrieve model parameters.
#'
#' @return A nested list of the form:
#' \code{reference_performance[[model_name]][[pooling_method]]},
#' where \code{model_name} is \code{"mi"} or \code{"mimi"},
#' and \code{pooling_method} is one of
#' \code{"complete"}, \code{"predictions"}, \code{"scores"}.
#'
#' @details
#' Reference probabilities are computed using externally defined
#' parameters \code{theta} and \code{phi}. Monte Carlo integration
#' is performed with \code{B = 5000}.
#'
#' @export
get_reference_performance = function(imputed_validation_sets, y_true, scenario) {
  
  mse_list = list("mi" = list(),
                  "mimi" = list())
  
  # complete
  data_test = imputed_validation_sets[["data"]]
  complete_data_test = data_test[complete.cases(data_test),]
  references_probabilities = compute_reference_probabilities(complete_data_test,
                                                             theta = theta,
                                                             beta_phi = phi[[scenario]][["beta"]],
                                                             B = 5000,
                                                             parallel = TRUE,
                                                             compute_observed = FALSE)
  
  mse_list[["mi"]][["complete"]] = compute_msd(complete_data_test[["Y"]],
                                               unlist(references_probabilities[["EY_X1X2"]]))
  mse_list[["mimi"]][["complete"]] = compute_msd(complete_data_test[["Y"]],
                                                 unlist(references_probabilities[["EY_X1X2MX1"]]))
  
  # predictions and scores
  
  nrow_data_test     = nrow(imputed_validation_sets[["data"]])
  m_imputed_datasets = imputed_validation_sets[["m"]]
  
  prediction_empty_matrix = matrix(NA,
                                   nrow = nrow_data_test,
                                   ncol = m_imputed_datasets)
  predictions_list = list("mi" = prediction_empty_matrix,
                          "mimi" = prediction_empty_matrix)
  
  for (imputation_index in 1:m_imputed_datasets) {
    imputed_dataset = complete(imputed_validation_sets,
                               imputation_index)
    
    references_probabilities = compute_reference_probabilities(imputed_dataset,
                                                               theta = theta,
                                                               beta_phi = phi[[scenario]][["beta"]],
                                                               B = 5000,
                                                               parallel = TRUE,
                                                               compute_observed = FALSE)
    
    predictions_list[["mi"]][,imputation_index] = unlist(references_probabilities[["EY_X1X2"]])
    predictions_list[["mimi"]][,imputation_index] = unlist(references_probabilities[["EY_X1X2MX1"]])
  }
  
  for (method in c("mi", "mimi")) {
    mse_list[[method]][["predictions"]] = compute_msd(y_true,
                                                      rowMeans(predictions_list[["mi"]]))
    mse_list[[method]][["scores"]] = mean(apply(predictions_list[[method]],
                                                MARGIN = 2,
                                                FUN = compute_msd,
                                                reference = y_true))
  }
  
  # must return a list reference_performance[[model_name]][[pool]]
  return(mse_list)
}


compute_true_risks <- function(theta,
                               beta_phi,
                               N = 1e6) {
  ## --- 1. Extract parameters from theta ---
  
  mu1 <- theta$X1$beta[1]
  sd1 <- theta$X1$sigma
  
  mu2 <- theta$X2$beta[1]
  sd2 <- theta$X2$sigma
  
  beta0 <- theta$Y$beta["(Intercept)"]
  beta1 <- theta$Y$beta["X1"]
  beta2 <- theta$Y$beta["X2"]
  sdY   <- theta$Y$sigma
  
  ## --- 2. Simulate joint distribution ---
  
  X1 <- rnorm(N, mu1, sd1)
  X2 <- rnorm(N, mu2, sd2)
  
  muY_X <- beta0 + beta1 * X1 + beta2 * X2
  Y <- rnorm(N, muY_X, sdY)
  
  epsilon <- Y - muY_X
  
  ## --- 3. Simulate missingness using beta_phi ---
  
  # Ensure coefficient order
  lp <- beta_phi["(Intercept)"] +
    beta_phi["X1"] * X1 +
    beta_phi["X2"] * X2 +
    beta_phi["Y"]  * Y
  
  pM1 <- plogis(lp)
  M   <- rbinom(N, 1, pM1)
  
  ## --- 4. Compute risks ---
  
  # 3️⃣ Unconditional full-data risk
  risk_full_unconditional <- mean(epsilon^2)
  
  # 1️⃣ Full risk conditional on M=0
  risk_full <- mean(epsilon[M == 0]^2)
  
  # 2️⃣ MU deployment risk
  # Need E[Y | X2]
  # Since X1 ⟂ X2 in your model:
  # E[Y | X2] = beta0 + beta1 * E[X1] + beta2 * X2
  
  muY_X2 <- beta0 + beta1 * mu1 + beta2 * X2
  
  pred_MU <- ifelse(M == 0, muY_X, muY_X2)
  
  risk_obs <- mean((Y - pred_MU)^2)
  
  return(list(
    risk_full = risk_full,
    risk_obs = risk_obs,
    risk_full_unconditional = risk_full_unconditional
  ))
}