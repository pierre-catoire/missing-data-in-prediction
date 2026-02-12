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
