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

get_performance = function(prediction_function,
                           prediction_model,
                           imputed_validation_sets,
                           y_true,
                           pooling_method = "predictions") {
  
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
  return(mse)
}