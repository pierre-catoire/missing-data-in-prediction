# The functions listed above are used only for the application

#' Order a named vector by its names
#'
#' Reorders a named vector according to the alphabetical order of its names.
#' This is used to ensure consistent alignment between coefficient vectors
#' and design matrices before matrix multiplication.
#'
#' @param x A named numeric vector.
#'
#' @return A numeric vector sorted according to \code{names(x)}.
#' @export
order_by_name_vector = function(x){
  name_order = order(names(x))
  sorted_vector = x[name_order]
  return(sorted_vector)
}

#' Order matrix columns by column names
#'
#' Reorders the columns of a matrix according to the alphabetical order
#' of its column names. This is used to align predictor matrices with
#' coefficient vectors before computing linear predictors.
#'
#' @param x A matrix with named columns.
#'
#' @return A matrix with columns sorted according to \code{colnames(x)}.
#' @export
order_by_name_matrix = function(x){
  name_order = order(colnames(x))
  sorted_matrix = x[,name_order]
  return(sorted_matrix)
}

#' Extract missingness patterns as binary strings
#'
#' For a given dataset and set of predictors, returns a character vector
#' encoding the missingness pattern of each observation. Each pattern is
#' represented as a string of 0s and 1s, where 1 indicates a missing value.
#'
#' @param dataset A data frame.
#' @param predictors A character vector of predictor names.
#'
#' @return A character vector of length \code{nrow(dataset)} containing
#' binary missingness patterns.
#' @export
get_patterns = function(dataset,predictors){
  apply(is.na(dataset[,predictors]),1,
        function(z) paste(as.integer(z), collapse = "")
  )
}

#' Fit pattern-specific logistic regression submodels
#'
#' Fits one logistic regression model per observed missingness pattern.
#' For each pattern, only predictors fully observed in that subset are used.
#' Binary predictors with only one observed level in the subset are excluded.
#'
#' @param data_train Training dataset containing predictors and binary outcome \code{Y}.
#' @param predictors Character vector of predictor names.
#'
#' @return A list of fitted \code{glm} objects indexed by missingness pattern.
#' The list also contains the original \code{predictors}.
#' @export
fitps = function(data_train, predictors){
  # Get patterns as strings in data_train
  data_train[["pattern"]] = get_patterns(data_train, predictors)
  
  # Split data_train into pattern subsets
  data_train_split = split(data_train, data_train[["pattern"]])
  
  # Intialise submodels list to return
  pattern_submodels = list()
  
  # Loop over patterns
  for (pattern in unique(data_train[["pattern"]])) {
    
    # get the subset of data_train matching the pattern
    d = data_train_split[[pattern]]
    
    # get the predictors observed for that pattern
    observed_predictors = predictors[colSums(is.na(d[,predictors])) == 0]
    
    # Exclude the binary predictors which have only one level observed
    # in that subset
    predictors_to_include = c()
    
    for(predictor in observed_predictors){
      if(is.numeric(d[[predictor]])){
        predictors_to_include = append(predictors_to_include, predictor)
      }
      else if (length(unique(d[[predictor]])) >= 2) {
        predictors_to_include = append(predictors_to_include, predictor)
      }
    }
    
    # Build formula
    fmla = reformulate(predictors_to_include, response = "Y")
    
    # Fit model
    pattern_submodels[[pattern]] = glm(fmla, data = d, family = "binomial")
  }
  pattern_submodels[["predictors"]] = predictors
  return(pattern_submodels)
}

#' Predict using pattern-specific submodels
#'
#' Generates predicted probabilities for a test dataset using
#' pattern-specific logistic regression models fitted by \code{fitps()}.
#'
#' @param data_test Test dataset.
#' @param pattern_submodels Output of \code{fitps()}.
#'
#' @return A numeric vector of predicted probabilities.
#' @export
predps = function(data_test, pattern_submodels){
  predictors = pattern_submodels[["predictors"]]
  # Initialise predictions
  preds = rep(NA,nrow(data_test))
  
  # get patterns in data_test
  data_test[["pattern"]] = get_patterns(data_test, predictors)
  
  # Loop over patterns
  for (pattern in unique(data_test[["pattern"]])) {
    # identify the observations in data_test matching the pattern
    id_pattern = data_test[["pattern"]] == pattern
    
    # predict using the pattern submodel matching the pattern
    preds[id_pattern] = predict(pattern_submodels[[pattern]],
                                newdata = data_test[id_pattern,],
                                type = "response")
  }
  return(preds)
}

#' Fit multiple imputation prediction model (MI)
#'
#' Performs multiple imputation using \code{mice()}, then:
#' \itemize{
#'   \item Fits a pooled logistic regression prediction model for \code{Y}
#'   using all predictors.
#'   \item Fits pooled imputation models for each predictor within each
#'   missingness pattern.
#' }
#'
#' @param data_train Training dataset.
#' @param predictors Character vector of predictor names.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{predictors}: predictor names,
#'   \item \code{impFunctions}: imputation models per pattern,
#'   \item \code{predFunction}: pooled prediction coefficients.
#' }
#' @export
fitmi = function(data_train,predictors){
  # Get patterns as strings in data_train
  data_train[["pattern"]] = get_patterns(data_train, predictors)
  
  # Perform multiple imputation
  imp = mice(data_train[,c(predictors,"Y")], printFlag = F)
  
  # Fit the prediction function on all predictors
  fit_pred = with(data=imp,exp=glm(reformulate(predictors, response = "Y"), family = "binomial"))
  predFunction = setNames(pool(fit_pred)[["pooled"]][["estimate"]],
                          pool(fit_pred)[["pooled"]][["term"]])
  
  # Initialise the list of imputation functions
  impFunctions = list()
  
  # Loop over patterns
  for(pattern in unique(data_train[["pattern"]])){
    
    # Initialise the list of imputation functions for that pattern
    impFunctions[[pattern]] = list()
    
    # Get observed and missing predictors for that pattern
    observed_predictors = predictors[strsplit(pattern, "")[[1]] == "0"]
    missing_predictors = predictors[strsplit(pattern, "")[[1]] == "1"]
    
    # Loop over missing predictors for that pattern
    for(missing_predictor in missing_predictors){
      
      # Fit an imputation function for the missing predictor
      fit_imp = with(data=imp,exp=glm(reformulate(observed_predictors,
                                                  response = missing_predictor),
                                      family = "binomial"))
      
      # Store the imputation function
      impFunctions[[pattern]][[missing_predictor]] = setNames(pool(fit_imp)[["pooled"]][["estimate"]],
                                                              pool(fit_imp)[["pooled"]][["term"]])
    }
  }
  return(list(predictors = predictors,
              impFunctions = impFunctions,
              predFunction = predFunction))
}

#' Predict outcome for one observation using MI
#'
#' For a single observation, missing predictors are imputed via
#' Monte Carlo sampling using pooled imputation models. Predictions
#' are averaged across simulated imputations.
#'
#' Currently implemented for binary predictors only.
#'
#' @param individual A single-row data frame.
#' @param mimodel Output of \code{fitmi()}.
#' @param nSample Number of Monte Carlo samples (default 1000).
#'
#' @return Predicted probability.
#' @export
predmi_byrow = function(individual, mimodel, nSample = 1000){
  # Extract the variables
  pattern = individual[["pattern"]]
  predictors = mimodel[["predictors"]]
  impFunctions = mimodel[["impFunctions"]]
  predFunction = mimodel[["predFunction"]]
  
  # Get the observed predictors of the individual
  observed_predictors_i = predictors[strsplit(pattern, "")[[1]] == "0"]
  
  # Initialise the matrix of observed and imputed values
  predictor_values_matrix = matrix(rep(NA,nSample*(1+length(predictors))),
                                   nrow = nSample, ncol = 1+length(predictors))
  
  colnames(predictor_values_matrix) = c("(Intercept)",predictors)
  
  # Initialise the intercept in predictor_values_matrix
  predictor_values_matrix[,"(Intercept)"] = 1
  
  # Loop over predictors
  for (predictor in predictors) {
    
    # If the predictor is observed, store the value of the predictor in all
    # rows of the predictor_values_matrix
    if (!is.na(individual[[predictor]])) {
      predictor_values_matrix[,predictor] = as.numeric(individual[[predictor]])
    } else {
      # If the predictor is missing
      
      # If the missing predictor is a factor
      if (is.factor(individual[[predictor]])) {
        # Get the regression coefficients
        coefs = order_by_name_vector(impFunctions[[pattern]][[predictor]])
        
        # Get the value of the observed predictors
        values = order_by_name_vector(c("(Intercept)" = 1,
                                        unlist(individual[observed_predictors_i])))
        values = as.numeric(values)
        # Estimate the probability of the missing predictor
        prob = (1+exp(-c(coefs %*% values)))^(-1)
        
        # Sample from the estimated probability
        predictor_values_matrix[,predictor] = as.numeric(rbinom(n = nSample,
                                                                size = 1, prob = prob))
      } else if (is.numeric(individual[[predictor]])) {
        # If the missing predictor is numeric
        # TODO
        # Not implemented yet: requires obtaining the residual of the imputation functions
      }
    }
  }
  # apply the prediction function on the whole sample 
  predSample = order_by_name_matrix(predictor_values_matrix) %*% order_by_name_vector(predFunction)
  
  # Obtain the averaged prediction
  pred = (1+exp(-mean(predSample)))^-1
  
  # Return the prediction
  return(pred)
}

#' Predict using multiple imputation (MI)
#'
#' Applies \code{predmi_byrow()} to each observation in a dataset.
#'
#' @param data_test Test dataset.
#' @param mimodel Output of \code{fitmi()}.
#'
#' @return Numeric vector of predicted probabilities.
#' @export
predmi = function(data_test, mimodel){
  predictors = mimodel[["predictors"]]
  # get the patterns in data_test
  data_test[["pattern"]] = get_patterns(data_test, predictors)
  
  # apply predmi_byrow to all individuals in data_test
  res = sapply(
    seq_len(nrow(data_test)),
    function(i) predmi_byrow(data_test[i, , drop = FALSE], mimodel)
  )
  return(res)
}

#' Fit multiple imputation model with missingness indicators (MIMI)
#'
#' Extends MI by augmenting the model with missingness indicator variables.
#' Performs multiple imputation and fits pooled prediction and imputation
#' models including both predictors and missingness indicators.
#'
#' @param data_train Training dataset.
#' @param predictors Character vector of predictor names.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{predictors}
#'   \item \code{impFunctions}
#'   \item \code{predFunction}
#'   \item \code{miss_inds}: missingness indicator names
#' }
#' @export
fitmimi = function(data_train,predictors){
  # Create a vector of missingness indicators
  miss_inds = c()
  for(predictor in predictors){
    if(any(is.na(data_train[[predictor]]))){
      miss_ind_label = paste(predictor,"MISS",sep = "_")
      miss_inds = append(miss_inds, miss_ind_label)
      data_train[[miss_ind_label]] = factor(ifelse(is.na(data_train[[predictor]]), 1, 0))
    }
  }
  
  # perform multiple imputation
  # Note: mice in MIMI produces warnings caused by collinearity between
  # missingness indicators, and between missingness indicators and observed variables
  imp = suppressWarnings(mice(data_train, printFlag = F))
  
  # Fit the prediction function on all predictors
  fit_pred = with(data=imp,exp=glm(reformulate(c(predictors,miss_inds), response = "Y"), family = "binomial"))
  predFunction = setNames(pool(fit_pred)[["pooled"]][["estimate"]],
                          pool(fit_pred)[["pooled"]][["term"]])
  
  # Get the patterns in data_train
  data_train[["pattern"]] = get_patterns(data_train, predictors)
  
  # Initialise the imputation functions
  impFunctions = list()
  
  # Loop over patterns
  for(pattern in unique(data_train[["pattern"]])){
    
    # Initialise the list of imputation functions ofr that pattern
    impFunctions[[pattern]] = list()
    
    # Get observed and missing predictors for that pattern
    observed_predictors = predictors[strsplit(pattern, "")[[1]] == "0"]
    missing_predictors = predictors[strsplit(pattern, "")[[1]] == "1"]
    
    # Loop over missing predictors for that pattern
    for(missing_predictor in missing_predictors){
      
      # Fit the imputation function of this missing predictor for that pattern
      fit_imp = with(data=imp,exp=glm(reformulate(c(observed_predictors,miss_inds),
                                                  response = missing_predictor),
                                      family = "binomial"))
      
      # Store the impputation function
      impFunctions[[pattern]][[missing_predictor]] = setNames(pool(fit_imp)[["pooled"]][["estimate"]],
                                                              pool(fit_imp)[["pooled"]][["term"]])
    }
  }
  return(list(predictors = predictors,
              impFunctions = impFunctions,
              predFunction = predFunction,
              miss_inds = miss_inds))
}

#' Predict outcome for one observation using MIMI
#'
#' Performs Monte Carlo imputation for missing predictors using
#' imputation models that include missingness indicators, then
#' averages predicted probabilities across samples.
#'
#' Currently implemented for binary predictors only.
#'
#' @param individual A single-row data frame.
#' @param mimimodel Output of \code{fitmimi()}.
#' @param nSample Number of Monte Carlo samples (default 1000).
#'
#' @return Predicted probability.
#' @export
predmimi_byrow = function(individual, mimimodel, nSample = 1000){
  # Extract the variables
  pattern = individual[["pattern"]]
  predictors = mimimodel[["predictors"]]
  impFunctions = mimimodel[["impFunctions"]]
  predFunction = mimimodel[["predFunction"]]
  miss_inds = mimimodel[["miss_inds"]]
  
  # Get the observed predictors of the individual
  observed_predictors_i = predictors[strsplit(pattern, "")[[1]] == "0"]
  
  # Initialise the matrix of observed and imputed values
  predictor_values_matrix = matrix(rep(NA,nSample*(1+length(predictors)+length(miss_inds))),
                                   nrow = nSample, ncol = 1+length(predictors)+length(miss_inds))
  
  colnames(predictor_values_matrix) = c("(Intercept)",c(predictors,miss_inds))
  
  # Initialise the intercept in predictor_values_matrix
  predictor_values_matrix[,"(Intercept)"] = 1
  
  # Initialise the missingness indicators in predictor_values_matrix
  for (miss_ind in miss_inds){
    miss_var = strsplit(miss_ind,split = "_MISS")[[1]]
    predictor_values_matrix[,miss_ind] = ifelse(is.na(individual[[miss_var]]),1,0)
    individual[[miss_ind]] = ifelse(is.na(individual[[miss_var]]),1,0)
  }
  
  # Loop over predictors
  for (predictor in predictors) {
    # If the predictor is observed, store the value of the predictor in all
    # rows of the predictor_values_matrix
    if (!is.na(individual[[predictor]])) {
      predictor_values_matrix[,predictor] = as.numeric(individual[[predictor]])
    } else {
      # If the predictor is missing
      
      # If the missing predictor is a factor
      if(is.factor(individual[[predictor]])) {
        # Get the regression coefficients
        coefs = order_by_name_vector(impFunctions[[pattern]][[predictor]])
        
        # Get the value of the observed predictors
        values = order_by_name_vector(c("(Intercept)" = 1,
                                        unlist(individual[c(observed_predictors_i,miss_inds)])))
        values = as.numeric(values)
        
        # Estimate the probability of the missing predictor
        prob = (1+exp(-c(coefs %*% values)))^(-1)
        
        # Sample from the estimated probability
        predictor_values_matrix[,predictor] = as.numeric(rbinom(n = nSample,
                                                                size = 1, prob = prob))
      } else if (is.numeric(individual[[predictor]])) {
        # If the missing predictor is numeric
        # TODO
        # Not implemented yet: requires obtaining the residual of the imputation functions
      }
    }
  }
  # Apply the prediction function on the whole sample 
  predSample = order_by_name_matrix(predictor_values_matrix) %*% order_by_name_vector(predFunction)
  
  # Obtain the averaged prediction
  pred = (1+exp(-mean(predSample)))^-1
  
  # Return the prediction
  return(pred)
}

#' Predict using MIMI
#'
#' Applies \code{predmimi_byrow()} to each observation in a dataset.
#'
#' @param data_test Test dataset.
#' @param mimimodel Output of \code{fitmimi()}.
#'
#' @return Numeric vector of predicted probabilities.
#' @export
predmimi = function(data_test, mimimodel){
  predictors = mimimodel[["predictors"]]
  # get the patterns in data_test
  data_test[["pattern"]] = get_patterns(data_test, predictors)
  
  # apply predmimi_byrow to all individuals in data_test
  res = sapply(
    seq_len(nrow(data_test)),
    function(i) predmimi_byrow(data_test[i, , drop = FALSE], mimimodel)
  )
  return(res)
}

#' Bootstrap mean squared error
#'
#' Computes the mean squared error (MSE) and a percentile bootstrap
#' confidence interval.
#'
#' @param x Predicted values.
#' @param y_true True outcome values.
#' @param nboot Number of bootstrap samples (default 10000).
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{mse}: empirical MSE
#'   \item \code{ci}: 95% percentile bootstrap interval
#'   \item \code{dist}: bootstrap distribution
#' }
#' @export
bootstrap_mse = function(x,y_true,nboot = 10000){
  squared_error = (x-y_true)^2
  res = rep(NA, nboot)
  
  for (i in 1:nboot) {
    boot_sample = sample(squared_error,
                         length(squared_error),
                         replace = T)
    res[i] = mean(boot_sample)
  }
  
  return(list(mse = mean((x-y_true)^2),
              ci = quantile(res, c(.025, .975)),
              dist = res))
}

#' Compute MSE table by missingness pattern
#'
#' Computes bootstrap MSE estimates for each training procedure,
#' overall and stratified by missingness pattern.
#'
#' @param predictions Named list of prediction vectors.
#' @param patterns Character vector of missingness patterns.
#' @param y_true True outcome values.
#'
#' @return A data frame summarizing sample size and MSE with
#' 95% bootstrap confidence intervals.
#' @export
compute_mse_table = function(predictions, patterns, y_true) {
  
  training_procedures = names(predictions)
  unique_patterns = unique(patterns)
  
  # Initialise result container
  result = matrix(
    NA,
    nrow = length(unique_patterns)+1,
    ncol = length(training_procedures)+1,
    dimnames = list(c("Overall",unique_patterns), c("N (%)",training_procedures))
  )
  
  # Initialise pattern_frequency, used to sort by pattern frequency
  pattern_frequency = c()
  
  for (pattern in c("Overall",unique_patterns)) {
    if(pattern == "Overall") {
      idx = 1:length(y_true)
    } else {
      idx = which(patterns == pattern)
    }
    
    n_pattern = length(idx)
    result[pattern, "N (%)"] = sprintf("%i (%.1f%%)",
                                       n_pattern,
                                       100*n_pattern/length(y_true))
    
    pattern_frequency = append(pattern_frequency, n_pattern)
    
    for (procedure in training_procedures) {
      
      boot_res = bootstrap_mse(
        x = predictions[[procedure]][idx],
        y_true = y_true[idx]
      )
      
      result[pattern, procedure] = sprintf(
        "%.3f (%.3f–%.3f)",
        boot_res[["mse"]],
        boot_res[["ci"]][1],
        boot_res[["ci"]][2]
      )
    }
  }
  result_table = as.data.frame(result)
  result_table = result_table[order(pattern_frequency, decreasing = T),]
  return(result_table)
}
