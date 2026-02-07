#' Generate predictions according to a specified training procedure
#'
#' Dispatches prediction to the appropriate backend depending on the selected
#' training procedure. This function provides a unified interface for obtaining
#' predictions from pattern-specific models, complete-case models, and
#' likelihood-based approaches.
#'
#' @param object Object containing fitted model components produced by a
#'   training procedure (e.g. pattern-specific submodels or estimated
#'   distributional parameters).
#' @param newdata Data frame containing covariates and missingness indicators
#'   required for prediction.
#' @param procedure_id Character scalar identifying the training procedure to
#'   be used. Supported values include 
#'   \begin{itemize}
#'   \item \code{"ps"}: Pattern Submodels
#'   \item \code{"ccs"}: Complete Cases Submdodels
#'   \item \code{"mle"}: Maximum Likelihood Estimation (using 
#'   Expectation-Maximisation), with marginalisation of the missing predictors
#'   \item \code{"mle_mi"}: Maximum Likelihood Estimation (using 
#'   Expectation-Maximisation), with marginalisation of the missing predictors,
#'   including missingness indicators
#'   \code{mi}: Multiple imputation
#'   \code{mimi}: Multiple imputation with missingness indicators
predict_Y = function(object,
                     newdata,
                     procedure_id) {
  check_data_frame(newdata, "newdata")
  check_character_value(procedure_id, "procedure_id")
  
  switch(
    procedure_id,
    
    "ps"  = predict_submodels(
      submodels = prediction_function,
      newdata = newdata
    ),
    
    "ccs" = predict_submodels(
      submodels = prediction_function,
      newdata = newdata
    ),
    
    "mle" = predict_mle(
      mle_parameters = prediction_function,
      newdata = newdata
    ),
    
    "mle_mi" = predict_mle_mi(
      mle_parameters = prediction_function,
      newdata = newdata
    ),
    
    "mi" = predict_mi(
      mi_model = prediction_function,
      newdata = newdata
    ),
    
    "mimi" = predict_mimi(
      mimi_model = prediction_function,
      newdata = newdata
    ),
    
    stop(sprintf(
      "Unknown procedure_id `%s`.", procedure_id
    ), call. = FALSE)
  )
}

#' Train pattern-specific linear submodels
#'
#' Fits separate linear regression models for each observed missingness pattern
#' defined by the indicator \code{MX1}. For each pattern, the regression model
#' includes only the covariates observed under that pattern.
#'
#' @param dataset Data frame containing outcome \code{Y}, covariates
#'   \code{X1}, \code{X2}, and the missingness indicator \code{MX1}.
#'
#' @return A named list of fitted \code{lm} objects. Each element corresponds
#'   to a missingness pattern and is indexed by the character representation
#'   of the pattern (e.g. \code{"0"}, \code{"1"}).
#'
#' @details
#' For each unique value of \code{MX1}, a separate linear model is fitted using
#' only observations belonging to that pattern. The set of predictors is
#' determined by the pattern: covariates with missingness indicators equal to
#' zero are included, while missing covariates are excluded from the model.
#'
#' This corresponds to a pattern-mixture modeling approach in which both the
#' model structure and the fitted parameters may differ across missingness
#' patterns.
#'
#' @examples
#' \dontrun{
#' submodels <- train_ps(data_train)
#' }
train_ps = function(dataset) {
  # Tests
  check_data_frame(dataset, "dataset")
  # Function body
  patterns = unique(dataset[["MX1"]])
  submodels  = setNames(vector("list", length(patterns)), patterns)
  for(pattern in patterns){
    indicators = c(pattern, 0)
    predictors = c("X1","X2")[indicators == 0]
    submodel_formula = reformulate(termlabels = predictors,
                                   response = "Y")
    dataset_subset = dataset[dataset[["MX1"]] == as.numeric(pattern),]
    submodels[[as.character(pattern)]] = lm(submodel_formula,dataset_subset)
  }
  return(submodels)
}

#' Train complete-case submodels by missingness pattern
#'
#' Fits linear regression models for each missingness pattern defined by
#' \code{MX1}, using complete cases with respect to the covariates included
#' in the corresponding model.
#'
#' @param dataset Data frame containing outcome \code{Y}, covariates
#'   \code{X1}, \code{X2}, and the missingness indicator \code{MX1}.
#'
#' @return A named list of fitted \code{lm} objects. Each element corresponds
#'   to a missingness pattern and is indexed by the character representation
#'   of the pattern.
#'
#' @details
#' For each missingness pattern, the set of predictors is determined in the
#' same way as in \code{train_ps()}, based on the observed covariates under
#' that pattern. However, model fitting is restricted to complete cases with
#' respect to the selected predictors.
#'
#' This procedure corresponds to a complete-case analysis performed separately
#' within each missingness pattern.
#'
#' @examples
#' \dontrun{
#' submodels <- train_ccs(data_train)
#' }
train_ccs = function(dataset) {
  # Tests
  check_data_frame(dataset, "dataset")
  # Function body
  patterns = unique(dataset[["MX1"]])
  submodels  = setNames(vector("list", length(patterns)), patterns)
  for(pattern in patterns){
    indicators = c(pattern, 0)
    predictors = c("X1","X2")[indicators == 0]
    submodel_formula = reformulate(termlabels = predictors,
                                   response = "Y")
    dataset_subset = dataset[complete.cases(dataset[,predictors]),]
    submodels[[as.character(pattern)]] = lm(submodel_formula,dataset_subset)
  }
  return(submodels)
}

#' Predict outcomes using pattern-specific submodels
#'
#' Generates predictions by applying pattern-specific regression models to
#' the corresponding subsets of a dataset, as defined by the missingness
#' indicator \code{MX1}. Used both by Pattern Submodels and Complete Cases 
#' Submodels
#'
#' @param submodels Named list of fitted \code{lm} objects, typically produced
#'   by \code{train_ps()} or \code{train_ccs()}. Names must correspond to
#'   missingness patterns.
#' @param newdata Data frame containing covariates and the missingness
#'   indicator \code{MX1}.
#'
#' @return A numeric vector of predicted values, with length equal to the
#'   number of rows in \code{newdata}. Predictions are returned in the original
#'   row order.
#'
#' @details
#' For each unique value of \code{MX1} in \code{newdata}, predictions are
#' obtained using the corresponding submodel. If a missingness pattern is
#' present in \code{newdata} but no matching submodel is available, a warning
#' is issued and \code{NA} values are returned for observations with that
#' pattern.
#'
#' @examples
#' \dontrun{
#' predictions <- predict_submodels(submodels, data_test)
#' }
predict_submodels = function(submodels, newdata){
  # Tests
  check_data_frame(newdata, "newdata")
  
  # Function body
  patterns = unique(newdata[["MX1"]])
  predictions = rep(NA,nrow(newdata))
  
  for(pattern in patterns){
    if(!(pattern %in% names(submodels))){
      warning(sprintf("pattern `%s` is present in the dataset but no corresponding
                   submodel is available. NA will be rendered for that pattern.",
                      pattern), call. = FALSE)
      next
    }
    idx = newdata[["MX1"]] == pattern
    predictions[idx] = predict(submodels[[as.character(pattern)]],
                               newdata = newdata[idx,])
  }
  return(predictions)
}

#' Estimate Gaussian model parameters by maximum likelihood
#'
#' Estimates the mean vector and covariance matrix of a multivariate normal
#' distribution for \code{(X1, X2, Y)} using maximum likelihood under missing
#' data, via the EM algorithm.
#'
#' @param dataset Data frame containing variables \code{X1}, \code{X2}, and
#'   \code{Y}. Missing values are allowed.
#'
#' @return A list with two components:
#' \describe{
#'   \item{mu}{Named numeric vector giving the estimated mean of
#'     \code{X1}, \code{X2}, and \code{Y}.}
#'   \item{sigma}{Named covariance matrix of \code{(X1, X2, Y)}. Rows and
#'     columns are ordered as \code{X1}, \code{X2}, \code{Y}.}
#' }
#'
#' @details
#' The estimation is performed under the assumption that
#' \eqn{(X_1, X_2, Y)} follows a multivariate normal distribution. Missing values
#' are handled using the EM algorithm as implemented in the
#' \code{\link[Norm]{em.norm}} routine.
#'
#' This function returns only the estimated distributional parameters and does
#' not perform any prediction. Conditional expectations used for prediction are
#' computed separately by \code{predict_mle()}.
#'
#' @examples
#' \dontrun{
#' mle_parameters <- train_mle(data_train)
#' }
train_mle = function(dataset) {
  #Tests
  check_data_frame(dataset, "dataset")
  
  #Function body
  variable_names = c("X1","X2","Y")
  dataset_as_matrix = as.matrix(dataset[,variable_names])
  preliminary_prepared_dataset = prelim.norm(dataset_as_matrix)
  mle_estimates  = em.norm(preliminary_prepared_dataset,showits=F)
  mle_parameters = getparam.norm(preliminary_prepared_dataset,mle_estimates)
  names(mle_parameters[["mu"]]) = c("X1","X2","Y")
  colnames(mle_parameters[["sigma"]]) = c("X1","X2","Y")
  rownames(mle_parameters[["sigma"]]) = c("X1","X2","Y")
  return(mle_parameters)
}

#' Predict outcome using Gaussian MLE parameters, marginalising over missing
#' predictors
#'
#' Computes predictions of \code{Y} based on estimated Gaussian model
#' parameters, using conditional expectations given observed covariates.
#'
#' @param mle_parameters List containing elements \code{mu} and \code{sigma},
#'   as returned by \code{train_mle()}.
#' @param newdata Data frame containing covariates \code{X1}, \code{X2}, and
#'   the missingness indicator \code{MX1}.
#'
#' @return A numeric vector of predicted values for \code{Y}, with length equal
#'   to the number of rows in \code{newdata}.
#'
#' @details
#' Predictions are obtained as conditional expectations under a multivariate
#' normal model. When both \code{X1} and \code{X2} are observed, the prediction
#' is
#' \deqn{
#'   \mathbb{E}(Y \mid X_1, X_2).
#' }
#'
#' When \code{X1} is missing (i.e., \code{MX1 == 1}), predictions are instead
#' computed using
#' \deqn{
#'   \mathbb{E}(Y \mid X_2).
#' }
#'
#' This corresponds to optimal prediction under squared-error loss in the
#' Gaussian model.
#'
#' @examples
#' \dontrun{
#' predictions <- predict_mle(mle_parameters, data_test)
#' }
predict_mle = function(mle_parameters, newdata){
  # Tests
  check_data_frame(newdata, "newdata")
  # Function body
  predictors = c("X1","X2")
  idx1 = newdata[["MX1"]] == 1
  
  mu = mle_parameters[["mu"]]
  sigma_XX = mle_parameters[["sigma"]][predictors, predictors]
  sigma_YX = mle_parameters[["sigma"]]["Y",predictors]
  
  X_centered = base::cbind(newdata[["X1"]] - mu["X1"],
                     newdata[["X2"]] - mu["X2"])
  
  predictions = as.vector(mu["Y"] + X_centered %*% solve(sigma_XX) %*% sigma_YX)
  
  sigma_YX2 = mle_parameters[["sigma"]]["Y","X2"]
  sigma_X2 = mle_parameters[["sigma"]]["X2","X2"]
  
  predictions_Y_X2 = mu[["Y"]] + sigma_YX2 / 
    sigma_X2 * (newdata[["X2"]] - mu[["X2"]])
  predictions[idx1] = predictions_Y_X2[idx1]
  return(predictions)
}

#' Fit a Gaussian MLE model including missingness indicators
#'
#' Fits a multivariate normal model by maximum likelihood using the EM
#' algorithm, including missingness indicators as additional variables in the
#' joint model. This allows the estimation to account for informative
#' missingness through explicit modeling of the missingness process.
#'
#' @param dataset Data frame containing variables \code{X1}, \code{X2},
#'   \code{Y}, and the missingness indicator \code{MX1}.
#'
#' @return A list containing the estimated parameters of the joint Gaussian
#' model:
#' \describe{
#'   \item{mu}{Named numeric vector of estimated means.}
#'   \item{sigma}{Estimated covariance matrix.}
#'   \item{no_missingness_flag}{Logical. Indicates whether the estimation was
#'     performed without missingness indicators because no missing values were
#'     present in the data.}
#' }
#'
#' @details
#' When no missingness is present in the dataset (\code{MX1 == 0} for all
#' observations), the function falls back to \code{train_mle()} and sets
#' \code{no_missingness_flag = TRUE}.
#'
#' Otherwise, the missingness indicator \code{MX1} is included as an additional
#' variable in the joint Gaussian model, and parameters are estimated using the
#' EM algorithm implemented in \pkg{norm}.
#'
#' This approach corresponds to likelihood-based estimation under a model that
#' explicitly incorporates missingness indicators.
#'
#' @seealso \code{\link{train_mle}}
train_mle_mi = function(dataset) {
  #Tests
  check_data_frame(dataset, "dataset")
  
  #Function body
  
  # If no missing data at estimation
  if(all(dataset[["MX1"]] == 0)){
    result = train_mle(dataset)
    result[["no_missingness_flag"]] = TRUE
    return(result)
  }else{
    variable_names = c("X1","X2","Y","MX1")
    dataset_as_matrix = as.matrix(dataset[,variable_names])
    preliminary_prepared_dataset = prelim.norm(dataset_as_matrix)
    mle_estimates  = em.norm(preliminary_prepared_dataset,showits=F)
    mle_parameters = getparam.norm(preliminary_prepared_dataset,mle_estimates)
    names(mle_parameters[["mu"]]) = variable_names
    colnames(mle_parameters[["sigma"]]) = variable_names
    rownames(mle_parameters[["sigma"]]) = variable_names
    result = mle_parameters
    result[["no_missingness_flag"]] = FALSE
    return(result)
  }
  #TODO: see if we can use train_mle and add an option "missingness indicators = T"
}

#' Predict outcomes from a Gaussian MLE model with missingness indicators
#'
#' Computes predictions of the outcome \code{Y} from a multivariate Gaussian
#' model fitted by maximum likelihood with missingness indicators included as
#' additional variables.
#'
#' @param mle_parameters List. Output of \code{train_mle_mi()}, containing the
#'   estimated mean vector \code{mu}, covariance matrix \code{sigma}, and the
#'   logical flag \code{no_missingness_flag}.
#' @param newdata Data frame containing variables \code{X1}, \code{X2}, and the
#'   missingness indicator \code{MX1}.
#'
#' @return A numeric vector of predicted values of \code{Y}, with one entry per
#'   row of \code{newdata}.
#'
#' @details
#' When \code{no_missingness_flag == TRUE}, predictions are obtained by calling
#' \code{predict_mle()} directly.
#'
#' Otherwise, predictions are computed using conditional expectations under the
#' fitted joint Gaussian model:
#' \itemize{
#'   \item If \code{MX1 == 0}, predictions correspond to
#'     \eqn{E(Y \mid X_1, X_2, MX_1 = 0)}.
#'   \item If \code{MX1 == 1}, predictions correspond to
#'     \eqn{E(Y \mid X_2, MX_1 = 1)}.
#' }
#'
#' The appropriate conditional expectation is applied observation-wise
#' according to the observed missingness pattern.
#'
#' @seealso \code{\link{train_mle_mi}}, \code{\link{predict_mle}}
predict_mle_mi = function(mle_parameters, newdata) {
  if(mle_parameters[["no_missingness_flag"]]){
    return(predict_mle(mle_parameters, newdata))
  }
  idx1 = newdata[["MX1"]] == 1
  mu     = mle_parameters[["mu"]]
  Sigma  = mle_parameters[["sigma"]]
  
  #### 1. CASE M1 = 0 → predict E[Y|X1, X2, M1=0]
  
  predictors_MX1_0 = c("X1", "X2", "MX1")
  
  sigma_XX = Sigma[predictors_MX1_0, predictors_MX1_0]
  sigma_YX = Sigma["Y", predictors_MX1_0]
  
  X_centered_full = base::cbind(
    newdata[["X1"]]  - mu["X1"],
    newdata[["X2"]]  - mu["X2"],
    newdata[["MX1"]] - mu["MX1"]
  )

  pred_full = as.vector(mu["Y"] + X_centered_full %*% solve(sigma_XX) %*% sigma_YX)
  

  #### 2. CASE M1 = 1 → predict E[Y|X2,M1=1]
  predictors_MX1_1 = c("X2", "MX1")
  
  sigma_X2M1 = Sigma[predictors_MX1_1, predictors_MX1_1]
  sigma_YX2M1 = Sigma["Y", predictors_MX1_1]
  
  X_centered_X2M1 = base::cbind(
    newdata[["X2"]]  - mu["X2"],
    newdata[["MX1"]] - mu["MX1"]
  )
  
  pred_marg = as.vector(mu["Y"] + 
                          X_centered_X2M1 %*% solve(sigma_X2M1) %*% sigma_YX2M1)
  
  
  #### 3. Combine predictions
  predictions = pred_full
  predictions[idx1] = pred_marg[idx1]
  
  return(predictions)
}

#' Train prediction models using multiple imputation
#'
#' Fits prediction models based on multiple imputation of missing values using
#' the \pkg{mice} framework. For each imputed dataset, a prediction model for
#' \code{Y} is fitted and stored.
#'
#' @param dataset Data frame containing variables \code{X1}, \code{X2}, and
#'   \code{Y}, potentially with missing values.
#' @param m Integer. Number of multiple imputations. Defaults to 5.
#' @param method Character. Imputation method passed to \code{mice()}.
#'   Defaults to \code{"norm"}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{m}{Number of imputations.}
#'   \item{impModel}{List of fitted imputation models for \code{X1}, one per
#'     imputed dataset.}
#'   \item{predModel}{List of fitted prediction models for \code{Y}, one per
#'     imputed dataset.}
#' }
#'
#' @details
#' Missing values are imputed using \code{mice()}. For each completed dataset,
#' an imputation model for \code{X1} conditional on \code{X2} and a prediction
#' model for \code{Y} conditional on \code{X1} and \code{X2} are fitted using
#' linear regression.
#'
#' Predictions from these models are typically combined at the prediction
#' stage rather than by pooling coefficients.
#'
#' @seealso \code{\link{predict_mi}}, \code{\link{mice}}
train_mi = function(dataset, m = 5, method = "norm") {
  variables = c("X1","X2","Y")
  imp = mice(dataset[,variables], m = m, method = method, printFlag = F)
  result = list(m = m,
                impModel = list(),
                predModel = list())
  for(impSet in 1:m){
    result[["impModel"]][[impSet]] = lm(formula("X1 ~ X2"),
                                        data = complete(imp,impSet))
    result[["predModel"]][[impSet]] = lm(formula("Y ~ X1+X2"),
                                         data = complete(imp,impSet))
  }
  return(result)
  #TODO: see if interaction terms may be relevant
}

#' Predict outcomes using multiple imputation models
#'
#' Generates predictions of the outcome \code{Y} from a set of models fitted on
#' multiply imputed datasets, and pools predictions across imputations by
#' averaging.
#'
#' @param mi_model List. Output of \code{train_mi()}, containing imputation
#'   models and prediction models for each imputed dataset.
#' @param newdata Data frame containing variables \code{X1}, \code{X2}, and the
#'   missingness indicator \code{MX1}.
#'
#' @return A numeric vector of pooled predictions of \code{Y}, with one entry
#'   per row of \code{newdata}.
#'
#' @details
#' For observations with missing \code{X1} (\code{MX1 == 1}), \code{X1} is first
#' imputed using the corresponding imputation model in each imputed dataset.
#' Predictions of \code{Y} are then obtained using the fitted prediction model
#' for that imputed dataset.
#'
#' Final predictions are computed by averaging predictions across all
#' imputations. Coefficients are not pooled.
#'
#' @seealso \code{\link{train_mi}}, \code{\link{mice}}
predict_mi = function(mi_model, newdata) {
  idx1 = newdata[["MX1"]] == 1
  result = list()
  for(m in 1:mi_model[["m"]]){
    newdata[idx1,"X1"] = predict(mi_model[["impModel"]][[m]],
                                 newdata = newdata[idx1,])
    result[[m]] = predict(mi_model[["predModel"]][[m]],
                          newdata = newdata[,c("X1","X2")])
  }
  predictions = Reduce(`+`, result) / length(result)
  return(as.numeric(predictions))
}

#' Train prediction models using multiple imputation with missingness indicators
#'
#' Fits prediction models based on multiple imputation while explicitly
#' incorporating missingness indicators as predictors. This approach allows
#' the imputation and prediction stages to account for informative missingness.
#'
#' @param dataset Data frame containing variables \code{X1}, \code{X2},
#'   \code{Y}, and the missingness indicator \code{MX1}.
#' @param m Integer. Number of multiple imputations. Defaults to 5.
#' @param method Character. Imputation method passed to \code{mice()}.
#'   Defaults to \code{"norm"}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{m}{Number of imputations.}
#'   \item{impModel}{List of imputation models for \code{X1} fitted on observations
#'     with \code{MX1 == 1}.}
#'   \item{impModelWithoutMI}{List of imputation models for \code{X1} fitted as if
#'     \code{X1} were always observed at training.}
#'   \item{predModel}{List of prediction models for \code{Y}, including
#'     \code{MX1} as a predictor, one per imputed dataset.}
#' }
#'
#' @details
#' Multiple imputation is performed using \pkg{mice}. The predictor matrix is
#' constructed so that missingness indicators are not themselves imputed and do
#' not act as predictors in the imputation models.
#'
#' For each imputed dataset, a prediction model of \code{Y} on \code{X1},
#' \code{X2}, and \code{MX1} is fitted. Imputation models for \code{X1} are fitted
#' conditionally on \code{X2}, with special handling when no missingness is
#' present.
#'
#' Predictions from these models are typically pooled at the prediction stage
#' rather than by pooling coefficients.
#'
#' @seealso \code{\link{predict_mimi}}, \code{\link{train_mi}}, \code{\link{mice}}
train_mimi = function(dataset, m = 5, method = "norm") {
  variables = c("X1","X2","Y","MX1")
  idx1 = dataset[["MX1"]] == 1
  
  # Prepare the predictor matrix, describing which variables are involved in
  # each imputation model.
  predictor_matrix = make.predictorMatrix(dataset[,variables])
  # imputation model for X1 is estimated only on cases with MX1 == 0
  predictor_matrix[,"MX1"] = 0
  predictor_matrix["MX1",] = 0
  
  imp = mice(dataset[,variables],
             m = m,
             method = method,
             predictorMatrix = predictor_matrix,
             printFlag = F)
  
  result = list(m = m,
                impModel = list(),
                impModelWithoutMI = list(), # Model used to impute X1 if it was always observed at training
                predModel = list())
  
  for(impSet in 1:m){
    if(any(idx1)){
      result[["impModel"]][[impSet]] = lm(formula("X1 ~ X2"),
                                          data = complete(imp,impSet)[idx1,])
    }else{
      result[["impModel"]][[impSet]] = NULL
    }
    result[["impModelWithoutMI"]][[impSet]] = lm(formula("X1~X2"),
                                                 data = complete(imp,impSet))
    result[["predModel"]][[impSet]] = lm(formula("Y ~ X1+X2+MX1"),
                                         data = complete(imp,impSet))
    #TODO: check if interaction terms are useful
  }
  return(result)
}

#' Predict outcomes using multiple imputation with missingness indicators
#'
#' Generates predictions of the outcome \code{Y} from models fitted on multiply
#' imputed datasets that explicitly incorporate missingness indicators, and
#' pools predictions across imputations by averaging.
#'
#' @param mimi_model List. Output of \code{train_mimi()}, containing imputation
#'   models, prediction models, and the number of imputations.
#' @param newdata Data frame containing variables \code{X1}, \code{X2}, and the
#'   missingness indicator \code{MX1}.
#'
#' @return A numeric vector of pooled predictions of \code{Y}, with one entry
#'   per row of \code{newdata}.
#'
#' @details
#' For observations with missing \code{X1} (\code{MX1 == 1}), \code{X1} is
#' imputed within each imputed dataset prior to prediction. When imputation
#' models for missing \code{X1} are unavailable (i.e. no missingness was
#' present at estimation), a fallback imputation model fitted without
#' missingness indicators is used and a warning is issued.
#'
#' Predictions of \code{Y} are then obtained from models that include
#' \code{MX1} as a predictor. Final predictions are computed by averaging
#' predictions across imputations. Coefficients are not pooled.
#'
#' @seealso \code{\link{train_mimi}}, \code{\link{predict_mi}}, \code{\link{mice}}
predict_mimi = function(mimi_model, newdata) {
  idx1 = newdata[["MX1"]] == 1
  result = list()
  
  for(m in 1:mimi_model[["m"]]){
    if(any(idx1)){
      if(length(mimi_model[["impModel"]]) > 0){
        newdata[idx1,"X1"] = predict(mimi_model[["impModel"]][[m]],
                                                    newdata = newdata[idx1,])
      }else{
        warning("Missing values are present at testing but not at estimation.")
        newdata[idx1,"X1"] = predict(mimi_model[["impModelWithoutMI"]][[m]],
                                                    newdata = newdata[idx1,])
      }
    }
    result[[m]] = predict(mimi_model[["predModel"]][[m]],
                          newdata = newdata[,c("X1","X2","MX1")])
  }
  predictions = Reduce(`+`, result) / length(result)
  return(as.numeric(predictions))
}