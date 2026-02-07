#' Simulate training and testing datasets under a missingness mechanism
#'
#' Generates independent training and testing datasets from a Gaussian
#' data-generating model for \code{X1}, \code{X2}, and \code{Y}, together with
#' missingness indicators for \code{X1} and \code{Y}. Missing values are not
#' introduced at this stage; only missingness indicators are generated.
#'
#' @param train_size Integer. Number of observations in the training dataset.
#' @param test_size Integer. Number of observations in the testing dataset.
#' @param theta List. Parameters of the data-generating model for
#'   \code{X1}, \code{X2}, and \code{Y}.
#' @param beta_phi Numeric vector. Coefficients of the logistic model governing
#'   missingness in \code{X1}, including the intercept.
#' @param missingness_target_X1 Numeric scalar in \eqn{[0,1]}. Target marginal
#'   missingness proportion for \code{X1}.
#' @param missingness_target_Y Numeric scalar in \eqn{[0,1]}. Target marginal
#'   missingness proportion for \code{Y}. Defaults to 0.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{train}{Data frame containing \code{X1}, \code{X2}, \code{Y} and
#'     missingness indicators \code{MX1} and \code{MY} for the training set.}
#'   \item{test}{Data frame with the same structure, generated independently
#'     for the testing set.}
#' }
#'
#' @details
#' The variables \code{X1} and \code{X2} are generated independently from
#' normal distributions specified in \code{theta}. The outcome \code{Y} is
#' generated from a linear Gaussian model conditional on \code{X1} and
#' \code{X2}.
#'
#' Missingness in \code{X1} is generated according to a logistic model with
#' linear predictor \code{beta_phi[1] + beta_phi[2] * X1 + beta_phi[3] * X2 +
#' beta_phi[4] * Y}. Missingness in \code{Y} is generated independently with
#' probability \code{missingness_target_Y}.
#'
#' The intercept of the missingness model may be tuned externally to ensure
#' that the marginal probability of missingness in \code{X1} matches
#' \code{missingness_target_X1} in expectation.
#'
#' The function returns fully observed variables together with missingness
#' indicators; conversion of indicators into \code{NA} values is handled
#' separately by \code{mask_data()}.
#'
#' @examples
#' \dontrun{
#' data <- simulate_data(
#'   train_size = 1000,
#'   test_size = 1000,
#'   theta = theta,
#'   beta_phi = beta_phi,
#'   missingness_target_X1 = 0.3
#' )
#' }
simulate_data = function (train_size,
                          test_size,
                          theta,
                          beta_phi,
                          missingness_target_X1,
                          missingness_target_Y = 0) {
  # Tests
  check_numeric_value(train_size,"train_size")
  check_numeric_value(test_size,"test_size")
  if(!(scenario %in% missingness_scenarios)){stop("`scenario` is invalid.")}
  check_numeric_value(missingness_target_X1,"missingness_target_X1")
  check_value_bounds(missingness_target_X1,
                     "missingness_target_X1",
                     c(0,1))
  check_numeric_value(missingness_target_Y)
  check_value_bounds(missingness_target_Y,
                     "missingness_target_Y",
                     c(0,1))
  
  # Function body
  
  result = list("train" = NULL,
                "test" = NULL)
  size = list("train" = train_size,
              "test"  = test_size)
  
  ## Generate variables X1, X2, Y
  for (set in names(result)){
    X1 = rnorm(train_size,
               theta[["X1"]][["beta"]],
               theta[["X1"]][["sigma"]])
    X2 = rnorm(train_size,
               theta[["X2"]][["beta"]],
               theta[["X2"]][["sigma"]])
    Y  = drop(base::cbind(1,X1,X2) %*% theta[["Y"]][["beta"]]) + 
      rnorm(train_size,0,theta[["Y"]][["sigma"]])
    
    #TODO: tune the missingness proportion
    
    # Generate missingness indicators
    eta_MX1 = (1+exp(-base::cbind(1,X1,X2,Y) %*% beta_phi))^-1
    MX1 = ifelse(runif(size[[set]]) > eta_MX1, 0, 1)
    MY = ifelse(runif(size[[set]]) > missingness_target_Y, 0, 1)
    result[[set]] = data.frame(X1,X2,Y,MX1,MY)
  }
  return(result)
}

#' Apply missingness indicators to a dataset
#'
#' Converts missingness indicators into missing values (\code{NA}) in the
#' corresponding variables, while preserving the indicators themselves.
#' The number of observations and the structure of the dataset are unchanged.
#'
#' @param dataset Data frame containing variables \code{X1}, \code{X2},
#'   \code{Y} and missingness indicators \code{MX1} and \code{MY}.
#' @param mask_Y Logical. Should missingness be applied to \code{Y} using the
#'   indicator \code{MY}? If \code{FALSE}, \code{Y} is returned fully observed.
#'
#' @return A data frame with the same number of rows as \code{dataset}, in which
#'   values of \code{X1} (and optionally \code{Y}) are set to \code{NA} according
#'   to the corresponding missingness indicators.
#'
#' @details
#' Missingness is applied as follows:
#' \itemize{
#'   \item If \code{MX1 == 1}, then \code{X1} is set to \code{NA}.
#'   \item If \code{mask_Y == TRUE} and \code{MY == 1}, then \code{Y} is set to
#'   \code{NA}.
#' }
#'
#' The missingness indicators \code{MX1} and \code{MY} are always preserved in
#' the output. No imputation is performed by this function.
#'
#' @examples
#' \dontrun{
#' data_full <- simulate_data(
#'   train_size = 100,
#'   test_size = 100,
#'   theta = theta,
#'   beta_phi = beta_phi,
#'   missingness_target_X1 = 0.3
#' )
#'
#' data_train <- mask_data(data_full$train, mask_Y = FALSE)
#' }
mask_data = function (dataset, mask_Y) {
  # Tests
  check_data_frame(dataset, "dataset")
  check_logical_value(mask_Y, "mask_Y")
  
  # Function body
  X1 = ifelse(dataset[["MX1"]] == 0, dataset[["X1"]], NA)
  if (mask_Y) {
    Y = ifelse(dataset[["MY"]] == 0, dataset[["Y"]], NA)
  } else {
    Y = dataset[["Y"]]
  }
  data.frame(
    X1  = X1,
    X2  = dataset[["X2"]],
    Y   = Y,
    MX1 = dataset[["MX1"]],
    MY  = dataset[["MY"]]
  )
}

#' Simulate from the joint distribution of (X1, X2, Y)
#'
#' Generates an i.i.d. sample from the joint Gaussian data-generating model
#' specified by \code{theta}. This function is primarily used for Monte Carlo
#' integration when computing oracle quantities.
#'
#' @param B Integer. Number of observations to generate.
#' @param theta List. Parameters of the joint distribution of \code{X1},
#'   \code{X2}, and \code{Y}.
#'
#' @return A data frame with \code{B} rows and columns \code{X1}, \code{X2},
#'   and \code{Y}.
#'
#' @details
#' The variables \code{X1} and \code{X2} are generated independently from normal
#' distributions. The outcome \code{Y} is generated from a linear Gaussian model
#' conditional on \code{X1} and \code{X2}.
simulate_joint_XY = function(B, theta) {
  
  X1 = rnorm(B, mean = theta$X1$beta, sd = theta$X1$sigma)
  X2 = rnorm(B, mean = theta$X2$beta, sd = theta$X2$sigma)
  
  Y =
    theta$Y$beta["(Intercept)"] +
    theta$Y$beta["X1"] * X1 +
    theta$Y$beta["X2"] * X2 +
    rnorm(B, 0, theta$Y$sigma)
  
  data.frame(X1 = X1, X2 = X2, Y = Y)
}

#' Tune the intercept of a missingness model to match a target proportion
#'
#' Calibrates the intercept of a logistic missingness model so that the
#' marginal probability of missingness matches a prespecified target, under
#' the joint data-generating model defined by \code{theta}.
#'
#' @param target_missingness Numeric scalar in \eqn{[0,1]}. Target marginal
#'   probability of missingness.
#' @param beta_phi Numeric vector. Coefficients of the missingness model
#'   excluding the intercept.
#' @param theta List. Parameters of the joint distribution of
#'   \code{X1}, \code{X2}, and \code{Y}.
#' @param B Integer. Monte Carlo sample size used to approximate expectations.
#'   Defaults to 50,000.
#' @param tol Numeric. Tolerance passed to the root-finding algorithm.
#'
#' @return Numeric scalar. Value of the intercept that approximately yields
#'   the target marginal missingness proportion.
#'
#' @details
#' The intercept is obtained by solving a one-dimensional root-finding problem
#' using Monte Carlo integration and \code{uniroot()}. The Monte Carlo sample
#' is fixed during optimization to ensure numerical stability.
#'
#' A large negative value is returned when \code{target_missingness == 0} to
#' avoid convergence issues.
tune_missingness_intercept = function(target_missingness,
                                      beta_phi,
                                      theta,
                                      B = 50000,
                                      tol = 1e-4) {
  
  check_numeric_value(target_missingness, "target_missingness")
  check_value_bounds(target_missingness, "target_missingness", c(0, 1))
  
  # Workaround to avoid absence of convergence when the target missingness is 0
  if(target_missingness == 0){
    return(-1e12)
    break
  }
  
  ## Monte Carlo sample (fixed for stability)
  joint_sample = simulate_joint_XY(B, theta)
  
  ## Linear predictor without intercept
  eta_no_intercept =
    beta_phi["X1"] * joint_sample$X1 +
    beta_phi["X2"] * joint_sample$X2 +
    beta_phi["Y"]  * joint_sample$Y
  
  ## Function whose root we want
  f = function(intercept) {
    mean(plogis(intercept + eta_no_intercept)) - target_missingness
  }
  
  ## Root finding
  res = uniroot(
    f,
    interval = c(-50, 20),
    tol = tol
  )
  
  return(res$root)
}