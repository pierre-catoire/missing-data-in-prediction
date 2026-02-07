#' Compute oracle conditional expectations for a single observation
#'
#' Computes oracle conditional expectations of the outcome \code{Y} under the
#' true data-generating and missingness mechanisms, for a single observation
#' characterized by \code{(X1, X2, MX1)}.
#'
#' @param X1 Numeric scalar. Value of covariate \code{X1}.
#' @param X2 Numeric scalar. Value of covariate \code{X2}.
#' @param MX1 Integer scalar (0 or 1). Missingness indicator for \code{X1}.
#' @param theta List. Parameters of the joint data-generating model for
#'   \code{X1}, \code{X2}, and \code{Y}.
#' @param beta_phi Numeric vector. Coefficients of the logistic missingness model
#'   for \code{MX1}, including the intercept.
#' @param B Integer. Monte Carlo sample size used to approximate conditional
#'   expectations. Defaults to 5,000.
#'
#' @return A named numeric vector with the following elements:
#' \describe{
#'   \item{EY_X1X2}{Oracle value of \eqn{E(Y \mid X_1, X_2)}.}
#'   \item{EY_X2}{Oracle value of \eqn{E(Y \mid X_2)}.}
#'   \item{EY_X1X2_MX1}{Oracle value of \eqn{E(Y \mid X_1, X_2, MX_1)}.}
#'   \item{EY_X2_MX1}{Oracle value of \eqn{E(Y \mid X_2, MX_1)}.}
#' }
#'
#' @details
#' The expectations conditional on the missingness indicator \code{MX1} are
#' approximated using Monte Carlo integration under the true missingness model.
#' The weighting scheme corresponds to conditioning on the observed value of
#' \code{MX1}.
#'
#' This function is intended for internal use when computing oracle reference
#' predictors in simulation studies.
oracle_reference_one = function(X1,
                                X2,
                                MX1,
                                theta,
                                beta_phi,
                                B = 5000) {
  
  ## --------------------------------------------------------------------------
  ## 1. E(Y | X1, X2)
  ## --------------------------------------------------------------------------
  EY_X1X2 =
    theta[["Y"]][["beta"]]["(Intercept)"] +
    theta[["Y"]][["beta"]]["X1"] * X1 +
    theta[["Y"]][["beta"]]["X2"] * X2
  
  ## --------------------------------------------------------------------------
  ## 2. E(Y | X2)
  ## --------------------------------------------------------------------------
  EY_X2 =
    theta[["Y"]][["beta"]]["(Intercept)"] +
    theta[["Y"]][["beta"]]["X1"] * theta[["X1"]][["beta"]] +
    theta[["Y"]][["beta"]]["X2"] * X2
  
  ## --------------------------------------------------------------------------
  ## 3. E(Y | X1, X2, MX1)  (MC integration)
  ## --------------------------------------------------------------------------
  Y_draws =
    rnorm(B, mean = EY_X1X2, sd = theta[["Y"]][["sigma"]])
  
  linpred =
    beta_phi["(Intercept)"] +
    beta_phi["X1"] * X1 +
    beta_phi["X2"] * X2 +
    beta_phi["Y"]  * Y_draws
  
  p_MX1 = plogis(linpred)
  
  weights =
    if (MX1 == 1) p_MX1 else (1 - p_MX1)
  
  EY_X1X2_MX1 =
    sum(Y_draws * weights) / sum(weights)
  
  ## --------------------------------------------------------------------------
  ## 4. E(Y | X2, MX1)  (MC integration over X1 and Y)
  ## --------------------------------------------------------------------------
  X1_draws =
    rnorm(B, mean = theta[["X1"]][["beta"]], sd = theta[["X1"]][["sigma"]])
  
  Y_draws2 =
    rnorm(
      B,
      mean =
        theta[["Y"]][["beta"]]["(Intercept)"] +
        theta[["Y"]][["beta"]]["X1"] * X1_draws +
        theta[["Y"]][["beta"]]["X2"] * X2,
      sd = theta[["Y"]][["sigma"]]
    )
  
  linpred2 =
    beta_phi["(Intercept)"] +
    beta_phi["X1"] * X1_draws +
    beta_phi["X2"] * X2 +
    beta_phi["Y"]  * Y_draws2
  
  p_MX1_2 = plogis(linpred2)
  
  weights2 =
    if (MX1 == 1) p_MX1_2 else (1 - p_MX1_2)
  
  EY_X2_MX1 =
    sum(Y_draws2 * weights2) / sum(weights2)
  
  ## --------------------------------------------------------------------------
  ## Return
  ## --------------------------------------------------------------------------
  c(
    EY_X1X2     = EY_X1X2,
    EY_X2       = EY_X2,
    EY_X1X2_MX1 = EY_X1X2_MX1,
    EY_X2_MX1   = EY_X2_MX1
  )
}

#' Compute oracle reference probabilities for a dataset
#'
#' Computes oracle conditional expectations of the outcome \code{Y} for each
#' observation in a dataset, under the true data-generating and missingness
#' mechanisms.
#'
#' @param dataset Data frame containing at least the variables \code{X1},
#'   \code{X2}, and the missingness indicator \code{MX1}.
#' @param theta List. Parameters of the joint data-generating model for
#'   \code{X1}, \code{X2}, and \code{Y}.
#' @param beta_phi Numeric vector. Coefficients of the logistic missingness model
#'   for \code{MX1}, including the intercept.
#' @param B Integer. Monte Carlo sample size used to approximate oracle
#'   expectations for each observation. Defaults to 5,000.
#' @param parallel Logical. Should computations be parallelized over
#'   observations using the \pkg{future.apply} framework? Defaults to
#'   \code{TRUE}.
#'
#' @return A data frame with one row per observation in \code{dataset} and the
#' following columns:
#' \describe{
#'   \item{EY_X1X2}{Oracle value of \eqn{E(Y \mid X_1, X_2)}.}
#'   \item{EY_X2}{Oracle value of \eqn{E(Y \mid X_2)}.}
#'   \item{EY_X1X2MX1}{Oracle value of \eqn{E(Y \mid X_1, X_2, MX_1)}.}
#'   \item{EY_X2MX1}{Oracle value of \eqn{E(Y \mid X_2, MX_1)}.}
#' }
#'
#' @details
#' Oracle quantities are computed observation-wise using Monte Carlo
#' integration, by calling \code{oracle_reference_one()} for each row of the
#' dataset. When \code{parallel = TRUE}, computations are parallelized across
#' observations using \pkg{future.apply}.
#'
#' This function is intended for simulation studies and provides reference
#' predictors that are not available in practice.
#'
#' @seealso \code{\link{oracle_reference_one}}
compute_reference_probabilities = function(dataset,
                                           theta,
                                           beta_phi,
                                           B = 5000,
                                           parallel = TRUE) {
  
  ## Tests
  check_data_frame(dataset, "dataset")
  
  required_vars = c("X1", "X2", "MX1")
  if (!all(required_vars %in% names(dataset))) {
    stop("`dataset` must contain X1, X2 and MX1.", call. = FALSE)
  }
  
  ## Parallel backend
  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop("Package `future.apply` is required for parallel computation.",
           call. = FALSE)
    }
    future::plan(future::multisession)
  } else {
    future::plan(future::sequential)
  }
  
  ## Oracle computation (parallelised over observations)
  res =
    future.apply::future_lapply(
      seq_len(nrow(dataset)),
      function(i) {
        oracle_reference_one(
          X1         = dataset[["X1"]][i],
          X2         = dataset[["X2"]][i],
          MX1        = dataset[["MX1"]][i],
          theta      = theta,
          beta_phi   = beta_phi,
          B          = B
        )
      },
      future.seed = TRUE
    )
  
  ## Assemble output
  reference_probabilities =
    as.data.frame(do.call(base::rbind, res))
  
  rownames(reference_probabilities) = NULL
  colnames(reference_probabilities) = c("EY_X1X2",
                                        "EY_X2",
                                        "EY_X1X2MX1",
                                        "EY_X2MX1")
  
  return(reference_probabilities)
}