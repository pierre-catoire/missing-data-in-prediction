#' Simulation study for prediction under missing data mechanisms
#'
#' This package contains the full simulation code used to evaluate prediction
#' methods under various missing data mechanisms. It implements data-generating
#' models with controlled missingness, multiple training procedures, oracle
#' reference predictors, and performance evaluation pipelines.
#'
#' The code is designed for reproducibility and transparency, and accompanies
#' a methodological study on prediction under missing covariates.
#'
#' @details
#' The simulation framework includes:
#' \itemize{
#'   \item Generation of training and testing datasets with controlled
#'     missingness mechanisms.
#'   \item Estimation procedures based on complete cases, pattern submodels,
#'     likelihood-based methods, and multiple imputation.
#'   \item Oracle reference predictors computed under the true data-generating
#'     and missingness mechanisms.
#'   \item Evaluation of predictive performance using MSE and oracle-based
#'     MSEP criteria.
#' }
#'
#' This package is not intended for general-purpose use and is distributed
#' solely for reproducibility of the simulation study.
#'
#' @author
#' Pierre Catoire \email{pierre@@pierre-catoire.page}
#'
#' @seealso
#' \code{\link{simulate_data}},
#' \code{\link{evaluate_performance}},
#' \code{\link{write_results_tables}}
"_PACKAGE"