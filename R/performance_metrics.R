#' Compute mean squared difference
#'
#' Computes the mean squared difference between a reference vector and a vector
#' of predictions. Both vectors must be numeric, of equal length, and contain
#' no missing values. The same function is used to compute Mean Squared Error
#' (MSE) and Mean Squared Error of Prediction (MSEP)
#'
#' @param reference Numeric vector. Reference values (e.g. true outcomes or
#'   oracle predictions).
#' @param prediction Numeric vector. Predicted values to be evaluated.
#'
#' @return A numeric scalar giving the mean squared error.
#'
#' @details
#' The mean squared difference is defined as
#' \deqn{
#'   \mathrm{MSD} = \frac{1}{n} \sum_{i=1}^n (r_i - \hat r_i)^2,
#' }
#' where \eqn{r_i} denotes the reference value and \eqn{\hat r_i} the prediction
#' for observation \eqn{i}.
#'
#' @examples
#' compute_mse(
#'   reference  = c(1, 2, 3),
#'   prediction = c(1.1, 1.9, 3.2)
#' )
compute_msd = function(reference, prediction) {
  # Tests
  if (anyNA(reference)){
    stop("`reference` contains NA values.")
  }
  if (!is.numeric(reference)){
    stop("`reference` must be numeric.")
  }
  if (anyNA(prediction)){
    stop("`prediction` contains NA values.")
  }
  if (!is.numeric(prediction)){
    stop("`prediction` must be numeric.")
  }
  if(length(reference) != length(prediction)){
    stop("`reference` and `prediction` must have the same length.")
  }
  return(mean((reference-prediction)^2))
}

#' Evaluate predictive performance of all methods and oracle references
#'
#' Computes predictive performance metrics for a set of prediction methods and
#' oracle reference predictors, stratified by missingness pattern.
#'
#' @param simulation_object List. Output of a single simulation run, containing
#'   the test data, predictions from all methods, oracle reference quantities,
#'   and metadata.
#'
#' @return A nested list with one element per analysis group
#' (\code{"overall"}, \code{"complete"}, \code{"incomplete"}). Each group
#' contains named numeric vectors of performance values for the following
#' metrics:
#' \describe{
#'   \item{mse}{Mean squared error with respect to the true outcome \code{Y}.}
#'   \item{msep_omu}{Mean squared error of prediction with respect to the oracle
#'     predictor ignoring missingness (\code{refOMU}).}
#'   \item{msep_omc}{Mean squared error of prediction with respect to the oracle
#'     predictor conditioning on missingness (\code{refOMC}).}
#' }
#'
#' @details
#' Performance is evaluated separately for:
#' \itemize{
#'   \item all test observations (\code{"overall"}),
#'   \item observations with fully observed \code{X1} (\code{"complete"}),
#'   \item observations with missing \code{X1} (\code{"incomplete"}).
#' }
#'
#' All estimators and oracle reference predictors (\code{refMU}, \code{refMC})
#' are evaluated on an equal footing. The function returns no aggregated or
#' smoothed quantities; it operates at the level of a single simulation run.
#'
#' @seealso \code{\link{compute_msd}}, \code{\link{compute_reference_probabilities}}
evaluate_performance = function(simulation_object) {
  
  ## Extract components
  data_test  = simulation_object$data$test
  Y_true     = data_test$Y
  MX1        = data_test$MX1
  
  predictions = simulation_object$predictions
  references  = simulation_object$reference_probabilities[c("refMU","refMC")]
  oracles     = simulation_object$reference_probabilities[c("refOMU","refOMC")]
  
  ## Define groups
  groups = list(
    overall    = rep(TRUE, length(Y_true)),
    complete   = MX1 == 0,
    incomplete = MX1 == 1
  )
  
  ## Initialize output
  out = vector("list", length(groups))
  names(out) = names(groups)
  
  ## Loop over groups
  for (g in names(groups)) {
    
    idx = groups[[g]]
    
    out[[g]] = list()
    
    ## MSE with respect to Y
    out[[g]][["mse"]] =
      sapply(c(predictions,references), function(pred) {
        compute_msd(Y_true[idx], pred[idx])
      })
    
    ## MSEP wrt oracle ignoring MX1 (REFMU)
    out[[g]][["msep_omu"]] =
      sapply(c(predictions,references), function(pred) {
        compute_msd(oracles$refOMU[idx], pred[idx])
      })
    
    ## MSEP wrt oracle conditioning on MX1 (REFMC)
    out[[g]][["msep_omc"]] =
      sapply(c(predictions,references), function(pred) {
        compute_msd(oracles$refOMC[idx], pred[idx])
      })
  }
  
  return(out)
}

#' Smooth a performance series using LOESS
#'
#' Fits a locally weighted regression (LOESS) of a response variable on a
#' covariate and predicts the smoothed values on a specified grid. Extreme
#' covariate values can be trimmed prior to smoothing to reduce boundary
#' effects.
#'
#' @param x Numeric vector. Covariate values (e.g. observed missingness
#'   proportions).
#' @param y Numeric vector. Response values to be smoothed (e.g. performance
#'   metrics).
#' @param xloess Numeric vector. Grid of covariate values at which the smoothed
#'   curve is evaluated.
#' @param span Numeric. LOESS smoothing parameter. Defaults to 0.75.
#' @param trim Numeric vector of length 2. Lower and upper quantiles used to
#'   trim extreme values of \code{x} before fitting. Defaults to
#'   \code{c(0.02, 0.98)}.
#'
#' @return Numeric vector of smoothed values evaluated at \code{xloess}.
#'
#' @details
#' Missing values in \code{x} or \code{y} are removed prior to fitting.
#' The LOESS fit uses a local linear model (\code{degree = 1}).
loess_smooth_series = function(x,
                               y,
                               xloess,
                               span = 0.75,
                               trim = c(0.02, 0.98)) {
  
  ## Remove NA
  ok = !is.na(x) & !is.na(y)
  x = x[ok]
  y = y[ok]
  
  ## Trim extremes
  q = quantile(x, probs = trim)
  keep = (x >= q[1]) & (x <= q[2])
  
  x = x[keep]
  y = y[keep]
  
  ## Fit loess
  fit = loess(
    y ~ x,
    span = span,
    degree = 1,
    control = loess.control(surface = "direct")
  )
  
  ## Predict
  as.numeric(predict(fit, newdata = data.frame(x = xloess)))
}

#' Write performance results and LOESS-smoothed curves to CSV files
#'
#' Writes raw performance results and LOESS-smoothed performance curves to
#' comma-separated value (CSV) files, for each missingness scenario, analysis
#' group, and performance metric.
#'
#' @param tables List. Nested list of performance results as constructed during
#'   the simulation loop. For each group and metric, results are indexed by the
#'   target missingness proportion and contain both observed missingness and
#'   performance values.
#' @param scenario Character scalar. Identifier of the missingness scenario.
#' @param out_dir Character scalar. Path to the directory where CSV files are
#'   written.
#' @param loess_span Numeric. Span parameter passed to the LOESS smoother.
#'   Defaults to 0.75.
#' @param loess_trim Numeric vector of length 2. Lower and upper quantiles used
#'   to trim extreme values of the observed missingness proportion prior to
#'   LOESS fitting. Defaults to \code{c(0.02, 0.98)}.
#' @param n_loess Integer. Number of points used to evaluate the LOESS-smoothed
#'   curves. Defaults to 200.
#'
#' @return Invisibly returns \code{TRUE}. The primary effect of the function is
#'   writing CSV files to disk.
#'
#' @details
#' For each combination of analysis group and performance metric, two CSV files
#' are produced:
#' \itemize{
#'   \item A raw performance file containing one row per simulation setting,
#'     with columns for observed missingness, target missingness, and the
#'     performance of each method.
#'   \item A LOESS-smoothed file containing smoothed performance curves for each
#'     method, evaluated on a common grid of observed missingness proportions.
#' }
#'
#' LOESS smoothing is intended solely for visualization purposes and is applied
#' independently to each method. No aggregation or smoothing is performed prior
#' to this step.
#'
#' @seealso \code{\link{loess_smooth_series}}
write_results_tables = function(tables,
                                scenario,
                                out_dir,
                                loess_span = 0.75,
                                loess_trim = c(0.02, 0.98),
                                n_loess = 200) {
  
  check_character_value(scenario, "scenario")
  check_data_frame_path(out_dir)
  
  for (group in names(tables)) {
    for (metric in names(tables[[group]])) {
      
      tbl = tables[[group]][[metric]]
      
      ## ----------------------------------------------------------------------
      ## Raw performance table
      ## ----------------------------------------------------------------------
      df_perf = do.call(base::rbind, lapply(tbl, `[[`, "performance"))
      df_perf = as.data.frame(df_perf)
      
      df_perf$observed_missingness =
        sapply(tbl, `[[`, "observed_missingness")
      
      df_perf$target_missingness =
        as.numeric(names(tbl))
      
      df_perf = df_perf[order(df_perf$observed_missingness), ]
      
      raw_file = sprintf(
        "%s_%s_%s_points.csv",
        scenario,
        group,
        metric
      )
      
      write.csv(
        df_perf,
        file = file.path(out_dir, raw_file),
        row.names = FALSE
      )
      
      ## ----------------------------------------------------------------------
      ## LOESS-smoothed table
      ## ----------------------------------------------------------------------
      x = df_perf$observed_missingness
      
      ## Common prediction grid
      xloess = seq(
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        length.out = n_loess
      )
      
      df_loess = data.frame(observed_missingness = xloess)
      
      method_names = setdiff(
        names(df_perf),
        c("observed_missingness", "target_missingness")
      )
      
      for (m in method_names) {
        
        df_loess[[m]] =
          loess_smooth_series(
            x = df_perf$observed_missingness,
            y = df_perf[[m]],
            xloess = xloess,
            span = loess_span,
            trim = loess_trim
          )
      }
      
      loess_file = sprintf(
        "%s_%s_%s_loess.csv",
        scenario,
        group,
        metric
      )
      
      write.csv(
        df_loess,
        file = file.path(out_dir, loess_file),
        row.names = FALSE
      )
    }
  }
  
  invisible(TRUE)
}