################################################################################
## Validation analysis: fresh, minimal fitting/prediction/pooling machinery
##
## This file deliberately does NOT reuse train_mi()/train_mimi() from
## training_procedures.R: those carry unneeded baggage for this analysis and
## train_mimi() in particular has a known bug (its imputation model for X1 is
## fit on already-mice-imputed rows instead of the observed ones). Instead,
## fit_linear_mi() below is a fresh, minimal MI-then-pool implementation used
## for both the MU (Y ~ X1 + X2) and MC (Y ~ X1 + X2 + MX1) substantive
## models.
##
## Together with the samplers in validation_functions.R
## (sample_x1_mu/sample_x1_mc, and their "_y" outcome-conditional
## counterparts) and the theoretical risks in the same file
## (compute_theoretical_risks), the functions here implement the empirical
## side of the validation-consistency analysis. For a given simulated
## dataset, validate_one_point() evaluates, for both the MU and the MC
## target:
##
##   - risk-pooling (RP) / predictions-pooling (PP), crossed with two
##     estimation-quality conditions:
##       * "estimated": the fitted (trained-by-MI) prediction function,
##         validated using a validation-time imputation model itself fitted
##         with mice on the test set -- the fully realistic, deployment-
##         representative combination.
##       * "optimal": the Bayes-optimal (oracle) prediction function,
##         validated using validation-time imputation drawn from the true
##         conditional distribution -- the fully idealised combination, used
##         to check whether the validation-consistency propositions hold
##         exactly once neither piece is estimated (so any residual gap from
##         the theoretical target risk is pure Monte Carlo / finite-sample
##         noise, not bias introduced by a mis-estimated prediction function
##         or imputation model).
##     (An earlier version of this analysis paired the fitted prediction
##     function with both imputation qualities; pairing "optimal" with the
##     oracle prediction function instead gives each of the two conditions a
##     clean interpretation -- "fully realistic" vs "fully idealised" --
##     matching the fitted/oracle split already used below for CCVal.)
##   - complete-case validation (CCVal), both for the fitted prediction
##     function (ccval_fitted) and for the oracle one (ccval_optimal) -- the
##     same fitted/oracle split as above, so any gap between the two isolates
##     whether a departure from the theoretical CP risk originates from the
##     training procedure or from the validation procedure itself.
##   - optionally (when include_outcome_variant = TRUE), the same RP/PP
##     quantities recomputed with the validation-time imputation model (both
##     optimal and estimated) also given the true outcome Y -- illustrative
##     only, never deployment-realistic. This variant keeps the *fitted*
##     prediction function throughout for both its optimal and estimated
##     imputation conditions: it tests a different proposition (whether
##     including the outcome in the validation-time imputation model
##     recovers the full risk, for ANY fixed CP predictor under test -- see
##     Section on "Inclusion of the outcome"), not one specific to the oracle
##     predictor, so there is no oracle-prediction endpoint to add here.
################################################################################

#' Fit a linear outcome model via multiple imputation and Rubin's pooling
#'
#' Multiply imputes \code{data} (using \pkg{mice}'s \code{"norm"} method,
#' with the outcome included as a predictor of missing X1, since this is a
#' training-time imputation model where using the outcome is standard
#' practice to avoid attenuation of the X1-Y association), fits
#' \code{formula} by OLS on each of the \code{m} completed datasets, and
#' pools the resulting coefficients with Rubin's rules.
#'
#' @param data Data frame to be imputed and fit on (typically a training
#'   set with missing X1).
#' @param formula Formula for the substantive (outcome) model, e.g.
#'   \code{Y ~ X1 + X2} or \code{Y ~ X1 + X2 + MX1}.
#' @param variables Character vector of column names of \code{data} to pass
#'   to \code{mice()} (only these are imputed / used as predictors).
#' @param exclude_from_predictor_matrix Character vector of variable names
#'   to exclude from being used as *predictors* in the imputation model
#'   (their column in \pkg{mice}'s predictor matrix is zeroed out). Used
#'   for \code{"MX1"} in the MC family: within the observed-X1 subset that
#'   \pkg{mice} uses to fit the norm imputation model, MX1 is constant
#'   (= 0), so it carries no information and cannot be estimated as a
#'   predictor there.
#' @param m Integer. Number of multiple imputations. Defaults to 5.
#'
#' @return A named numeric vector of pooled coefficient estimates (names
#'   are term names, including \code{"(Intercept)"}).
fit_linear_mi = function(data, formula, variables,
                         exclude_from_predictor_matrix = character(0),
                         m = 5) {
  pm = make.predictorMatrix(data[, variables, drop = FALSE])
  if (length(exclude_from_predictor_matrix) > 0) {
    pm[, exclude_from_predictor_matrix] = 0
    pm[exclude_from_predictor_matrix, ] = 0
  }

  imp = mice(data[, variables, drop = FALSE], m = m, method = "norm",
            predictorMatrix = pm, printFlag = FALSE)

  fits = lapply(seq_len(m), function(i) lm(formula, data = complete(imp, i)))
  pooled = pool(as.mira(fits))

  coef = setNames(pooled$pooled$estimate, as.character(pooled$pooled$term))

  ## Edge case: a term can be perfectly collinear with the rest of the model
  ## (e.g. MX1 in the MC formula when the missingness proportion is exactly
  ## 0, so MX1 is constant across the whole training set) and come back as
  ## NA from lm()/pool(). Drop such terms rather than let an NA coefficient
  ## propagate into every prediction; dropping MX1 here is also the
  ## substantively correct fallback, since a non-varying M carries no
  ## information and the MC model then coincides with the MU model.
  if (anyNA(coef)) {
    dropped = names(coef)[is.na(coef)]
    warning(sprintf("fit_linear_mi(): dropping non-estimable (NA) term(s): %s",
                    paste(dropped, collapse = ", ")), call. = FALSE)
    coef = coef[!is.na(coef)]
  }

  coef
}

#' Predict from a named coefficient vector on new data
#'
#' Generic linear-model prediction that works for any set of terms present
#' in \code{coef} (e.g. 3 coefficients for MU, 4 for MC including MX1),
#' by building the model matrix implied by the non-intercept term names.
#'
#' @param coef Named numeric vector of coefficients, as returned by
#'   \code{fit_linear_mi()} (or an oracle coefficient vector built from the
#'   true \code{theta$Y$beta} for a Bayes-optimal predictor -- see
#'   \code{oracle_coef_for_family()}).
#' @param newdata Data frame containing the columns named in \code{coef}
#'   (other than \code{"(Intercept)"}).
#'
#' @return Numeric vector of predictions, length \code{nrow(newdata)}.
predict_linear = function(coef, newdata) {
  terms = setdiff(names(coef), "(Intercept)")
  formula = reformulate(termlabels = terms, response = NULL)
  X = model.matrix(formula, data = newdata)
  as.vector(X %*% coef[colnames(X)])
}

#' Bayes-optimal (oracle) coefficient vector for the MU or MC target
#'
#' Under the data-generating model in \code{theta}, the missingness
#' indicator MX1 has no direct effect on Y given (X1, X2) -- it is a
#' downstream consequence of (X1, X2, Y), not a cause of Y -- so the oracle
#' CP predictor for the MC family, \eqn{E[Y \mid X_1, X_2, M_{X1} = 0]},
#' coincides with the MU oracle predictor, \eqn{E[Y \mid X_1, X_2]}. This
#' returns that shared oracle relationship expressed as a coefficient
#' vector, with an explicit (zero) \code{"MX1"} term added for the MC
#' family so that \code{predict_linear()} builds the same
#' \code{~ X1 + X2 + MX1} design used by the fitted MC coefficient.
#'
#' @param theta List. Data-generating model parameters (as in config.R).
#' @param family One of \code{"mu"} or \code{"mc"}.
#'
#' @return A named numeric vector of coefficients, in the same form as
#'   \code{fit_linear_mi()}'s return value.
oracle_coef_for_family = function(theta, family = c("mu", "mc")) {
  family = match.arg(family)
  coef = theta[["Y"]][["beta"]]
  if (family == "mc") coef = c(coef, "MX1" = 0)
  coef
}

#' Build multiply-imputed copies of a test set for validation
#'
#' Constructs \code{m} completed versions of \code{data_test} with its
#' missing X1 values filled in, using either an "estimated" (mice-fit,
#' practice-realistic) or "optimal" (true-distribution, oracle) imputation
#' model. By default the outcome Y is excluded from the imputation model,
#' as it must be at deployment; setting \code{include_outcome = TRUE}
#' builds the illustrative-only "_withY" variant instead, where the
#' imputation model (of either quality) is also given the true Y.
#'
#' @param data_test Data frame with columns X1 (possibly missing), X2, Y,
#'   MX1.
#' @param m Integer. Number of imputations.
#' @param method One of \code{"estimated"} or \code{"optimal"}.
#' @param family One of \code{"mu"} or \code{"mc"}; controls which true
#'   distribution the "optimal" sampler targets, and whether MX1 is
#'   excluded from the "estimated" imputation model's predictors.
#' @param include_outcome Logical. If \code{TRUE}, the outcome Y is made
#'   available to the imputation model (both "estimated" and "optimal").
#'   Defaults to \code{FALSE} (the deployment-realistic default).
#' @param theta,beta_phi Required when \code{method = "optimal"}: the
#'   data-generating and missingness model parameters used by the
#'   \code{sample_x1_*()} samplers in \code{validation_functions.R}.
#' @param K,B_inner SIR parameters forwarded to \code{sample_x1_mc()} /
#'   \code{sample_x1_mc_y()} when \code{method = "optimal"} and
#'   \code{family = "mc"} (\code{B_inner} is unused, and not forwarded, when
#'   \code{include_outcome = TRUE}, since \code{sample_x1_mc_y()} does not
#'   need to integrate Y out -- it is already observed).
#' @param verbose Logical. Print progress via \code{log_step()}.
#'
#' @return A list of \code{m} completed data frames (copies of
#'   \code{data_test} with X1 filled in).
build_imputed_test_sets = function(data_test, m,
                                   method = c("estimated", "optimal"),
                                   family = c("mu", "mc"),
                                   include_outcome = FALSE,
                                   theta = NULL, beta_phi = NULL,
                                   K = 1000, B_inner = 200,
                                   verbose = FALSE) {
  method = match.arg(method)
  family = match.arg(family)

  n = nrow(data_test)
  idx_miss = which(data_test[["MX1"]] == 1)

  if (method == "estimated") {
    base_vars = if (family == "mc") c("X1", "X2", "MX1") else c("X1", "X2")
    variables = if (include_outcome) c(base_vars, "Y") else base_vars

    pm = make.predictorMatrix(data_test[, variables, drop = FALSE])
    if (family == "mc") {
      pm[, "MX1"] = 0
      pm["MX1", ] = 0
    }
    imp = mice(data_test[, variables, drop = FALSE], m = m, method = "norm",
              predictorMatrix = pm, printFlag = FALSE)

    imputed_list = lapply(seq_len(m), function(i) {
      dat = data_test
      dat[, variables] = complete(imp, i)
      dat
    })

    if (verbose) {
      log_step(sprintf("build_imputed_test_sets: estimated imputation (%s%s), m = %d done",
                       family, if (include_outcome) ", +Y" else "", m), indent = 2)
    }

  } else { # optimal
    if (length(idx_miss) == 0) {
      imputed_list = replicate(m, data_test, simplify = FALSE)
      return(imputed_list)
    }

    if (!include_outcome) {
      if (family == "mu") {
        draws = sample_x1_mu(n = length(idx_miss), m = m, theta = theta)
        ess_summary = NULL
      } else {
        sir = sample_x1_mc(X2 = data_test[["X2"]][idx_miss], m = m,
                           theta = theta, beta_phi = beta_phi,
                           K = K, B = B_inner, verbose = verbose)
        draws = sir$draws
        ess_summary = sir$ess_frac
      }
    } else {
      X2_miss = data_test[["X2"]][idx_miss]
      Y_miss  = data_test[["Y"]][idx_miss]
      if (family == "mu") {
        draws = sample_x1_mu_y(X2 = X2_miss, Y = Y_miss, m = m, theta = theta)
        ess_summary = NULL
      } else {
        sir = sample_x1_mc_y(X2 = X2_miss, Y = Y_miss, m = m,
                             theta = theta, beta_phi = beta_phi,
                             K = K, verbose = verbose)
        draws = sir$draws
        ess_summary = sir$ess_frac
      }
    }

    imputed_list = lapply(seq_len(m), function(i) {
      dat = data_test
      dat[["X1"]][idx_miss] = draws[, i]
      dat
    })

    if (verbose) {
      msg = sprintf("build_imputed_test_sets: optimal imputation (%s%s), m = %d done",
                    family, if (include_outcome) ", +Y" else "", m)
      if (!is.null(ess_summary)) {
        msg = paste0(msg, sprintf(" | mean ESS frac %.3f | min ESS frac %.3f",
                                  mean(ess_summary), min(ess_summary)))
      }
      log_step(msg, indent = 2)
    }
  }

  imputed_list
}

#' Risk-pooling and predictions-pooling MSE across multiply-imputed test sets
#'
#' @param coef Named coefficient vector, as returned by
#'   \code{fit_linear_mi()} or \code{oracle_coef_for_family()}.
#' @param imputed_list List of \code{m} completed test-set data frames, as
#'   returned by \code{build_imputed_test_sets()}.
#' @param y_true Numeric vector of true outcome values, aligned with the
#'   rows of every data frame in \code{imputed_list}.
#'
#' @return A list with elements \code{rp} (risk-pooling MSE: mean of the
#'   per-imputation MSEs) and \code{pp} (predictions-pooling MSE: MSE of
#'   the across-imputation-averaged predictions).
compute_rp_pp = function(coef, imputed_list, y_true) {
  preds = sapply(imputed_list, function(dat) predict_linear(coef, dat))
  mse_vec = apply(preds, 2, function(p) compute_msd(y_true, p))
  rp = mean(mse_vec)
  pp = compute_msd(y_true, rowMeans(preds))
  list(rp = rp, pp = pp)
}

#' Complete-case validation MSE
#'
#' @param coef Named coefficient vector, as returned by
#'   \code{fit_linear_mi()} or \code{oracle_coef_for_family()}.
#' @param data_test Data frame with columns X1, X2, Y, MX1.
#'
#' @return Numeric scalar, the MSE of \code{predict_linear(coef, .)}
#'   evaluated on the \code{MX1 == 0} subset of \code{data_test}.
compute_ccval = function(coef, data_test) {
  idx0 = data_test[["MX1"]] == 0
  data_cc = data_test[idx0, , drop = FALSE]
  pred = predict_linear(coef, data_cc)
  compute_msd(data_cc[["Y"]], pred)
}

#' Run the full empirical validation analysis for one simulated dataset
#'
#' Trains an MU (\code{Y ~ X1 + X2}) or MC (\code{Y ~ X1 + X2 + MX1})
#' forecaster on the training set via \code{fit_linear_mi()}, then validates
#' it -- alongside the Bayes-optimal (oracle) forecaster of the same
#' family -- under: risk-pooling and predictions-pooling (each computed once
#' for the fitted forecaster with estimated (mice-fit) validation-time
#' imputation, and once for the oracle forecaster with optimal
#' (true-distribution) validation-time imputation), plus complete-case
#' validation (again once fitted, once oracle). See the file-level comment
#' above for the rationale behind this fitted/estimated vs. oracle/optimal
#' pairing.
#'
#' @param simulation_object List. One pre-generated simulation object (as
#'   read from \code{output/main/raw/.../simulation_*.rds}), containing
#'   \code{data$train}, \code{data$test}, \code{beta_phi}, and
#'   \code{metadata}.
#' @param family One of \code{"mu"} or \code{"mc"}.
#' @param theta List. Data-generating model parameters (as in config.R).
#' @param m Integer. Number of imputations (both for training and for the
#'   validation-time imputed test-set constructions). Defaults to 5.
#' @param K,B_inner SIR parameters forwarded to the "optimal" MC sampler.
#' @param include_outcome_variant Logical. If \code{TRUE}, also compute the
#'   four "_withY" risk-/predictions-pooling columns (illustrative-only
#'   validation-time imputation that is also given the true outcome Y, kept
#'   on the fitted prediction function throughout -- see the file-level
#'   comment above). Defaults to \code{FALSE}.
#' @param verbose Logical. Print progress via \code{log_step()}.
#'
#' @return A one-row data frame with columns \code{scenario},
#'   \code{missingness_target}, \code{missingness_proportion}, \code{family},
#'   \code{riskPooling_optimal}, \code{predictionsPooling_optimal},
#'   \code{riskPooling_estimated}, \code{predictionsPooling_estimated},
#'   \code{ccval_fitted}, \code{ccval_optimal}, and -- when
#'   \code{include_outcome_variant = TRUE} -- \code{riskPooling_optimal_withY},
#'   \code{predictionsPooling_optimal_withY}, \code{riskPooling_estimated_withY},
#'   \code{predictionsPooling_estimated_withY}.
validate_one_point = function(simulation_object, family = c("mu", "mc"),
                              theta, m = 5, K = 1000, B_inner = 200,
                              include_outcome_variant = FALSE,
                              verbose = FALSE) {
  family = match.arg(family)

  data_train = simulation_object[["data"]][["train"]]
  data_test  = simulation_object[["data"]][["test"]]
  beta_phi   = simulation_object[["beta_phi"]]
  scenario   = simulation_object[["metadata"]][["scenario"]]
  target     = simulation_object[["metadata"]][["missingness_target_X1"]]
  y_true     = data_test[["Y"]]

  if (family == "mu") {
    formula = Y ~ X1 + X2
    variables_train = c("X1", "X2", "Y")
    exclude_train = character(0)
  } else {
    formula = Y ~ X1 + X2 + MX1
    variables_train = c("X1", "X2", "Y", "MX1")
    exclude_train = "MX1"
  }

  oracle_coef = oracle_coef_for_family(theta, family)

  if (verbose) {
    log_step(sprintf("validate_one_point: %s | %s | target %.3f | fitting via MI (m = %d)",
                     scenario, family, target, m), indent = 1)
  }

  coef_fitted = fit_linear_mi(data_train, formula = formula, variables = variables_train,
                              exclude_from_predictor_matrix = exclude_train, m = m)

  ## --- ordinary (Y-free) validation-time imputation ---
  imputed_estimated = build_imputed_test_sets(
    data_test, m = m, method = "estimated", family = family, verbose = verbose
  )
  imputed_optimal = build_imputed_test_sets(
    data_test, m = m, method = "optimal", family = family,
    theta = theta, beta_phi = beta_phi, K = K, B_inner = B_inner, verbose = verbose
  )

  ## Fitted prediction / estimated imputation: fully realistic.
  rp_pp_estimated = compute_rp_pp(coef_fitted, imputed_estimated, y_true)
  ## Oracle prediction / optimal imputation: fully idealised.
  rp_pp_optimal   = compute_rp_pp(oracle_coef, imputed_optimal, y_true)

  ccval_fitted  = compute_ccval(coef_fitted, data_test)
  ccval_optimal = compute_ccval(oracle_coef, data_test)

  if (verbose) {
    log_step(sprintf(
      "validate_one_point: %s | %s | target %.3f | RPopt %.4f PPopt %.4f RPest %.4f PPest %.4f CCVfit %.4f CCVopt %.4f",
      scenario, family, target,
      rp_pp_optimal$rp, rp_pp_optimal$pp,
      rp_pp_estimated$rp, rp_pp_estimated$pp, ccval_fitted, ccval_optimal
    ), indent = 1)
  }

  result = data.frame(
    scenario = scenario,
    missingness_target = target,
    missingness_proportion = mean(data_train[["MX1"]]),
    family = family,
    riskPooling_optimal = rp_pp_optimal$rp,
    predictionsPooling_optimal = rp_pp_optimal$pp,
    riskPooling_estimated = rp_pp_estimated$rp,
    predictionsPooling_estimated = rp_pp_estimated$pp,
    ccval_fitted = ccval_fitted,
    ccval_optimal = ccval_optimal,
    stringsAsFactors = FALSE
  )

  if (include_outcome_variant) {
    imputed_estimated_withY = build_imputed_test_sets(
      data_test, m = m, method = "estimated", family = family,
      include_outcome = TRUE, verbose = verbose
    )
    imputed_optimal_withY = build_imputed_test_sets(
      data_test, m = m, method = "optimal", family = family,
      include_outcome = TRUE,
      theta = theta, beta_phi = beta_phi, K = K, B_inner = B_inner, verbose = verbose
    )

    ## Fitted prediction function throughout -- see file-level comment.
    rp_pp_estimated_withY = compute_rp_pp(coef_fitted, imputed_estimated_withY, y_true)
    rp_pp_optimal_withY   = compute_rp_pp(coef_fitted, imputed_optimal_withY, y_true)

    result$riskPooling_optimal_withY          = rp_pp_optimal_withY$rp
    result$predictionsPooling_optimal_withY   = rp_pp_optimal_withY$pp
    result$riskPooling_estimated_withY        = rp_pp_estimated_withY$rp
    result$predictionsPooling_estimated_withY = rp_pp_estimated_withY$pp

    if (verbose) {
      log_step(sprintf(
        "validate_one_point: %s | %s | target %.3f | +Y: RPopt %.4f PPopt %.4f RPest %.4f PPest %.4f",
        scenario, family, target,
        rp_pp_optimal_withY$rp, rp_pp_optimal_withY$pp,
        rp_pp_estimated_withY$rp, rp_pp_estimated_withY$pp
      ), indent = 1)
    }
  }

  result
}
