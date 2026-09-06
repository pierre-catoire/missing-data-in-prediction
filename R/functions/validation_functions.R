################################################################################
## (Removed: this file used to open with impute_at_validation(),
## get_function_performance(), get_reference_performance() and
## compute_true_risks() -- an earlier, pre-validation_analysis.R generation
## of this file's machinery. None of the four were called anywhere in the
## current pipeline (validate_one_point() in validation_analysis.R replaced
## them); deleted as dead code rather than left to rot. See git history if
## you need to recover them.)
################################################################################

## =============================================================================
## Theoretical (population-level) MU / MC risks: full, cp (complete-case) and
## op (observed-pattern / deployment) variants
## =============================================================================
#
# Notation (X = (X1,X2), Xo = observed part of X, M = missingness indicator
# for X1, S = a scoring rule, squared error by default):
#
#   risk_mu_op   = E[S(Y, P(Y|Xo))]              -- MU, deployed as-is
#   risk_mu_cp   = E[S(Y, P(Y|X))  | M = 0]      -- MU, full-X predictor, complete cases only
#   risk_mu_full = E[S(Y, P(Y|X))]                -- MU, full-X predictor, everyone
#   risk_mc_op   = E[S(Y, P(Y|Xo,M))]            -- MC, deployed as-is (uses each unit's own M)
#   risk_mc_cp   = E[S(Y, P(Y|X,M=0)) | M = 0]   -- MC, full-X predictor with M forced to 0, complete cases only
#   risk_mc_full = E[S(Y, P(Y|X,M=0))]            -- MC, full-X predictor with M forced to 0, everyone
#
# risk_mu_op/cp/full are exactly compute_true_risks()'s risk_obs / risk_full /
# risk_full_unconditional, renamed to line up with the mu/mc, op/cp/full
# naming used for the validation analysis. The MU quantities are analytic
# (no Monte Carlo needed, since E[Y|X1,X2] and E[Y|X2] are closed-form under
# the Gaussian data-generating model with X1 ⟂ X2). The MC quantities need
# E[Y|X1,X2,M] and E[Y|X2,M], which are not closed-form (M enters through a
# logistic model on (X1,X2,Y)) and are obtained by importance-weighted Monte
# Carlo integration, vectorised and chunked over the simulated population so
# that N can be large without an N-times-slower per-unit loop.

#' Monte Carlo estimate of E[Y | X1, X2, M = m], vectorised over units
#'
#' For each unit i with covariates (X1[i], X2[i]), approximates
#' \eqn{E[Y \mid X_1 = X1_i, X_2 = X2_i, M = m]} by importance-weighted Monte
#' Carlo integration over \eqn{Y \mid X_1, X_2 \sim N(\mu_{Y|X}, \sigma_Y)},
#' with importance weights \eqn{P(M = m \mid X_1, X_2, Y)} from the logistic
#' missingness model. The same B standard-normal draws are reused (as a
#' shared, "common random numbers" grid) for every unit, and units are
#' processed in chunks to bound memory use.
#'
#' @param X1,X2 Numeric vectors of equal length. Covariate values.
#' @param m Integer, 0 or 1. The value of M to condition on.
#' @param theta List. Data-generating model parameters (as in config.R).
#' @param beta_phi Numeric vector. Coefficients of the logistic missingness
#'   model for M, including the intercept.
#' @param B Integer. Number of Monte Carlo draws per unit. Defaults to 2000.
#' @param chunk_size Integer. Number of units processed per chunk, to keep
#'   the N x B intermediate matrices a manageable size. Defaults to 5000.
#'
#' @return Numeric vector, same length as X1, of E[Y | X1, X2, M = m].
ey_given_x1x2_m = function(X1, X2, m, theta, beta_phi,
                           B = 2000, chunk_size = 5000) {
  check_numeric_vector(X1, "X1")
  check_numeric_vector(X2, "X2")
  check_same_length(X1, X2, "X1", "X2")

  n = length(X1)
  z = rnorm(B) # shared draws, reused across all units/chunks

  beta0 = theta[["Y"]][["beta"]][["(Intercept)"]]
  beta1 = theta[["Y"]][["beta"]][["X1"]]
  beta2 = theta[["Y"]][["beta"]][["X2"]]
  sdY   = theta[["Y"]][["sigma"]]

  out = numeric(n)
  for (start in seq(1, n, by = chunk_size)) {
    idx = start:min(start + chunk_size - 1, n)

    muY_X   = beta0 + beta1 * X1[idx] + beta2 * X2[idx]     # length(idx)
    Y_draws = outer(muY_X, sdY * z, "+")                     # length(idx) x B

    lp = beta_phi[["(Intercept)"]] +
      beta_phi[["X1"]] * X1[idx] +
      beta_phi[["X2"]] * X2[idx] +
      beta_phi[["Y"]]  * Y_draws                              # length(idx) x B (X1,X2 recycled by column)

    p = plogis(lp)
    w = if (m == 1) p else (1 - p)

    out[idx] = rowSums(Y_draws * w) / rowSums(w)
  }
  out
}

#' Monte Carlo estimate of E[Y | X2, M = m], marginalising over X1
#'
#' For each unit i with covariate X2[i], approximates
#' \eqn{E[Y \mid X_2 = X2_i, M = m]} by importance-weighted Monte Carlo
#' integration jointly over \eqn{X_1 \sim N(\mu_{X_1}, \sigma_{X_1})} and
#' \eqn{Y \mid X_1, X_2 \sim N(\mu_{Y|X}, \sigma_Y)}, using the same shared
#' (X1, Y-noise) draws for every unit and chunking over units.
#'
#' @inheritParams ey_given_x1x2_m
#' @param X2 Numeric vector. Covariate values.
#'
#' @return Numeric vector, same length as X2, of E[Y | X2, M = m].
ey_given_x2_m = function(X2, m, theta, beta_phi,
                         B = 2000, chunk_size = 5000) {
  check_numeric_vector(X2, "X2")

  n = length(X2)
  X1_draws = rnorm(B, theta[["X1"]][["beta"]][["(Intercept)"]], theta[["X1"]][["sigma"]])
  z        = rnorm(B)

  beta0 = theta[["Y"]][["beta"]][["(Intercept)"]]
  beta1 = theta[["Y"]][["beta"]][["X1"]]
  beta2 = theta[["Y"]][["beta"]][["X2"]]
  sdY   = theta[["Y"]][["sigma"]]

  muY_X1_part = beta0 + beta1 * X1_draws                     # length B, shared across units

  out = numeric(n)
  for (start in seq(1, n, by = chunk_size)) {
    idx = start:min(start + chunk_size - 1, n)
    x2_chunk = X2[idx]                                        # length(idx)

    muY_X  = outer(beta2 * x2_chunk, muY_X1_part, "+")        # length(idx) x B
    Y_draws = muY_X + matrix(sdY * z, nrow = length(idx), ncol = B, byrow = TRUE)

    lp = beta_phi[["(Intercept)"]] +
      matrix(beta_phi[["X1"]] * X1_draws, nrow = length(idx), ncol = B, byrow = TRUE) +
      beta_phi[["X2"]] * x2_chunk +
      beta_phi[["Y"]]  * Y_draws

    p = plogis(lp)
    w = if (m == 1) p else (1 - p)

    out[idx] = rowSums(Y_draws * w) / rowSums(w)
  }
  out
}

#' Compute the six theoretical MU / MC risks (full, cp, op)
#'
#' Simulates a population of size \code{N} from the true data-generating and
#' missingness models (\code{theta}, \code{beta_phi}) and computes the six
#' theoretical risks defined at the top of this section: \code{risk_mu_op},
#' \code{risk_mu_cp}, \code{risk_mu_full}, \code{risk_mc_op},
#' \code{risk_mc_cp}, \code{risk_mc_full}.
#'
#' The MU quantities are computed analytically (closed-form conditional
#' means under the Gaussian model). The MC quantities require the
#' conditional means E[Y|X1,X2,M] and E[Y|X2,M], obtained via
#' \code{ey_given_x1x2_m()} and \code{ey_given_x2_m()}.
#'
#' @param theta List. Data-generating model parameters (as in config.R).
#' @param beta_phi Numeric vector. Coefficients of the logistic missingness
#'   model for M, including the intercept.
#' @param N Integer. Size of the simulated population used to approximate
#'   the outer expectations. Defaults to 1e5.
#' @param B Integer. Number of Monte Carlo draws used for each inner
#'   E[Y|X1,X2,M] / E[Y|X2,M] integral. Defaults to 2000.
#' @param score Function of \code{(y, pred)}. Scoring rule S used in every
#'   risk. Defaults to squared error, \code{(y - pred)^2}, matching
#'   \code{compute_msd()} used throughout the rest of the pipeline.
#' @param chunk_size Integer. Units processed per chunk in the Monte Carlo
#'   integrals, to bound memory use for large N. Defaults to 5000.
#'
#' @return A named list with elements \code{risk_mu_op}, \code{risk_mu_cp},
#'   \code{risk_mu_full}, \code{risk_mc_op}, \code{risk_mc_cp},
#'   \code{risk_mc_full}.
#'
#' @seealso \code{\link{compute_true_risks}}, \code{\link{ey_given_x1x2_m}},
#'   \code{\link{ey_given_x2_m}}
compute_theoretical_risks = function(theta, beta_phi, N = 1e5, B = 2000,
                                     score = function(y, pred) (y - pred)^2,
                                     chunk_size = 5000) {

  beta0 = theta[["Y"]][["beta"]][["(Intercept)"]]
  beta1 = theta[["Y"]][["beta"]][["X1"]]
  beta2 = theta[["Y"]][["beta"]][["X2"]]

  ## --- 1. Simulate a population from the true DGP + missingness model ---
  X1 = rnorm(N, theta[["X1"]][["beta"]][["(Intercept)"]], theta[["X1"]][["sigma"]])
  X2 = rnorm(N, theta[["X2"]][["beta"]][["(Intercept)"]], theta[["X2"]][["sigma"]])

  muY_X = beta0 + beta1 * X1 + beta2 * X2
  Y = rnorm(N, muY_X, theta[["Y"]][["sigma"]])

  lp = beta_phi[["(Intercept)"]] + beta_phi[["X1"]] * X1 +
    beta_phi[["X2"]] * X2 + beta_phi[["Y"]] * Y
  M = rbinom(N, 1, plogis(lp))
  idx0 = M == 0

  ## --- 2. MU risks (analytic; X1 independent of X2 under theta) ---
  muY_X2 = beta0 + beta1 * theta[["X1"]][["beta"]][["(Intercept)"]] + beta2 * X2
  pred_mu_op = ifelse(idx0, muY_X, muY_X2)

  risk_mu_full = mean(score(Y, muY_X))
  risk_mu_cp   = mean(score(Y[idx0], muY_X[idx0]))
  risk_mu_op   = mean(score(Y, pred_mu_op))

  ## --- 3. MC risks (need the M-informed conditional means) ---
  # E[Y | X1, X2, M = 0] for every unit: used by cp, full, and op's M=0 branch
  EY_X1X2_M0 = ey_given_x1x2_m(X1, X2, m = 0, theta = theta, beta_phi = beta_phi,
                               B = B, chunk_size = chunk_size)

  # E[Y | X2, M = 1]: only needed for op's M=1 branch
  idx1 = which(!idx0)
  EY_X2_M1 = numeric(N)
  if (length(idx1) > 0) {
    EY_X2_M1[idx1] = ey_given_x2_m(X2[idx1], m = 1, theta = theta, beta_phi = beta_phi,
                                   B = B, chunk_size = chunk_size)
  }

  pred_mc_op = ifelse(idx0, EY_X1X2_M0, EY_X2_M1)

  risk_mc_full = mean(score(Y, EY_X1X2_M0))
  risk_mc_cp   = mean(score(Y[idx0], EY_X1X2_M0[idx0]))
  risk_mc_op   = mean(score(Y, pred_mc_op))

  list(
    risk_mu_op   = risk_mu_op,
    risk_mu_cp   = risk_mu_cp,
    risk_mu_full = risk_mu_full,
    risk_mc_op   = risk_mc_op,
    risk_mc_cp   = risk_mc_cp,
    risk_mc_full = risk_mc_full
  )
}

## =============================================================================
## "Optimal" (true-distribution) imputation samplers
## =============================================================================
#
# These draw multiple imputed values of X1 from the *genuine* true
# conditional distribution implied by theta/phi -- not a distribution
# estimated from data -- to serve as the "optimal imputation" benchmark in
# the validation analysis:
#
#   sample_x1_mu(): draws from the true marginal P(X1 | X2). Under theta,
#     X1 is independent of X2 (X1 ~ N(mu1, sd1) with no dependence on X2),
#     so this is just independent draws from that marginal -- no MC needed.
#
#   sample_x1_mc(): draws from the true P(X1 | X2, M = 0), which has no
#     closed form (M enters through a logistic model on (X1,X2,Y), and Y
#     must be integrated out). Obtained via Sampling Importance Resampling
#     (SIR): propose X1 candidates from the prior P(X1), weight each
#     candidate by the (Y-marginalised) likelihood P(M = 0 | X1, X2), then
#     resample. Because the proposal is the true marginal itself, the
#     importance weight is exactly the likelihood, with no prior/proposal
#     ratio to track.

#' Draw multiply-imputed values of X1 from the true marginal P(X1 | X2)
#'
#' For the MU family, the true distribution of interest is P(X1 | X2), and
#' under the data-generating model in \code{theta}, X1 is generated
#' independently of X2. The "optimal" imputation draws are therefore simply
#' independent samples from the marginal N(mu1, sd1).
#'
#' @param n Integer. Number of units (rows) to draw for.
#' @param m Integer. Number of imputed values to draw per unit.
#' @param theta List. Data-generating model parameters (as in config.R).
#'
#' @return An n x m numeric matrix of draws.
sample_x1_mu = function(n, m, theta) {
  mu1 = theta[["X1"]][["beta"]][["(Intercept)"]]
  sd1 = theta[["X1"]][["sigma"]]
  matrix(rnorm(n * m, mean = mu1, sd = sd1), nrow = n, ncol = m)
}

#' Draw multiply-imputed values of X1 from the true P(X1 | X2, M = 0) via SIR
#'
#' Approximates the true (non-closed-form) posterior \eqn{P(X_1 \mid X_2,
#' M = 0)} by Sampling Importance Resampling, and draws \code{m} imputed
#' values per unit from that approximation.
#'
#' Algorithm, vectorised over units and chunked to bound memory use:
#' \enumerate{
#'   \item Draw \code{K} candidate X1 values from the true prior
#'     N(mu1, sd1) (the proposal distribution), shared across every unit.
#'   \item Draw \code{B} shared standard-normal draws used to Monte-Carlo
#'     integrate Y out of the missingness likelihood.
#'   \item For each chunk of units, precompute the (units x K) grids of
#'     \eqn{E[Y \mid X_1, X_2]} and of the Y-free part of the missingness
#'     model's linear predictor, then accumulate, over the B draws,
#'     \eqn{P(M = 0 \mid X_1, X_2, Y)} evaluated at
#'     \eqn{Y = E[Y \mid X_1,X_2] + \sigma_Y z_b}. Averaging over the B
#'     draws approximates \eqn{P(M = 0 \mid X_1, X_2)} for every
#'     (unit, candidate) pair.
#'   \item Normalise the resulting (units x K) importance weights per unit
#'     (per row) and resample \code{m} candidate indices per unit with
#'     probability proportional to its weights.
#' }
#'
#' Because the proposal for X1 is the true prior itself, the importance
#' weight for each candidate is exactly the (Y-marginalised) likelihood
#' \eqn{P(M = 0 \mid X_1, X_2)}; there is no additional prior/proposal
#' density ratio to apply.
#'
#' @param X2 Numeric vector. Covariate values of the units to draw for
#'   (typically the units with missing X1 in a test set).
#' @param m Integer. Number of imputed values to draw per unit.
#' @param theta List. Data-generating model parameters (as in config.R).
#' @param beta_phi Numeric vector. Coefficients of the logistic missingness
#'   model for M, including the intercept.
#' @param K Integer. Number of candidate X1 values (SIR particles) shared
#'   across all units. Defaults to 1000.
#' @param B Integer. Number of Monte Carlo draws used to integrate Y out of
#'   the missingness likelihood for each (unit, candidate) pair. Defaults
#'   to 200.
#' @param chunk_size Integer. Number of units processed per chunk, to bound
#'   the (units x K) intermediate matrices' memory use. Defaults to 1000.
#' @param min_ess Numeric in (0,1]. Minimum acceptable per-unit effective
#'   sample size fraction (ESS / K); a warning is issued listing how many
#'   units fall below this threshold, as a diagnostic for weight
#'   degeneracy. Defaults to 0.01.
#' @param verbose Logical. If TRUE, prints a one-line progress/ESS summary
#'   per chunk via \code{log_step()}. Defaults to FALSE.
#'
#' @return A list with elements:
#' \describe{
#'   \item{draws}{An n x m numeric matrix of imputed X1 draws (n =
#'     \code{length(X2)}).}
#'   \item{ess_frac}{Numeric vector of length n, the per-unit effective
#'     sample size fraction (in (0,1]) achieved by the SIR approximation --
#'     a low value flags units where the resampled draws are dominated by
#'     very few candidates.}
#' }
sample_x1_mc = function(X2, m, theta, beta_phi,
                        K = 1000, B = 200, chunk_size = 1000,
                        min_ess = 0.01, verbose = FALSE) {
  check_numeric_vector(X2, "X2")

  n = length(X2)

  mu1 = theta[["X1"]][["beta"]][["(Intercept)"]]
  sd1 = theta[["X1"]][["sigma"]]
  sdY = theta[["Y"]][["sigma"]]

  beta0 = theta[["Y"]][["beta"]][["(Intercept)"]]
  beta1 = theta[["Y"]][["beta"]][["X1"]]
  beta2 = theta[["Y"]][["beta"]][["X2"]]

  ## Shared proposal candidates and Y-integration draws, reused across all
  ## units/chunks (common random numbers).
  x1_candidates = rnorm(K, mu1, sd1)
  z             = rnorm(B)

  draws    = matrix(NA_real_, nrow = n, ncol = m)
  ess_frac = numeric(n)

  n_chunks = ceiling(n / chunk_size)
  chunk_i  = 0

  for (start in seq(1, n, by = chunk_size)) {
    chunk_i = chunk_i + 1
    idx = start:min(start + chunk_size - 1, n)
    x2_chunk = X2[idx]
    nb = length(idx)

    ## (nb x K) grids, computed once per chunk
    muY_grid = outer(x2_chunk, x1_candidates,
                     function(x2, x1) beta0 + beta1 * x1 + beta2 * x2)
    lp_base  = outer(x2_chunk, x1_candidates,
                     function(x2, x1) beta_phi[["(Intercept)"]] +
                       beta_phi[["X1"]] * x1 + beta_phi[["X2"]] * x2)

    ## Accumulate P(M = 0 | X1, X2, Y) over the B shared Y-noise draws
    weight_accum = matrix(0, nrow = nb, ncol = K)
    for (b in seq_len(B)) {
      lp = lp_base + beta_phi[["Y"]] * (muY_grid + sdY * z[b])
      weight_accum = weight_accum + (1 - plogis(lp))
    }

    row_sums = rowSums(weight_accum)
    if (any(row_sums <= 0)) {
      stop("sample_x1_mc(): degenerate importance weights (all-zero) for ",
           "at least one unit; try increasing K or B.", call. = FALSE)
    }
    weight_norm = weight_accum / row_sums

    ess_frac_chunk = 1 / (K * rowSums(weight_norm^2))
    ess_frac[idx] = ess_frac_chunk

    for (i in seq_len(nb)) {
      draws[idx[i], ] = sample(x1_candidates, size = m, replace = TRUE,
                               prob = weight_norm[i, ])
    }

    if (verbose) {
      log_step(sprintf(
        "sample_x1_mc: chunk %d/%d (%d units) | mean ESS frac %.3f | min ESS frac %.3f",
        chunk_i, n_chunks, nb, mean(ess_frac_chunk), min(ess_frac_chunk)
      ), indent = 2)
    }
  }

  n_low_ess = sum(ess_frac < min_ess)
  if (n_low_ess > 0) {
    warning(sprintf(
      "sample_x1_mc(): %d/%d units have an effective sample size fraction below %.3f (weight degeneracy); consider increasing K and/or B.",
      n_low_ess, n, min_ess
    ), call. = FALSE)
  }

  list(draws = draws, ess_frac = ess_frac)
}

## =============================================================================
## "With outcome" variants: X1 | X2, Y  and  X1 | X2, Y, M = 0
## =============================================================================
#
# These are the *illustrative-only* counterparts of sample_x1_mu()/
# sample_x1_mc() used to build a validation-time imputation model that
# (unrealistically, since Y is never available at deployment) is also given
# the true outcome Y. They exist to isolate how much of the
# optimal-vs-estimated imputation gap seen in the ordinary (Y-free) analysis
# is attributable to *not* conditioning on Y, by showing what happens once
# both the "optimal" and "estimated" (mice) imputation models are allowed to
# use it.
#
# x1_given_x2y_moments(): under theta, (X1, Y) given X2 = x2 is exactly
#   bivariate Gaussian (X1 ~ N(mu1, sd1^2), independent of X2, and
#   Y = beta0 + beta1*X1 + beta2*X2 + N(0, sdY^2)), so X1 | X2, Y has a
#   closed-form Gaussian conditional -- no SIR needed for this piece.
#
# sample_x1_mu_y(): draws directly from that closed-form P(X1 | X2, Y).
#
# sample_x1_mc_y(): draws from P(X1 | X2, Y, M = 0) via SIR, using the
#   closed-form P(X1 | X2, Y) as the proposal and the (now fully known,
#   since Y is given) likelihood P(M = 0 | X1, X2, Y) as the importance
#   weight. Unlike sample_x1_mc(), no inner Monte Carlo loop over Y is
#   needed -- Y is observed here, not integrated out -- so this is both
#   simpler and cheaper.

#' Closed-form conditional moments of X1 given (X2, Y) under theta
#'
#' @param X2,Y Numeric vectors of equal length.
#' @param theta List. Data-generating model parameters (as in config.R).
#'
#' @return A list with elements \code{mean} (numeric vector, length
#'   \code{length(X2)}) and \code{sd} (numeric scalar, shared across units).
x1_given_x2y_moments = function(X2, Y, theta) {
  check_numeric_vector(X2, "X2")
  check_numeric_vector(Y, "Y")
  if (length(X2) != length(Y)) {
    stop("x1_given_x2y_moments(): X2 and Y must have the same length.", call. = FALSE)
  }

  mu1 = theta[["X1"]][["beta"]][["(Intercept)"]]
  sd1 = theta[["X1"]][["sigma"]]
  sdY = theta[["Y"]][["sigma"]]

  beta0 = theta[["Y"]][["beta"]][["(Intercept)"]]
  beta1 = theta[["Y"]][["beta"]][["X1"]]
  beta2 = theta[["Y"]][["beta"]][["X2"]]

  ## Cov(X1, Y | X2) = beta1 * sd1^2 ; Var(Y | X2) = beta1^2 * sd1^2 + sdY^2
  cov_x1y = beta1 * sd1^2
  var_y   = beta1^2 * sd1^2 + sdY^2

  mean_x1 = mu1 + (cov_x1y / var_y) * (Y - (beta0 + beta2 * X2 + beta1 * mu1))
  sd_x1   = sqrt(sd1^2 - cov_x1y^2 / var_y)

  list(mean = mean_x1, sd = sd_x1)
}

#' Draw multiply-imputed values of X1 from the true P(X1 | X2, Y)
#'
#' Closed-form counterpart of \code{sample_x1_mu()} for the (illustrative,
#' not deployment-realistic) case where the validation-time imputation model
#' is also given the true outcome Y.
#'
#' @param X2,Y Numeric vectors. Covariate/outcome values of the units to
#'   draw for (typically the units with missing X1 in a test set).
#' @param m Integer. Number of imputed values to draw per unit.
#' @param theta List. Data-generating model parameters (as in config.R).
#'
#' @return An n x m numeric matrix of draws (n = \code{length(X2)}).
sample_x1_mu_y = function(X2, Y, m, theta) {
  mom = x1_given_x2y_moments(X2, Y, theta)
  n = length(X2)
  matrix(rnorm(n * m, mean = rep(mom$mean, times = m), sd = mom$sd),
        nrow = n, ncol = m)
}

#' Draw multiply-imputed values of X1 from the true P(X1 | X2, Y, M = 0) via SIR
#'
#' Same SIR logic as \code{sample_x1_mc()}, but conditioning on the (given,
#' known) outcome Y instead of marginalising over it: the proposal is the
#' closed-form Gaussian \eqn{P(X_1 \mid X_2, Y)} (see
#' \code{x1_given_x2y_moments()}), and each candidate's importance weight is
#' exactly \eqn{P(M = 0 \mid X_1, X_2, Y)} -- no inner Monte Carlo loop over
#' Y is required since Y is already known for every unit.
#'
#' @param X2,Y Numeric vectors. Covariate/outcome values of the units to
#'   draw for.
#' @param m Integer. Number of imputed values to draw per unit.
#' @param theta List. Data-generating model parameters (as in config.R).
#' @param beta_phi Numeric vector. Coefficients of the logistic missingness
#'   model for M, including the intercept.
#' @param K Integer. Number of candidate X1 values (SIR particles) drawn per
#'   unit. Defaults to 1000.
#' @param chunk_size Integer. Number of units processed per chunk, to bound
#'   the (units x K) intermediate matrices' memory use. Defaults to 1000.
#' @param min_ess Numeric in (0,1]. Minimum acceptable per-unit effective
#'   sample size fraction (ESS / K); see \code{sample_x1_mc()}. Defaults to
#'   0.01.
#' @param verbose Logical. If TRUE, prints a one-line progress/ESS summary
#'   per chunk via \code{log_step()}. Defaults to FALSE.
#'
#' @return A list with elements \code{draws} (n x m numeric matrix) and
#'   \code{ess_frac} (numeric vector of length n), as in \code{sample_x1_mc()}.
sample_x1_mc_y = function(X2, Y, m, theta, beta_phi,
                          K = 1000, chunk_size = 1000,
                          min_ess = 0.01, verbose = FALSE) {
  mom = x1_given_x2y_moments(X2, Y, theta)
  n = length(X2)

  ## Shared standard-normal offsets: candidate_{i,k} = mean[i] + sd * z[k].
  ## (sd is identical across units, so the same z can be reused everywhere.)
  z = rnorm(K)

  draws    = matrix(NA_real_, nrow = n, ncol = m)
  ess_frac = numeric(n)

  n_chunks = ceiling(n / chunk_size)
  chunk_i  = 0

  for (start in seq(1, n, by = chunk_size)) {
    chunk_i = chunk_i + 1
    idx = start:min(start + chunk_size - 1, n)
    nb = length(idx)

    ## (nb x K) grid of candidates for this chunk
    x1_candidates = outer(mom$mean[idx], mom$sd * z, "+")

    lp = beta_phi[["(Intercept)"]] +
      beta_phi[["X1"]] * x1_candidates +
      beta_phi[["X2"]] * X2[idx] +
      beta_phi[["Y"]]  * Y[idx]
    weight = 1 - plogis(lp)

    row_sums = rowSums(weight)
    if (any(row_sums <= 0)) {
      stop("sample_x1_mc_y(): degenerate importance weights (all-zero) for ",
           "at least one unit; try increasing K.", call. = FALSE)
    }
    weight_norm = weight / row_sums

    ess_frac_chunk = 1 / (K * rowSums(weight_norm^2))
    ess_frac[idx] = ess_frac_chunk

    for (i in seq_len(nb)) {
      draws[idx[i], ] = sample(x1_candidates[i, ], size = m, replace = TRUE,
                               prob = weight_norm[i, ])
    }

    if (verbose) {
      log_step(sprintf(
        "sample_x1_mc_y: chunk %d/%d (%d units) | mean ESS frac %.3f | min ESS frac %.3f",
        chunk_i, n_chunks, nb, mean(ess_frac_chunk), min(ess_frac_chunk)
      ), indent = 2)
    }
  }

  n_low_ess = sum(ess_frac < min_ess)
  if (n_low_ess > 0) {
    warning(sprintf(
      "sample_x1_mc_y(): %d/%d units have an effective sample size fraction below %.3f (weight degeneracy); consider increasing K.",
      n_low_ess, n, min_ess
    ), call. = FALSE)
  }

  list(draws = draws, ess_frac = ess_frac)
}