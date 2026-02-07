################################################################################
## Configuration file
## Defines the design of the simulation study
################################################################################

## =============================================================================
## 1. Sample size
## =============================================================================

set_size = 1000 # Size of both training and testing datasets
train_size = set_size # If different sizes for train and test are required
test_size  = set_size # If different sizes for train and test are required
monte_carlo_size = 1e5 # Size of the sample for Monte-Carlo approximation of reference probabilities

## =============================================================================
## 2. Data-generating model parameters
## =============================================================================

## -----------------------------------------------------------------------------
## Parameters of the joint distribution of X1, X2, Y
## -----------------------------------------------------------------------------

## Interpretation:
## X1 ~ N(beta_X1, sigma_X1^2)
## X2 ~ N(beta_X2, sigma_X2^2)
## Y  ~ N(beta_Y0 + beta_Y1 * X1 + beta_Y2 * X2, sigma_Y^2)

theta = list("X1" = list("beta" = c("(Intercept)" = 1),
                         "sigma" = 1),
             "X2" = list("beta" = c("(Intercept)" = -1),
                         "sigma" = 1),
             "Y"  = list("beta" = c("(Intercept)" = 1,
                                    "X1" = 1,
                                    "X2" = -1),
                         "sigma" = 1))


## -----------------------------------------------------------------------------
## Parameters of the conditional distribution MX1 | X1, X2, Y
## -----------------------------------------------------------------------------

missingness_scenarios = c("M1", # MX1 ~ 1
                          "M2", # MX1 ~ X2,
                          "M3", # MX1 ~ X1,
                          "M4", # MX1 ~ X1 + Y,
                          "M5") # MX1 ~ Y,

## Notes:
##   - the null coefficients in the beta vector of phi ensure that the
##     missingness mechanism is respeected
##   - the intercept is adjusted by the function tune_missingness_intercept() 
##     to ensure that the expected value of MX1 is approximately equal to the
##     missingness proportion target

phi = list("M1" = list("beta" = c("(Intercept)" = 0,
                                  "X1" = 0,
                                  "X2" = 0,
                                  "Y" = 0)),
           "M2" = list("beta" = c("(Intercept)" = 0,
                                  "X1" = 0,
                                  "X2" = -1,
                                  "Y" = 0)),
           "M3" = list("beta" = c("(Intercept)" = 0,
                                  "X1" = -2,
                                  "X2" = 0,
                                  "Y" = 0)),
           "M4" = list("beta" = c("(Intercept)" = 0,
                                  "X1" = 2,
                                  "X2" = 0,
                                  "Y" = 2)),
           "M5" = list("beta" = c("(Intercept)" = 0,
                                  "X1" = 0,
                                  "X2" = 0,
                                  "Y" = 1)))

## =============================================================================
## 3. Missingness proportions
## =============================================================================

missingness_proportion_MX1_min  = 0
missingness_proportion_MX1_max  = 0.7
missingness_proportion_MX1_step = 0.01
missingness_grid = seq(missingness_proportion_MX1_min,
                       missingness_proportion_MX1_max,
                       by = missingness_proportion_MX1_step)

missingness_target_Y = 0.4 # For secondary analysis

## =============================================================================
## 4. Training procedures
## =============================================================================

training_procedures = list(
  "ps" = list("procedure" = train_ps),
  "ccs" = list("procedure" = train_ccs), 
  "mle" = list("procedure" = train_mle),
  "mle_mi" = list("procedure" = train_mle_mi),
  "mi" = list("procedure" = train_mi),
  "mimi" = list("procedure" = train_mimi)
)

## =============================================================================
## 5. Performance metrics
## =============================================================================

## mse      : mean squared error
##          : = mean( (Y-Ypred)^2 )
## msep_omu : mean squared error of prediction with Oracle MU reference
##            = mean( (P(Y|X1,X2)-Ypred)^2 )
## msep_omc : mean squared error of prediction with Oracle MC reference
##            = mean( (P(Y|X1,X2,MX1)-Ypred)^2 )
performance_metrics = c("mse","msep_omu","msep_omc")

## =============================================================================
## 6. Analysis groups
## =============================================================================

analysis_groups = c("overall","complete","incomplete")

## =============================================================================
## 7. LOESS parameters
## =============================================================================

loess_span = 1