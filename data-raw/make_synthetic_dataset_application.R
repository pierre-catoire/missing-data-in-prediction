################################################################################
## Synthetic dataset for the application study
##
## The clinical dataset analysed in the article (678 trauma patients, Bordeaux
## University Hospital, 2017 to 2019) cannot be shared because of healthcare
## data protection regulations. This script builds a synthetic dataset with the
## same structure, so that 03_application.R can be run end to end.
##
## The synthetic dataset is generated from AGGREGATE STATISTICS ONLY, as
## published in the article:
##   * Supplementary Material, Table 2: number of patients by outcome, age
##     median and interquartile range by outcome, number of positive and of
##     missing values of each binary predictor by outcome;
##   * Main text, Table 1: number of patients in each observation pattern.
## No individual record of the clinical dataset is used.
##
## Construction:
##   1. Outcome: exactly 531 patients without and 147 with severe trauma.
##   2. Observation patterns: the joint table (outcome x pattern) is the
##      maximum entropy table (iterative proportional fitting) matching the
##      pattern counts and the number of missing values of each predictor by
##      outcome; it is then rounded to integers that match all these margins
##      exactly.
##   3. Binary predictors: among the patients with an observed value, the
##      number of positive values by outcome matches Supplementary Table 2
##      exactly; positives are allocated at random, independently of the
##      observation pattern given the outcome.
##   4. Age: drawn, by outcome, from a piecewise linear quantile function going
##      through the published quartiles, with stratified draws (minimum 18, as patients under 18 were
##      excluded, and maximum 90).
##
## As a consequence, the synthetic dataset reproduces the published margins,
## but not the associations between predictors, nor between age and
## missingness: results of 03_application.R on this dataset are NOT expected to
## reproduce Table 1 of the article.
##
## Usage (from the project root):
##   Rscript data-raw/make_synthetic_dataset_application.R
## Output:
##   input/synthetic_dataset_application.rda (a data frame named `dataset`)
################################################################################

set.seed(20261126)

## -----------------------------------------------------------------------------
## 1. Published aggregate statistics
## -----------------------------------------------------------------------------

n_by_y = c("0" = 531, "1" = 147)

# Observation patterns (1 = missing) for (AMS, HYPOX, COAG), main text Table 1
patterns = data.frame(
  AMS   = c(0, 0, 0, 1, 0, 1, 1, 1),
  HYPOX = c(0, 1, 0, 0, 1, 1, 0, 1),
  COAG  = c(0, 0, 1, 0, 1, 0, 1, 1),
  n     = c(424, 93, 78, 45, 15, 11, 10, 2)
)
stopifnot(sum(patterns$n) == sum(n_by_y))

predictors_binary = c("AMS", "HYPOX", "COAG")

# Supplementary Table 2: missing values and positive values, by outcome
n_missing = rbind("0" = c(AMS = 45, HYPOX = 73, COAG = 80),
                  "1" = c(AMS = 23, HYPOX = 48, COAG = 25))
n_positive = rbind("0" = c(AMS = 40, HYPOX = 41, COAG = 36),
                   "1" = c(AMS = 26, HYPOX =  9, COAG = 19))

# Supplementary Table 2: age quartiles, by outcome
age_quartiles = rbind("0" = c(25, 37, 51),
                      "1" = c(29, 44, 57))
age_min = 18
age_max = 90

## -----------------------------------------------------------------------------
## 2. Joint table of outcome and observation pattern
## -----------------------------------------------------------------------------

# Maximum entropy table by iterative proportional fitting
ipf_table = matrix(1, nrow = 2, ncol = nrow(patterns),
                   dimnames = list(names(n_by_y), NULL))
for (iteration in 1:5000) {
  ipf_table = ipf_table * rep(n_by_y / rowSums(ipf_table), ncol(ipf_table))
  ipf_table = t(t(ipf_table) * (patterns$n / colSums(ipf_table)))
  for (predictor in predictors_binary) {
    is_missing = patterns[[predictor]] == 1
    for (y in names(n_by_y)) {
      target_missing = n_missing[y, predictor]
      current_missing = sum(ipf_table[y, is_missing])
      current_observed = sum(ipf_table[y, !is_missing])
      ipf_table[y, is_missing] = ipf_table[y, is_missing] * target_missing / current_missing
      ipf_table[y, !is_missing] = ipf_table[y, !is_missing] *
        (n_by_y[y] - target_missing) / current_observed
    }
  }
}

# Integer table matching all margins exactly, closest to the IPF table: search
# over the counts of the first outcome group around their rounded IPF values
# (the counts of the second group follow from the pattern counts).
candidates = lapply(seq_len(nrow(patterns)), function(k) {
  centre = round(ipf_table["0", k])
  values = (centre - 2):(centre + 2)
  values[values >= 0 & values <= patterns$n[k]]
})
grid = as.matrix(expand.grid(candidates))
is_valid = rowSums(grid) == n_by_y["0"]
for (predictor in predictors_binary) {
  is_missing = patterns[[predictor]] == 1
  is_valid = is_valid & rowSums(grid[, is_missing, drop = FALSE]) == n_missing["0", predictor]
}
grid = grid[is_valid, , drop = FALSE]
if (nrow(grid) == 0) stop("No integer table matches the margins; widen the search.")
distance = rowSums(sweep(grid, 2, ipf_table["0", ])^2)
counts_y0 = grid[which.min(distance), ]
joint_counts = rbind("0" = counts_y0, "1" = patterns$n - counts_y0)

## -----------------------------------------------------------------------------
## 3. Individual records
## -----------------------------------------------------------------------------

simulate_age = function(n, quartiles) {
  # Stratified uniform draws, so that the sample quartiles match the published
  # ones up to rounding
  u = sample((seq_len(n) - 0.5) / n)
  age = approx(x = c(0, 0.25, 0.5, 0.75, 1),
               y = c(age_min, quartiles, age_max),
               xout = u)$y
  as.integer(round(age))
}

records = list()
for (y in names(n_by_y)) {
  pattern_index = rep(seq_len(nrow(patterns)), times = joint_counts[y, ])
  group = data.frame(AGE = simulate_age(n_by_y[y], age_quartiles[y, ]))
  for (predictor in predictors_binary) {
    is_missing = patterns[[predictor]][pattern_index] == 1
    values = rep(NA_integer_, n_by_y[y])
    observed = which(!is_missing)
    values[observed] = 0L
    values[sample(observed, n_positive[y, predictor])] = 1L
    group[[predictor]] = values
  }
  group$Y = as.integer(y)
  records[[y]] = group
}
dataset = do.call(rbind, records)
dataset = dataset[sample(nrow(dataset)), ]
rownames(dataset) = NULL

# Same structure as the dataset expected by 03_application.R
dataset = data.frame(
  AGE    = dataset$AGE,
  AMS    = factor(dataset$AMS,   levels = c(0, 1)),
  HYPOX  = factor(dataset$HYPOX, levels = c(0, 1)),
  COAG   = factor(dataset$COAG,  levels = c(0, 1)),
  Y      = factor(dataset$Y,     levels = c(0, 1)),
  AGELOG = log(dataset$AGE)
)

## -----------------------------------------------------------------------------
## 4. Checks against the published margins
## -----------------------------------------------------------------------------

stopifnot(all(table(dataset$Y) == n_by_y))
for (predictor in predictors_binary) {
  for (y in names(n_by_y)) {
    in_group = dataset$Y == y
    stopifnot(sum(is.na(dataset[[predictor]][in_group])) == n_missing[y, predictor])
    stopifnot(sum(dataset[[predictor]][in_group] == "1", na.rm = TRUE) == n_positive[y, predictor])
  }
}
observed_patterns = table(apply(is.na(dataset[, predictors_binary]) * 1, 1, paste, collapse = ""))
expected_patterns = setNames(patterns$n, apply(patterns[, predictors_binary], 1, paste, collapse = ""))
stopifnot(all(observed_patterns[names(expected_patterns)] == expected_patterns))

cat("Age quartiles by outcome (synthetic):\n")
print(tapply(dataset$AGE, dataset$Y, quantile, probs = c(0.25, 0.5, 0.75)))
cat("Age quartiles overall (synthetic; published: 26, 39, 52):\n")
print(quantile(dataset$AGE, probs = c(0.25, 0.5, 0.75)))
cat("Joint counts of outcome and pattern (AMS, HYPOX, COAG; 1 = missing):\n")
colnames(joint_counts) = names(expected_patterns)
print(joint_counts)

## -----------------------------------------------------------------------------
## 5. Save
## -----------------------------------------------------------------------------

dir.create("input", showWarnings = FALSE)
save(dataset, file = "input/synthetic_dataset_application.rda")
cat("Saved input/synthetic_dataset_application.rda\n")
