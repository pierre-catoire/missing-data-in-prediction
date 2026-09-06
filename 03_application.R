################################################################################
## Application study
##
## CHECKPOINTING / RESUME: the leave-one-out loop below saves its (partial)
## `predictions` list to output/application/raw/predictions_checkpoint.rds
## every `checkpoint_every` observations. On startup, if that checkpoint
## file already exists, observations already computed (non-NA entries) are
## skipped, so the process can be killed and restarted without redoing the
## whole leave-one-out loop.
################################################################################
## 1. Packages
library(dplyr) # For data manipulation
library(naniar) # Plotting distribution of missingness patterns
library(table1) # Format descriptive tables
library(mice) # Multiple imputation
library(ggplot2) # Plotting
library(knitr) # kable function

## 2. Reproducibility
base_seed = 314159
set.seed(base_seed)

## 3. Functions
source("R/functions/application_functions.R")
source("R/functions/logging_utils.R")

## 4. Load dataset
load("input/dataset_application.rda")

## 5. Output directories
dir.create("output/application/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("output/application/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("output/application/raw",     showWarnings = FALSE, recursive = TRUE)

## =============================================================================
## Population characteristics
## =============================================================================

# Create table describing population characteristics
table_population_characteristics = table1(as.formula("~AGE+AMS+HYPOX+COAG|Y"),
                                          dataset,
                                          render.continuous = c("Median [IQR]" = "MEDIAN [Q1-Q3]"))

table_population_characteristics_formatted = kable(table_population_characteristics,
                                                   format = "latex",
                                                   align = "c",
                                                   caption = "Population characteristics")

writeLines(table_population_characteristics_formatted,
           "output/application/tables/table1_population_characteristics.tex")

## =============================================================================
## Distribution of missingness patterns
## =============================================================================

# Create missingness pattern distribution figure
plot_distribution_of_missingness_patterns = gg_miss_upset(rename(dataset,
                                                                 Age = AGE,
                                                                 "Altered Mental Status" = "AMS",
                                                                 "Hypoxemia" = "HYPOX",
                                                                 "Coagulation disorder" = "COAG",
                                                                 "Significant trauma" = "Y"))

pdf("output/application/figures/fig1_missingness_patterns.pdf", width = 8, height = 6)
print(plot_distribution_of_missingness_patterns)
dev.off()

## =============================================================================
## Evaluation of training procedures
## =============================================================================
predictors = c("AGELOG", "AMS", "HYPOX" , "COAG")

n_obs = nrow(dataset)
checkpoint_every = 20
predictions_checkpoint_file = "output/application/raw/predictions_checkpoint.rds"

## Initialise prediction arrays, or resume from a previous (possibly
## interrupted) run's checkpoint
if (file.exists(predictions_checkpoint_file)) {
  predictions = readRDS(predictions_checkpoint_file)
  log_step(sprintf(
    "Resuming leave-one-out application study from checkpoint: %d/%d observations already done.",
    sum(!is.na(predictions[["PS"]])), n_obs
  ))
} else {
  predictions = list(
    "PS"   = rep(NA, n_obs),
    "MI"   = rep(NA, n_obs),
    "MIMI" = rep(NA, n_obs)
  )
}

log_step(sprintf(
  "Starting leave-one-out application study | %d observations | checkpoint every %d",
  n_obs, checkpoint_every
))

loop_start = Sys.time()

# Loop over observations
for (i in 1:n_obs) {

  if (!is.na(predictions[["PS"]][i])) next # already done (resume)

  # Split into training and testing sets
  split = 1:n_obs %in% i
  data_train = dataset[!split,]
  data_test  = dataset[split,]

  # Apply training procedures
  predictions[["PS"]][i]   = predps  (data_test,fitps  (data_train, predictors))
  predictions[["MI"]][i]   = predmi  (data_test,fitmi  (data_train, predictors))
  predictions[["MIMI"]][i] = predmimi(data_test,fitmimi(data_train, predictors))

  if (i %% 10 == 0 || i == n_obs) {
    log_progress(i, n_obs, loop_start, label = "leave-one-out")
  }

  if (i %% checkpoint_every == 0) {
    saveRDS(predictions, predictions_checkpoint_file)
    log_step(sprintf("Checkpoint saved (%d/%d done).", i, n_obs), indent = 1)
  }
}

## Final checkpoint save, so a completed run leaves the checkpoint file
## consistent with predictions.rda below.
saveRDS(predictions, predictions_checkpoint_file)

log_step("Leave-one-out application study finished.")

save(predictions, file = "output/application/raw/predictions.rda")

## Produce the results table
patterns = get_patterns(dataset, predictors)
y_true = as.numeric(1*(dataset["Y"]==1))

table_results = compute_mse_table(predictions, patterns, y_true) 
table_results_formatted = kable(table_results,
                                format = "latex",
                                align = "c",
                                caption = "Performance of evaluated procedures")

writeLines(table_results_formatted,
           "output/application/tables/table2_performance_of_evaluated_procedures.tex")

capture.output(
  sessionInfo(),
  file = "output/application/sessionInfoMain.txt"
)
