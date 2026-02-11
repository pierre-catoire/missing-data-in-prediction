################################################################################
## Application study
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

## Initialise prediction arrays
predictions = list(
  "PS"   = rep(NA, nrow(dataset)),
  "MI"   = rep(NA, nrow(dataset)),
  "MIMI" = rep(NA, nrow(dataset))
)

# Loop over observations
for (i in 1:nrow(dataset)) {
  message(i,"/",nrow(dataset))
  # Split into training and testing sets
  split = 1:nrow(dataset) %in% i
  data_train = dataset[!split,]
  data_test  = dataset[split,]
  
  # Apply training procedures
  predictions[["PS"]][i]   = predps  (data_test,fitps  (data_train, predictors))
  predictions[["MI"]][i]   = predmi  (data_test,fitmi  (data_train, predictors))
  predictions[["MIMI"]][i] = predmimi(data_test,fitmimi(data_train, predictors))
}

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
