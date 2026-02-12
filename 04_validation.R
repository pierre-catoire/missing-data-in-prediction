############################################################
# 0. Setup
############################################################

source("R/functions/training_procedures.R")
source("R/functions/reference_probabilities.R")
source("R/functions/performance_metrics.R")
source("R/functions/validation_functions.R")
source("R/functions/utils.R")
source("R/config/config.R")

dir.create("output/validation/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("output/validation/tables", recursive = TRUE, showWarnings = FALSE)

base_seed = 314159
set.seed(base_seed)


library(mice)


############################################################
# 1. VALIDATION PHASE
############################################################

pooling_methods = c("predictions", "scores", "complete")

for (scenario in missingness_scenarios) {
  
  for (missingness_target_X1 in missingness_grid) {
    
    message("Scenario: ", scenario, ", missingness target: ", missingness_target_X1)
    
    path_simulation_object = sprintf(
      "output/main/raw/%1$s/missingness_target_X1_%2$.3f/simulation_%1$s_%2$.3f.rds",
      scenario,
      missingness_target_X1
    )
    
    simulation_object = readRDS(path_simulation_object)
    
    data_train = simulation_object[["data"]][["train"]]
    data_test  = simulation_object[["data"]][["test"]]
    y_true     = data_test[["Y"]]
    
    model_mi   = train_mi(data_train)
    model_mimi = train_mimi(data_train)
    
    models = list(
      mi   = list(train = model_mi,   predict = predict_mi),
      mimi = list(train = model_mimi, predict = predict_mimi)
    )
    
    settings = expand.grid(
      include_miss_inds = c(FALSE, TRUE),
      include_outcome   = c(FALSE, TRUE)
    )
    
    validation_results = list()
    
    for (i in seq_len(nrow(settings))) {
      
      s = settings[i, ]
      
      imputed_sets = impute_at_validation(
        data_test,
        m = 5,
        predictors = predictors,
        outcome = outcome,
        miss_inds = miss_inds,
        include_miss_inds = s[["include_miss_inds"]],
        include_outcome   = s[["include_outcome"]]
      )
      
      setting_name = paste0(
        ifelse(s[["include_miss_inds"]], "missIndsIncluded", "missIndsExcluded"),
        ifelse(s[["include_outcome"]], "outcomeIncluded", "outcomeExcluded")
      )
      
      validation_results[[setting_name]] = list()
      
      reference_performance = get_reference_performance(imputed_sets,
                                                        y_true,
                                                        scenario)
      
      for (model_name in names(models)) {
        
        for (pool in pooling_methods) {
          
          validation_results[[setting_name]][[model_name]][["predicted"]][[pool]] =
            get_function_performance(
              models[[model_name]][["predict"]],
              models[[model_name]][["train"]],
              imputed_validation_sets = imputed_sets,
              y_true = y_true,
              pooling_method = pool
            )
          
          key_reference = paste0("mse", pool, "ref")
          
          validation_results[[setting_name]][[model_name]][["reference"]][[pool]] =
            reference_performance[[model_name]][[pool]]
          
        }
      }
    }
    
    validation_object = list(
      observed_missingness_train = mean(data_train[["MX1"]]),
      simulation_object = simulation_object,
      validation_results = validation_results
    )
    
    raw_dir = file.path(
      "output/validation/raw",
      scenario,
      sprintf("missingness_target_X1_%0.3f", missingness_target_X1)
    )
    
    dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
    
    saveRDS(
      validation_object,
      file = file.path(
        raw_dir,
        sprintf("validation_%s_%0.3f.rds", scenario, missingness_target_X1)
      )
    )
  }
}

############################################################
# 2. TABLE BUILDING PHASE
############################################################

for (scenario in missingness_scenarios) {
  
  settings = expand.grid(
    include_miss_inds = c(FALSE, TRUE),
    include_outcome   = c(FALSE, TRUE)
  )
  
  for (i in seq_len(nrow(settings))) {
    
    s = settings[i, ]
    
    setting_name = paste0(
      ifelse(s[["include_miss_inds"]], "missIndsIncluded", "missIndsExcluded"),
      ifelse(s[["include_outcome"]], "outcomeIncluded", "outcomeExcluded")
    )
    
    for (model_name in c("mi", "mimi")) {
      table_rows = list()
      final_table = list()
      
      for (missingness_target_X1 in missingness_grid) {
        
        raw_dir = file.path(
          "output/validation/raw",
          scenario,
          sprintf("missingness_target_X1_%0.3f", missingness_target_X1)
        )
        
        validation_object = readRDS(
          file.path(
            raw_dir,
            sprintf("validation_%s_%0.3f.rds", scenario, missingness_target_X1)
          )
        )
        
        simulation_object = validation_object[["simulation_object"]]
        
        vr = validation_object[["validation_results"]][[setting_name]]
        
        
        table_row = data.frame(
          observedmissingness = validation_object[["observed_missingness_train"]],
          targetmissingness   = missingness_target_X1)
        
        for (pool in pooling_methods) {
          table_row[[pool]] = vr[[model_name]][["predicted"]][[pool]]
          table_row[[paste0(pool,"ref")]] = vr[[model_name]][["reference"]][[pool]]
        }
        
        table_rows[[length(table_rows) + 1]] = table_row
      }
      final_table = do.call(base::rbind, table_rows)
      
      write.csv(
        final_table,
        file = file.path(
          "output/validation/tables",
          sprintf(
            "%s_%s_%s_%s_points.csv",
            scenario,
            ifelse(s[["include_outcome"]], "outcomeIncluded", "outcomeExcluded"),
            ifelse(s[["include_miss_inds"]], "missIndsIncluded", "missIndsExcluded"),
            model_name
          )
        ),
        row.names = FALSE
      )
      
      message(sprintf(
        "%s_%s_%s_%s_points.csv",
        scenario,
        ifelse(s[["include_outcome"]], "outcomeIncluded", "outcomeExcluded"),
        ifelse(s[["include_miss_inds"]], "missIndsIncluded", "missIndsExcluded"),
        model_name
      ),
      " /// ",
      nrow(final_table))
      
      ## ----------------------------------------------------------------------
      ## LOESS-smoothed table
      ## ----------------------------------------------------------------------
      x = final_table[["observedmissingness"]]
      
      ## Common prediction grid
      xloess = seq(
        min(x, na.rm = TRUE),
        max(x, na.rm = TRUE),
        length.out = n_loess
      )
      
      df_loess = data.frame(observedmissingness = xloess)
      
      method_names = setdiff(
        names(final_table),
        c("observedmissingness", "targetmissingness")
      )
      
      for (m in method_names) {
        
        df_loess[[m]] =
          loess_smooth_series(
            x = final_table[["observedmissingness"]],
            y = final_table[[m]],
            xloess = xloess,
            span = loess_span,
            trim = loess_trim
          )
      }
      
      write.csv(
        df_loess,
        file = file.path(
          "output/validation/tables",
          sprintf(
            "%s_%s_%s_%s_loess.csv",
            scenario,
            ifelse(s[["include_outcome"]], "outcomeIncluded", "outcomeExcluded"),
            ifelse(s[["include_miss_inds"]], "missIndsIncluded", "missIndsExcluded"),
            model_name
          )
        ),
        row.names = FALSE
      )
    }
  }
}

#TODO: fit LOESS similarly to application

## Session info
capture.output(
  sessionInfo(),
  file = "output/validation/sessionInfoMain.txt"
)