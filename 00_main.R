################################################################################
## Run all analyses
##
## All four stages below are checkpointed and resumable (01, 02 and 04 skip
## any grid point already saved to output/*/raw/; 03 skips any leave-one-out
## observation already saved to its checkpoint). Killing this process (or a
## crash) and re-running `Rscript 00_main.R` picks up where it left off
## instead of recomputing everything from scratch -- the one exception being
## a first run of 03 after adding its checkpointing: its resume file
## (output/application/raw/predictions_checkpoint.rds) does not exist yet
## the first time, so that stage's leave-one-out loop runs in full once, and
## resumes normally on any run after that.
################################################################################

source("01_simulation_main_analysis.R")
source("02_simulation_secondary_analysis.R")
source("03_application.R")
source("04_validation_analysis.R")
system("make -C latex/figures")