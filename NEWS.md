# Version 2.0 (2026-11-26)

Revision of the article for *Biostatistics*.

## Correction

-   `R/functions/application_functions.R`: in `predmi()` and `predmimi()`, observed binary predictors stored as factors were converted with `as.numeric()`, which returns the level index (1 or 2) instead of the value (0 or 1), both when used in the prediction model and when used as covariates of the imputation models. They are now encoded as 0 or 1 (`encode_value()`), and the marginalisation over missing predictors is shared by both functions (`predict_marginalised()`). This changed the application results (Table 1 of the article); simulation results are not affected, as the simulated predictors are continuous.

## Data

-   The application dataset `input/dataset_application.rda` is removed from the repository. It is replaced by `input/synthetic_dataset_application.rda`, generated from published aggregate statistics by the new script `data-raw/make_synthetic_dataset_application.R`.
-   `03_application.R` uses the clinical dataset when it is available locally as `input/dataset_application.rda` (now excluded from version control), and the synthetic dataset otherwise; outputs on the synthetic dataset are written to `output/application_synthetic/`.

## Other changes

-   README updated to the revised article: terminology (MARXYM, MARXYO), SNIO, description of the application and of the synthetic dataset, citation.
-   LaTeX build products removed from version control.
-   `CITATION.cff` added.

# Version 1.0 (2026-02-07)

Initial submission.
