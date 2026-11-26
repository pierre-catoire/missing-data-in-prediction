# Prediction with Missing Data: Target Probabilities and Missingness Mechanisms

*Authors: Pierre Catoire, Robin Genuer, Cécile Proust-Lima* *date: 2026-11-26* *version: 2.0*

This repository contains the code of the simulation study and of the application of the article "Prediction with Missing Data: Target Probabilities and Missingness Mechanisms" (Catoire, Genuer and Proust-Lima, submitted to *Biostatistics*), together with a synthetic dataset mirroring the clinical dataset of the application.

Version 2.0 accompanies the revised version of the article. Changes since version 1.0 are listed in `NEWS.md`.

The objective of the simulation study is to compare predictive performance of several estimation strategies under different missing data mechanisms, including mechanisms that violate classical Missing At Random (MAR) assumptions.

> [!IMPORTANT]
> Documentation is available for all implemented functions, using `?function`. To enable it, install `devtools` package if not done and run:

``` r
devtools::load_all()
```

## Running the full pipeline

The four analysis scripts are orchestrated, in order, by `00_main.R`:

``` r
source("01_simulation_main_analysis.R")     # main simulation study
source("02_simulation_secondary_analysis.R") # secondary study (missing Y, scenario 5)
source("03_application.R")                   # real-data application
source("04_validation_analysis.R")           # RP/PP validation-consistency study
system("make -C latex/figures")              # build every LaTeX figure
```

Each of `01`, `02` and `04` is checkpointed: raw simulation objects (or, for `04`, per-point results) are written to disk as soon as they are computed, and re-launching the same script after an interruption skips any point already on disk instead of recomputing it. The pipeline can therefore safely be killed and restarted at any point.

`04_validation_analysis.R` does not build any figures itself; it only writes the raw/table CSVs. All figures (for `01`/`02`/`04` alike) are built separately by the Makefile in `latex/figures/`, once the relevant tables exist.

## Overview of the simulation design

The main analysis is run in `01_simulation_main_analysis.R`.

Each simulation run proceeds as follows:

1.  Data generation

-   Covariates $X_1$ and $X_2$ are generated from independent Gaussian distributions
-   The outcome $Y$ is generated from a linear Gaussian model conditional on $(X_1, X_2)$
-   Missingness in $X_1$ is generated according to a logistic model, with missingness indicator $M_{X_1}$, that may depend on $X_1$, $X_2$ and/or $Y$, depending on the scenarios.
-   $Y$ is considered always observed in the main analysis. A secondary analysis on scenario 5 assesses the effect of missing $Y$, the mechanism of which is independent from $X_1$, $X_2$, $Y$ and $M_{X_1}$.
-   Missingness indicators are generated alongside the predictors and outcome. The variable values are masked separately.

2.  Missingness scenarios

-   Five missingness mechanisms are considered, as described in the paper:

| MCAR | MAR | MARXYM | MARXYO | NIMO | NICO | SNIO | Scenario |
|:----:|:---:|:------:|:------:|:----:|:----:|:----:|:--------:|
|  ✔   |  ✔  |   ✔    |   ✔    |  ✔   |  ✔   |  ✔   |    1     |
|  ✖   |  ✔  |   ✔    |   ✔    |  ✔   |  ✔   |  ✔   |    2     |
|  ✖   |  ✖  |   ✖    |   ✖    |  ✖   |  ✔   |  ✔   |    3     |
|  ✖   |  ✖  |   ✖    |   ✖    |  ✖   |  ✖   |  ✔   |    4     |
|  ✖   |  ✖  |   ✖    |   ✔    |  ✖   |  ✖   |  ✔   |    5     |

In all scenarios, the missingness of $Y$ is independent of $(Y, X_1, X_2, M_{X_1})$, so that SNIO holds.

-   The intercept of each missingness model is tuned to achieve a target missingness proportion in expectation.

3.  Training and testing sets

-   Independent training and testing datasets are generated for each missingness scenario and target missingness proportion.
-   Missing values are introduced only in the covariates of interest.

4.  Prediction methods

The following approaches are evaluated:

-   Pattern submodels
-   Complete-case submodels
-   Maximum Likelihood Estimation (using Expectation-Maximisation algorithm), with marginalisation of the missing predictors (with and without missingness indicators)
-   Multiple imputation (with and without missingness indicators)

5.  Reference predictors

Performance of optimal prediction functions is represented to illustrate best achievable performance:

-   MU reference: $\Pr(Y \mid X_o)$
-   MC reference: $\Pr(Y \mid X_o, M_X)$

> [!NOTE]
> As it requires Monte Carlo approximation, computation of optimal predictors (MU, MC, OMU, OMC) is computationally heavy, despite parallelisation. If slow, consider reducing the `monte_carlo_size` parameter in `R/config/config.R`. As an alternative, consider reducing the number of simulated datasets with the `missingness_proportion_MX1_step` parameter in `R/config/config.R`.

6.  Performance evaluation

Metrics of performance are:

-   Mean Squared Error (MSE): $\frac{1}{n} \underset{i=1}{\overset{N}{\sum}} \left(Y - \hat{\mathbb{E}}\left[Y \mid \mathcal{E} \right]\right)^2$
-   Mean Squared Error of Prediction (MSEP):
    -   with Oracle MU reference (MSEP-OMU): $\frac{1}{n} \underset{i=1}{\overset{N}{\sum}} \left(\Pr(Y \mid X_o, X_m) - \hat{\mathbb{E}}\left[Y \mid \mathcal{E} \right]\right)^2$
    -   with Oracle MC reference (MSEP-OMC): $\frac{1}{n} \underset{i=1}{\overset{N}{\sum}} \left(\Pr(Y \mid X_o, X_m, M_X) - \hat{\mathbb{E}}\left[Y \mid \mathcal{E} \right]\right)^2$

*With* $\mathcal{E}$ the evidence used by the prediction function: $X_o$ or $X_o, M_X$ depending on the prediction function. See the article for details.

7.  Output

Output contains:

-   Raw performance results, stored as `simulation_object` lists, loadable as `.rds` files in `output/main/raw/`, containing training and testing datasets, predictions of each prediction function, reference probabilities, and metadata including the missingness scenario and the target and observed missingness proportions.
-   Performance metrics of each method and reference, for any given scenario, performance metric and analysis group (all cases: overall; complete and incomplete cases), written to `output/main/tables/`. For each combination, a pair of tables is produced:
    -   the points, corresponding to the observed performance (example: `M2_overall_mse_points.csv`)
    -   a smoothing LOESS curve for better visualisation in the plots (example: `M2_overall_mse_loess.csv`)

## Secondary analysis (`02_simulation_secondary_analysis.R`)

The aim of the secondary analysis is to discuss the importance of subsetting the training dataset by excluding observations with missing outcome when the mechanism is MNAR but MARXYO holds. Using the datasets already generated for scenario 5 by the main analysis:

-   missingness in $Y$ is introduced completely at random (independent of $X_1$, $X_2$, $Y$ and $M_{X_1}$), with a probability of 40%
-   MLE and MI methods are fitted on both the complete training set (with missing $Y$) and the subset with only observed $Y$
-   their performances are compared with the MU and MC references

Results are written to `output/secondary/tables/`.

## Application study (`03_application.R`)

The application study predicts severe trauma (Injury Severity Score of 15 or more) from age (always observed), altered mental status, hypoxemia and coagulation disorder (possibly missing), among 678 patients admitted to the trauma bay of Bordeaux University Hospital between 2017 and 2019. Pattern submodels, multiple imputation, and multiple imputation with missingness indicators are compared by leave-one-out Brier score.

> [!IMPORTANT]
> The clinical dataset cannot be shared because of healthcare data protection regulations. Access may be granted upon reasonable request, subject to the applicable ethical approvals and data access regulations. The repository provides instead a **synthetic dataset**, `input/synthetic_dataset_application.rda`, with the same variables and structure.

### Synthetic dataset

`input/synthetic_dataset_application.rda` is generated by `data-raw/make_synthetic_dataset_application.R` from **aggregate statistics only**, as published in the article (Supplementary Material, Table 2, and main text, Table 1). No individual record of the clinical dataset was used. It reproduces exactly:

-   the number of patients with and without severe trauma (147 and 531);
-   the number of missing values and of positive values of each binary predictor, by outcome;
-   the number of patients in each of the 8 observation patterns;
-   the quartiles of age by outcome (up to rounding).

The joint distribution of outcome and observation pattern is the maximum entropy distribution matching these margins. Given the outcome, predictor values are independent of each other, of age and of the observation pattern. Running `03_application.R` on the synthetic dataset therefore exercises the whole pipeline, but its results are **not** expected to reproduce Table 1 of the article.

To regenerate it, run from the project root:

``` bash
Rscript data-raw/make_synthetic_dataset_application.R
```

### Running the application

If the clinical dataset is available locally as `input/dataset_application.rda` (this path is excluded from version control by `.gitignore`), `03_application.R` uses it and writes its outputs to `output/application/`. Otherwise, it uses the synthetic dataset and writes its outputs to `output/application_synthetic/`. The variables are:

| Variable | Description | Type |
|:---------|:------------|:-----|
| `AGE` | age (years) | integer, always observed |
| `AGELOG` | logarithm of age, used as predictor | numeric, always observed |
| `AMS` | altered mental status (Glasgow Coma Scale below 15) | factor (`0`, `1`), possibly missing |
| `HYPOX` | hypoxemia | factor (`0`, `1`), possibly missing |
| `COAG` | coagulation disorder | factor (`0`, `1`), possibly missing |
| `Y` | severe trauma | factor (`0`, `1`), always observed |

The script produces, directly (outside the LaTeX/Makefile pipeline), in `output/application/` or `output/application_synthetic/`:

-   `tables/table1_population_characteristics.tex`: a descriptive table of the population, stratified by outcome.
-   `figures/fig1_missingness_patterns.pdf`: an UpSet-style plot of the joint distribution of missingness patterns across the four covariates (age, altered mental status, hypoxemia, coagulation disorder), produced with `naniar::gg_miss_upset`.
-   `tables/table2_performance_of_evaluated_procedures.tex`: a table comparing the predictive performance (via leave-one-out) of pattern submodels, multiple imputation, and MI with missingness indicators.

The leave-one-out loop takes about 30 minutes on a laptop.

Because these outputs are generated directly by R (`ggplot2`/`naniar`/`kable`) rather than by the pgfplots pipeline, they are not part of `make -C latex/figures` and have no corresponding Makefile target.

## Validation analysis (`04_validation_analysis.R`)

This analysis is not reported in the article; it is part of the doctoral thesis of the first author and is kept for completeness.

This script evaluates, for each scenario and target missingness proportion, the consistency of two ways of pooling multiply-imputed validation data (risk pooling and predictions pooling) against complete-case validation and against the theoretical Bayes target risks, both when the outcome is excluded from the validation-time imputation model (ordinary branch) and when it is included (with-outcome / "withY" branch). Each branch is evaluated at both an idealised endpoint (oracle prediction function with an optimal, true-distribution imputation model) and a realistic endpoint (fitted prediction function with an estimated, mice-fitted imputation model).

Results are written to `output/validation_analysis/{raw,tables}`; as with `01`/`02`, the run is resumable. Figures are built separately by the Makefile (see below).

## Figures generation

All figures used in the article are generated automatically from the simulation output tables using LuaLaTeX and a Makefile.

LaTeX scripts for all figures are available in `latex/figures/`.

### Requirements

- LuaLaTeX (TeX Live ≥ 2022 or MiKTeX)
- GNU Make

#### Linux

- Both lualatex and make are usually available by default.

#### MacOS

- Install via Homebrew:

``` bash
brew install --cask mactex
brew install make
```

#### Windows

- Two common options:

  - Rtools (recommended for R users): https://cran.r-project.org/bin/windows/Rtools/
  - MiKTeX + Make via:
    - MSYS2, or
    - Git Bash, or
    - Windows Subsystem for Linux (WSL)

- After installation, make sure both lualatex and make are available in your terminal.

### Generating all figures

- From the project root:

``` bash
cd latex/figures
make
```

- This command builds, into `output/main/figures/`, `output/secondary/figures/` and `output/validation_analysis/figures/`, four families of figures (17 PDFs in total):

  - `make main` -- 9 figures from the main analysis (`output/main/tables/`): one per combination of analysis group (overall / complete / incomplete) and metric (MSE / MSEP-OMU / MSEP-OMC). Each figure is a 5-panel group plot (one panel per scenario) showing all 6 estimation procedures plus the two (non-oracle) optimal references, MU and MC.
  - `make secondary` -- 3 figures from the secondary analysis (`output/secondary/tables/`, scenario 5 only): one per metric (MSE / MSEP-OMU / MSEP-OMC). Each figure has 3 panels (overall / complete / incomplete), each showing MLE and MI fitted on the full training set or on the observed-Y subset, plus the MU/MC references.
  - `make theoretical_risks` -- 1 figure (`output/validation_analysis/tables/`): a 5-panel group plot (one per scenario) of the six theoretical Bayes target risks (MU/MC × OP/CP/full).
  - `make validation_consistency` -- 4 figures (`output/validation_analysis/tables/`): one per combination of target family (MU / MC) and branch (ordinary / with-outcome). Each is a 5-panel group plot showing risk pooling and predictions pooling (each at the oracle and fitted/estimated endpoints), complete-case validation (fitted and oracle model), and the three theoretical target risks.

- `make clean` removes the auxiliary/log files (not the PDFs) from those three output directories.

### Notes

- Every figure is generated directly from the CSV tables written by the R scripts above (`output/main/tables/`, `output/secondary/tables/`, `output/validation_analysis/tables/`). Make sure they are generated (or `output.zip` is extracted in the root directory) before running `make`.
- No manual editing of figures is required.
- The `03_application.R` figure (`fig1_missingness_patterns.pdf`) is produced directly by that script, not by the Makefile.
- The figure generation process is fully reproducible and platform-independent.

## Code structure

``` bash
├── 00_main.R                          # orchestrates 01-04, then builds all figures
├── 01_simulation_main_analysis.R      # main simulation study
├── 02_simulation_secondary_analysis.R # secondary study (missing Y, scenario 5)
├── 03_application.R                   # real-data application
├── 04_validation_analysis.R           # RP/PP validation-consistency study
├── R/
│   ├── config/
│   │   └── config.R                   # simulation parameters
│   └── functions/
│       ├── application_functions.R    # helper functions for 03_application.R
│       ├── data_generation.R          # simulation of datasets
│       ├── latex_export.R             # writes the pgfplots-ready CSVs for the validation figures
│       ├── logging_utils.R            # progress logging helpers
│       ├── performance_metrics.R      # performance metrics and table export (01, 02)
│       ├── reference_probabilities.R  # computation of oracle reference probabilities
│       ├── SimulationsForMissingnessInPrediction_package.R # pseudo-package description for documentation
│       ├── training_procedures.R      # training procedures and prediction functions
│       ├── utils.R                    # input checkers
│       ├── validation_analysis.R      # RP/PP/CCV computation for 04_validation_analysis.R
│       └── validation_functions.R     # supporting functions for the validation analysis
├── man/                                # function documentation
├── data-raw/
│   └── make_synthetic_dataset_application.R # builds the synthetic application dataset
├── input/
│   └── synthetic_dataset_application.rda    # synthetic dataset used by 03_application.R
├── latex/
│   └── figures/
│       ├── Makefile                          # builds all figures with `make`
│       ├── figure_main.tex                   # main-analysis group figure (9 variants)
│       ├── figure_scenario_unique.tex         # one main-analysis scenario panel
│       ├── figure_secondary.tex              # secondary-analysis group figure (3 variants)
│       ├── figure_theoretical_risks.tex      # theoretical Bayes target risks figure
│       ├── figure_scenario_theoretical_risks.tex # one theoretical-risks scenario panel
│       ├── figure_validation_consistency.tex # validation-consistency group figure (4 variants)
│       └── figure_scenario_validation_consistency.tex # one validation-consistency scenario panel
├── CITATION.cff
├── DESCRIPTION
├── LICENSE
├── NEWS.md
└── README.md
```

-   `01_simulation_main_analysis.R` reproduces all primary simulation results
-   `02_simulation_secondary_analysis.R` evaluates the effect of missingness of $Y$ on the performance of MLE and MI under the MARXYO mechanism
-   `03_application.R` reproduces the real-data application (on the synthetic dataset if the clinical dataset is not available)
-   `04_validation_analysis.R` evaluates the consistency of pooled multiple-imputation validation against complete-case validation and the theoretical target risks
-   `R/config/config.R` defines the simulation design and parameters
-   Scripts in `R/functions/` contain modular functions

## Reproducibility

-   All simulation results are reproducible given a fixed random seed
-   Session information is recorded automatically

## Datasets

-   Simulated datasets and simulation outputs are available for [download](https://www.pierre-catoire.page/files/missingness-in-prediction-output.zip).
-   The synthetic application dataset is in `input/` (see above).

## Intended use

This repository is intended for:

-   methodological transparency,
-   reproducibility of published results,
-   and peer review.

It is not designed as a general-purpose software package.

## Citation

If you use this code, please cite the article and this repository (see `CITATION.cff`):

> Catoire P, Genuer R, Proust-Lima C. Prediction with Missing Data: Target Probabilities and Missingness Mechanisms (software code), version 2.0. 2026. https://github.com/pierre-catoire/missing-data-in-prediction

## License

The code and the synthetic dataset are openly available under MIT license (see `LICENSE` for details).

## Contact

Feel free reaching out at [pierre\@pierre-catoire.page](mailto:pierre@pierre-catoire.page){.email} for any request or suggestion!
