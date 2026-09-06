# Prediction with missing information: simulation code

*Authors: Pierre Catoire, Cécile Proust-Lima, Robin Genuer* *date: 2026-02-07* *version: 1.0*

This repository contains the complete simulation code used in the study "Prediction with missing information".

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

| MCAR | MAR | MARX-YM | MARX-YO | NIMO | NICO | Scenario |
|:----:|:---:|:-------:|:-------:|:----:|:----:|:--------:|
|  ✔   |  ✔  |    ✔    |    ✔    |  ✔   |  ✔   |    1     |
|  ✖   |  ✔  |    ✔    |    ✔    |  ✔   |  ✔   |    2     |
|  ✖   |  ✖  |    ✖    |    ✖    |  ✖   |  ✔   |    3     |
|  ✖   |  ✖  |    ✖    |    ✖    |  ✖   |  ✖   |    4     |
|  ✖   |  ✖  |    ✖    |    ✔    |  ✖   |  ✖   |    5     |

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

The aim of the secondary analysis is to discuss the importance of subsetting the training dataset by excluding observations with missing outcome when the mechanism is MNAR, MARX-YO. Using the datasets already generated for scenario 5 by the main analysis:

-   missingness in $Y$ is introduced completely at random (independent of $X_1$, $X_2$, $Y$ and $M_{X_1}$), with a probability of 40%
-   MLE and MI methods are fitted on both the complete training set (with missing $Y$) and the subset with only observed $Y$
-   their performances are compared with the MU and MC references

Results are written to `output/secondary/tables/`.

## Application study (`03_application.R`)

The application study illustrates the methods on a real dataset (`input/dataset_application.rda`). It produces, directly (outside the LaTeX/Makefile pipeline):

-   `output/application/tables/table1_population_characteristics.tex`: a descriptive table of the population, stratified by outcome.
-   `output/application/figures/fig1_missingness_patterns.pdf`: an UpSet-style plot of the joint distribution of missingness patterns across the four covariates (age, altered mental status, hypoxemia, coagulation disorder), produced with `naniar::gg_miss_upset`.
-   `output/application/tables/table2_performance_of_evaluated_procedures.tex`: a table comparing the predictive performance (via leave-one-out) of pattern submodels, multiple imputation, and MI with missingness indicators.

Because these outputs are generated directly by R (`ggplot2`/`naniar`/`kable`) rather than by the pgfplots pipeline, they are not part of `make -C latex/figures` and have no corresponding Makefile target.

## Validation analysis (`04_validation_analysis.R`)

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
│       ├── plotting_functions.R       # quick diagnostic R plots (not the article's LaTeX figures)
│       ├── reference_probabilities.R  # computation of oracle reference probabilities
│       ├── SimulationsForMissingnessInPrediction_package.R # pseudo-package description for documentation
│       ├── training_procedures.R      # training procedures and prediction functions
│       ├── utils.R                    # input checkers
│       ├── validation_analysis.R      # RP/PP/CCV computation for 04_validation_analysis.R
│       └── validation_functions.R     # supporting functions for the validation analysis
├── man/                                # function documentation
├── input/
│   └── dataset_application.rda        # real dataset used by 03_application.R
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
├── DESCRIPTION
├── LICENSE
└── README.md
```

-   `01_simulation_main_analysis.R` reproduces all primary simulation results
-   `02_simulation_secondary_analysis.R` evaluates the effect of missingness of $Y$ on the performance of MLE and MI under the MARX-YO mechanism
-   `03_application.R` reproduces the real-data application
-   `04_validation_analysis.R` evaluates the consistency of pooled multiple-imputation validation against complete-case validation and the theoretical target risks
-   `R/config/config.R` defines the simulation design and parameters
-   Scripts in `R/functions/` contain modular functions

## Reproducibility

-   All simulation results are reproducible given a fixed random seed
-   Session information is recorded automatically

## Datasets

The generated output is available for [download](www.pierre-catoire.page/files/missingness-in-prediction-output.zip).

## Intended use

This repository is intended for:

-   methodological transparency,
-   reproducibility of published results,
-   and peer review.

It is not designed as a general-purpose software package.

## License

The following code and datasets are openly available under MIT license (see `LICENSE` for details).

## Contact

Feel free reaching out at [pierre\@pierre-catoire.page](mailto:pierre@pierre-catoire.page){.email} for any request or suggestion!
