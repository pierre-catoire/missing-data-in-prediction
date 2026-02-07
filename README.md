---
title: Simulation study: prediction under missing data mechanisms
author: Pierre Catoire
version: 1.0
date: 07-02-2026
---

This repository contains the complete simulation code used in the study “Prediction with missing information”.

The objective of the simulation study is to compare predictive performance of several estimation strategies under different missing data mechanisms, including mechanisms that violate classical Missing At Random (MAR) assumptions.

> [!IMPORTANT] Documentation is available for all implemented functions, using `?function`. To enable it, install `devtools` package if not done and run:

``` r
devtools::load_all()
```

## Overview of the simulation design

Each simulation run proceeds as follows:

1.  Data generation

-   Covariates $X_1$ and $X_2$ are generated from independent Gaussian distributions
-   The outcome $Y$ is generated from a linear Gaussian model conditional on $(X_1, X_2)$
-   Missingness in $X_1$ is generated according to a logistic model, with missingness indicator $M_{X_1}$, that may depend on $X_1$, $X_2$ and/or $Y$, depending on the scenarios.
-   $Y$ is considered always observed in the main analysis. A secondary analysis on scenario 5 assess the effect of missing $Y$, the mechanism of which being independent from $X_1$, $X_2$, $Y$ and $M_{X_1}$.
-   Missingness indicators are generated alongside the predictors and outcome. The variable values are masked separately

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
> As it requires Monte Carlo approximation, computation of optimal predictors (MU, MC, OMU, OMC) are computationally heavy, despite parallelisation. If slow, consider reduce the `monte_carlo_size` parameter in `config.R`.

6.  Performance evaluation

Metrics of performance are:

-   Mean Squared Error (MSE): $\frac{1}{n} \underset{i=1}{\overset{N}{\sum}} \left(Y - \hat{\mathbb{E}}\left[Y \mid \mathcal{E} \right]\right)^2$
-   Mean Squared Error of Prediction (MSEP):
-   with Oracle MU reference (MSEP-OMU): $\frac{1}{n} \underset{i=1}{\overset{N}{\sum}} \left(\Pr(Y \mid X_o, X_m) - \hat{\mathbb{E}}\left[Y \mid \mathcal{E} \right]\right)^2$
-   with Oracle MU reference (MSEP-OMC): $\frac{1}{n} \underset{i=1}{\overset{N}{\sum}} \left(\Pr(Y \mid X_o, X_m, M_X) - \hat{\mathbb{E}}\left[Y \mid \mathcal{E} \right]\right)^2$

*With* $\mathcal{E}$ the evidence used by the prediction function: $X_o$ or \$X_o,M_X depending on the prediction function. See the article for details.

7.  Output

Output contains:

-   Raw performance results are stored as `simulation_object` lists, loadable as `.rds` files in `output/raw/`, containing training and testing datasets, predictions of each prediction function, reference probabilities, and metadata including the missingness scenario and the target and observed missingness proportions.
-   performance metrics of each method and reference, for any given scenario, performance metric and analysis group: all cases (overall), complete and incomplete cases. For each, a couple of table is produced:
    -   the points, corresponding to the observed performance (example: `M2_overall_mse_points.csv`)
    -   a smoothing LOESS curve for better visualisation in the plots (example: `M2_overall_mse_loess.csv`) LOESS-smoothed performance curves are computed for visualization.

## Code structure

```         
├── R/
│   ├── data_generation.R
│   ├── training_procedures.R
│   ├── performance_metrics.R
│   ├── reference_probabilities.R
│   ├── utils.R
│   └── missingnessMechanismSimulations-package.R
├── man/
├── config.R
├── main.R
├── secondary_analysis.R
├── DESCRIPTION
├── LICENSE
└── README.md
```

-   `main.R` reproduces all primary simulation results
-   `secondary_analysis.R` evaluates the effect of missingness of $Y$ on the performance of MLE and MI under MARX-YO mechanism
-   `config.R` defines the simulation design and parameters
-   Scripts in `R` contain modular functions

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
