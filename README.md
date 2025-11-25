# German Politics Projections

This repository contains a small R project that uses Monte Carlo simulation to project short-term popularity trends for six major German political parties. The work was created as part of the "Modeling and simulations" course at the Faculty of Informatics in Pula.

## Table of Contents
- [Overview](#overview)
- [Usage](#usage)
- [Output and results](#output-and-results)
- [Licence](#license)
- [Contributing](#contributing)
- [Author & Contact](#author--contact)

## Overview

**Key idea:** fit probability distributions to historical changes in party popularity, then run many simulated trajectories to estimate likely short-term outcomes (means, ranges, and plots).

**Parties analysed:** CDU, SPD, Greens, AfD, FDP, The Left

**What you'll find in this repo**
- `German politics projections.R` — the primary R script that loads data, fits distributions (using `fitdistrplus`), and runs Monte Carlo simulations for each party.
- `njem_podaci.csv` — the dataset used for historical popularity and auxiliary variables (immigration, inflation, temperature, GDP).
- `LICENSE` — project license.

## Usage

### Requirements
- R (recommended: R 4.0+)
- Packages: `readr`, `fitdistrplus` (install with `install.packages()` if missing)

Example to install dependencies in R:

```r
install.packages(c("readr", "fitdistrplus"))
```

### How to run

Option 1 — interactive R / RStudio
1. Open `German politics projections.R` in RStudio.
2. Ensure the working directory contains `njem_podaci.csv` (or update the path in the script).
3. Source the script or run it line-by-line to view plots and summary statistics:

```r
source("German politics projections.R")
```

Option 2 — command line with `Rscript`

From the repository root in PowerShell:

```powershell
# set working directory and run
Set-Location -Path "{path}\german_politics_projections"
Rscript "German politics projections.R"
```

## Output and results
- The script produces time-series plots of historical popularity for each party and Monte Carlo simulation plots projecting the next three months (by default) with average simulated trajectories and thin lines for individual simulations.
- It also prints simple summary statistics (mean, sd, min, max) for each simulation month.

### Design notes and caveats
- The script chooses different parametric distributions for different parties (Weibull, uniform, gamma, etc.) based on fitted log-likelihoods.
- The GDP column in the provided dataset is removed in the script because the original author flagged it as flawed; the script uses a separate `bdp` data.frame for illustrative GDP values.
- The current simulation horizon is short (3 months) and uses simple univariate change models. The results should be interpreted as illustrations of how stochastic modelling can be used for short-term projections, not as definitive forecasts.

## License
See the `LICENSE` file in this repository for license details.

## Contributing
- Feel free to contribute if you wish. Open an issue or submit a pull request with a clear description of the change.

## Author & Contact
- Author: Sebastijan Dominis
- Contact: sebastijan.dominis99@gmail.com
