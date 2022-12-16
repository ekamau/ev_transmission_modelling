# Enterovirus transmission dynamics: England, UK
<br />
This repository contains the data and code used to produce the results presented in "Enterovirus A71 and Coxsackievirus A6 transmission dynamics in England, UK, 2006 – 2017: a mathematical modelling study using cross-sectional seroprevalence data" <br />

## Data
Data containing information on age, year of sample collection and antibody titer is provided in the `data` folder.<br />

## Reproducing our results
The recipes and workflow for producing our results are configured in a targets workflow contained within the `_targets.R` file. Each target is a step in the analysis. To rerun our results, install the `targets` package in R, and run `tar_make()`. Ensure other package dependencies listed in the `_targets.R` are installed in your computer.<br />
Alternatively, the R scripts in the `src/R` folder and the processed data in the `results` folder can be used to reproduce the results.<br />
