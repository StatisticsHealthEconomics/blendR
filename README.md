
# blendR: An R package for blending survival curves <img src='man/figures/hexbadge.png' style="float:right; height:200px;">

<!-- badges: start -->
[![R-CMD-check](https://github.com/StatisticsHealthEconomics/blendR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/StatisticsHealthEconomics/blendR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of blendR is to _blend_ two survival curves together from one to the other according to some defined blending function.
Originally developed for the following paper

> Che Z, Green N, Baio G. Blended Survival Curves: A New Approach to Extrapolation for Time-to-Event Outcomes from Clinical Trials in Health Technology Assessment. Med Decis Mak. 2022;43(3):299–310. 


## Installation

You can install the development version of blendR from [GitHub](https://github.com/) with:

```r
# install.packages("devtools")
devtools::install_github("StatisticsHealthEconomics/blendR")
```

## Example

This is a basic example which shows you how to solve a common problem. Using the _TA174_FCR_ data set contained in the `blendR` package, we fit exponential distribution survival models with no covariates with the `fit.models()` function from the `survHE` package. This employs the HMC sampler from Stan behind the scenes. The _external_ or _long-term_ data are obtained from an heuristic approach to simulating data consistent with user-defined constraints. The results are then blended into a single survival curve using the `blendsurv()` function.

```r
library(blendR)
library(survHE)

## trial data
data("TA174_FCR", package = "blendR")

## externally estimated data
data_sim <- ext_surv_sim(t_info = 144,
                         S_info = 0.05,
                         T_max = 180)
                         
obs_Surv <- fit.models(formula = Surv(death_t, death) ~ 1,
                        data = dat_FCR,
                        distr = "exponential",
                        method = "hmc")
                        
ext_Surv <- fit.models(formula = Surv(time, event) ~ 1,
                       data = data_sim,
                       distr = "exponential",
                       method = "hmc")
                       
blend_interv <- list(min = 48, max = 150)
beta_params <- list(alpha = 3, beta = 3)

ble_Surv <- blendsurv(obs_Surv, ext_Surv, blend_interv, beta_params)

plot(ble_Surv)
```
## GitHub Site

https://statisticshealtheconomics.github.io/blendR/

## Licence
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

## Contributing
Please submit contributions through `Pull Requests`, following the [contributing
guidelines](https://github.com/StatisticsHealthEconomics/blendR/blob/dev/CONTRIBUTING.md).
To report issues and/or seek support, please file a new ticket in the
[issue](https://github.com/StatisticsHealthEconomics/blendR/issues) tracker.

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/StatisticsHealthEconomics/blendR/blob/dev/CONDUCT.md). By participating in this project you agree to abide by its terms.
