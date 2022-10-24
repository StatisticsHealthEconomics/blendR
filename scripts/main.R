
# Example script for blendR


if (.Platform$OS.type == "windows") {
  if ("survHE" %in% rownames(installed.packages()) == FALSE) {
    install.packages("survHE",
                     repos = c("http://www.statistica.it/gianluca/R",
                               "https://cran.rstudio.org",
                               "https://inla.r-inla-download.org/R/stable"),
                     dependencies = TRUE)
  }
}

if (.Platform$OS.type == "windows") {
  if ("INLA" %in% rownames(installed.packages()) == FALSE) {

    if (!require("BiocManager", quietly = TRUE))
      install.packages("BiocManager")

    BiocManager::install("graph")
    BiocManager::install("Rgraphviz")

    # requires the latest version of R
    install.packages("INLA",
                     repos = c(getOption("repos"),
                               INLA = "https://inla.r-inla-download.org/R/stable"),
                     dep = TRUE)
  }
}


library(survHE)
library(INLA)

## load data
data("TA174_FCR", package = "blendR")
head(dat_FCR)


## observed estimate
obs_Surv <- surv_est_inla(data = dat_FCR,
                          cutpoints = seq(0, 180, by = 5))

## external estimate
ext_Surv <- ext_surv_est(t_info = 144, S_info = 0.05,
                         T_max = 180,
                         times_est = seq(0, 180),
                         distr = "gom")

blending_interval <- list(min = 48, max = 150)
beta_params <- list(alpha = 3, beta = 3)

ble_Surv <- blendsurv(obs_Surv, ext_Surv, blending_interval, beta_params)


plot(ble_Surv)


