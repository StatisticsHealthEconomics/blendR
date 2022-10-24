
# Example script for blendR

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


