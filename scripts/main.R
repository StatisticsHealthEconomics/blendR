
# Example script for blendR

library(survHE)
library(INLA)

set.seed(1996)

## load data
data("TA174_FCR", package = "blendR")
head(dat_FCR)


## observed estimate
obs_Surv <- surv_est_inla(data = dat_FCR,
                          cutpoints = seq(0, 180, by = 5))

## external estimate
data_sim <- ext_surv_sim(t_info = 144,
                         S_info = 0.05,
                         T_max = 180)

ext_Surv <- fit.models(formula = Surv(time, event) ~ 1,
                       data = data_sim,
                       distr = "gompertz",
                       method = "hmc",
                       priors = list(gom = list(a_alpha = 0.1,
                                                b_alpha = 0.1)))

blend_interv <- list(min = 48, max = 150)
beta_params <- list(alpha = 3, beta = 3)

ble_Surv <- blendsurv(obs_Surv, ext_Surv, blend_interv, beta_params)

plot(ble_Surv)


####################################
# two HMC survHE fits

obs_Surv2 <- fit.models(formula = Surv(death_t, death) ~ 1,
                        data = dat_FCR,
                        distr = "exponential",
                        method = "hmc")

ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                       data = data_sim,
                       distr = "exponential",
                       method = "hmc")

ble_Surv2 <- blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params)

# kaplan-meier
km <- survfit(Surv(death_t, death) ~ 1, data = dat_FCR)

plot(ble_Surv2) +
  geom_line(aes(km$time, km$surv, colour = "Kaplan-Meier"),
            size = 1.25, linetype = "dashed")


#######################################
# flexsurv frequentist background fit

obs_Surv3 <- fit.models(formula = Surv(death_t, death) ~ 1,
                        data = dat_FCR,
                        distr = "exponential",
                        method = "hmc")

ext_Surv3 <- flexsurv::flexsurvreg(formula = Surv(time, event) ~ 1,
                       data = data_sim,
                       dist = "gompertz")

ble_Surv3 <- blendsurv(obs_Surv3, ext_Surv3, blend_interv, beta_params)

plot(ble_Surv3)

