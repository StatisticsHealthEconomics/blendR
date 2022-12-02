

library(survHE)

data("TA174_FCR", package = "blendR")

test_that("test different distributions", {

  data_sim <- ext_surv_sim(t_info = 144,
                           S_info = 0.05,
                           T_max = 180)

  obs_Surv2 <- fit.models(formula = Surv(death_t, death) ~ 1,
                          data = dat_FCR,
                          distr = "exponential",
                          method = "hmc")

  blend_interv <- list(min = 48, max = 150)
  beta_params <- list(alpha = 3, beta = 3)

  # exponential

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "exponential",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")

  # weibull

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "weibull",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")

  # gompertz

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "gompertz",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")

  # log normal

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "lognormal",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")

  # gamma

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "gamma",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")

  # log logistic

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "loglogistic",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")
})
