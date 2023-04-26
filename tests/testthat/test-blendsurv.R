
if (!require("survHEhmc")) remotes::install_github('giabaio/survHEhmc')

library(survHE)
library(survHEhmc)

data("TA174_FCR", package = "blendR")


test_that("different distributions in survHE hmc", {

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

  suppressWarnings(
    expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")
  )

  # gompertz

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "gompertz",
                          method = "hmc")

  suppressWarnings(
    expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")
  )

  # log normal

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "lognormal",
                          method = "hmc")

  suppressWarnings(
    expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")
  )

  # gamma

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "gamma",
                          method = "hmc")

  suppressWarnings(
    expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")
  )

  # log logistic

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "loglogistic",
                          method = "hmc")

  suppressWarnings(
    expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params), "list")
  )
})


test_that("user-supplied time points for survival distribution", {

  data_sim <- ext_surv_sim(t_info = 144,
                           S_info = 0.05,
                           T_max = 180)

  obs_Surv2 <- fit.models(formula = Surv(death_t, death) ~ 1,
                          data = dat_FCR,
                          distr = "exponential",
                          method = "hmc")

  blend_interv <- list(min = 48, max = 150)
  beta_params <- list(alpha = 3, beta = 3)

  ext_Surv2 <- fit.models(formula = Surv(time, event) ~ 1,
                          data = data_sim,
                          distr = "exponential",
                          method = "hmc")

  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params, times = 0:100), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params, times = -100:100), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params, times = seq(0, 100, by = 0.5)), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params, times = 0:300), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv2, blend_interv, beta_params, times = seq(0, 300, by = 0.5)), "list")

  ## inla

  obs_Surv_inla <- fit_inla_pw(data = dat_FCR,
                               cutpoints = seq(0, 180, by = 5))

  expect_type(blendsurv(obs_Surv_inla, ext_Surv2, blend_interv, beta_params, times = 0:100), "list")
  expect_type(blendsurv(obs_Surv_inla, ext_Surv2, blend_interv, beta_params, times = seq(0, 100, by = 0.5)), "list")
  expect_type(blendsurv(obs_Surv_inla, ext_Surv2, blend_interv, beta_params, times = 0:300), "list")
  expect_type(blendsurv(obs_Surv_inla, ext_Surv2, blend_interv, beta_params, times = seq(0, 300, by = 0.5)), "list")

  # # error
  # xx <- blendsurv(obs_Surv_inla, ext_Surv2, blend_interv, beta_params, times = -100:100)


  # flexsurv

  ext_Surv_flex <- flexsurv::flexsurvreg(formula = Surv(time, event) ~ 1,
                                         data = data_sim,
                                         dist = "gompertz")

  expect_type(blendsurv(obs_Surv2, ext_Surv_flex, blend_interv, beta_params, times = 0:100), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv_flex, blend_interv, beta_params, times = -100:100), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv_flex, blend_interv, beta_params, times = seq(0, 100, by = 0.5)), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv_flex, blend_interv, beta_params, times = 0:300), "list")
  expect_type(blendsurv(obs_Surv2, ext_Surv_flex, blend_interv, beta_params, times = seq(0, 300, by = 0.5)), "list")

})

