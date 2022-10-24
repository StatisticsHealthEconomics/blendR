
#' Blend survival object
#'
#' @param obs_Surv Observed survival curve
#' @param ext_Surv External survival curve
#' @param blending_interval Maximum and minimum values
#' @param beta_params coefficients of a beta distribution
#'
#' @return
#' @export
#'
blendsurv <- function(obs_Surv, ext_Surv,
                      blending_interval,
                      beta_params = list(alpha = 3, beta = 3)) {

  tp <- seq(0, 180)

  ## parameters for the weight function
  wt_par <- list(a = blending_interval$min, b = blending_interval$max,
                 shape1 = beta_params$alpha, shape2 = beta_params$beta)

  weight <- with(wt_par,
                 pbeta((tp - a)/(b - a), shape1, shape2))

  ## blended estimate
  n_sim <- ncol(ext_Surv$S_ext)

  ble_Surv <- matrix(NA, nrow = 180 + 1, ncol = n_sim)

  for (i in seq_len(n_sim)) {
    ble_Surv[, i] <-
      obs_Surv$S_obs[, i]^(1 - weight) * ext_Surv$S_ext[, i]^weight
  }

  structure(
    list(obs_Surv = obs_Surv,
         ext_Surv = ext_Surv,
         ble_Surv = ble_Surv),
      class = "blended")
}

