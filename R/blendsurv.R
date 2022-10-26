
#' Blend survival object
#'
#' @param obs_Surv Observed data survival curve
#' @param ext_Surv External survival curve
#' @param blend_interv Maximum and minimum values
#' @param beta_params coefficients of a beta distribution
#' @param tp      A vector of times for which the survival curves are to be computed
#' @param nsim    The number of simulations from the distribution of the survival curves; default 100
#'
#' @return
#' @export
#'
blendsurv <- function(obs_Surv, ext_Surv,
                      blend_interv,
                      beta_params = list(alpha = 3, beta = 3),
                      times_est = seq(0, 180),
                      nsim = 100) {

  tmax <- length(times_est)

  # external survival curve
  S_ext <- make_surv(ext_Surv, t = times_est, nsim = nsim)

  # observed data survival curve
  S_obs <- make_surv(obs_Surv, t = times_est, nsim = nsim)

  ## parameters for the weight function
  wt_par <- list(a = blend_interv$min,
                 b = blend_interv$max,
                 shape1 = beta_params$alpha,
                 shape2 = beta_params$beta)

  weight <- with(wt_par,
                 pbeta((times_est - a)/(b - a), shape1, shape2))

  ## blended estimate
  n_sim <- ncol(S_ext)
  S_ble <- matrix(NA, nrow = tmax, ncol = n_sim)

  for (i in seq_len(n_sim)) {
    S_ble[, i] <-
      S_obs[, i]^(1 - weight) * S_ext[, i]^weight
  }

  structure(
    list(S_obs = S_obs,
         S_ext = S_ext,
         S_ble = S_ble),
    class = "blended")
}

