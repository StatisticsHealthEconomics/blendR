
#' Blended survival object
#'
#' This is the main function in the \pkg{blendR} package.
#' Two survival curves are supplied and blended according to
#' the blending distribution characterised by the blending interval and
#' the beta distribution parameters.
#'
#' @param obs_Surv,ext_Surv Observed and external data survival curves.
#'    These can come from \pkg{survHE}, \pkg{INLA} or \pkg{flexsurv} fits.
#' @param blend_interv Maximum and minimum values for the blending interval.
#' @param beta_params coefficients of a beta distribution
#' @param times_est A vector of times for which the survival curves
#'                  are to be computed
#' @param nsim The number of simulations from the distribution of
#'             the survival curves; default 100
#'
#' @return List of S for observed, external and blended curves.
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

  # parameters for the weight function
  wt_par <- list(a = blend_interv$min,
                 b = blend_interv$max,
                 shape1 = beta_params$alpha,
                 shape2 = beta_params$beta)

  w <- with(wt_par,
            pbeta((times_est - a)/(b - a), shape1, shape2))

  # blended estimate
  mat <- matrix(NA, nrow = tmax, ncol = nsim)

  for (i in seq_len(nsim)) {
    mat[, i] <- S_obs[, i]^(1 - w) * S_ext[, i]^w
  }

  ## based on survHE::make.surv()
  if (nsim == 1) {
    # If nsim=1 then only save the point estimates of the survival curve
    S <- data.frame(S = rowMeans(mat))
  } else {
    # If nsim > 1 then also give the lower and upper quartile of the
    # underlying distribution
    S <- data.frame(S = rowMeans(mat),
                    low = apply(mat, 1, quantile, 0.025),
                    upp = apply(mat, 1, quantile, 0.975))
  }

  res <-
    list(S = S,
         sim = NA,
         nsim = nsim,
         mat = mat,
         des.mat = NA,
         times = times_est,
         S_ext = S_ext,     ##TODO: mat_ext etc?
         S_obs = S_obs)

  structure(res, class = c("blended", class(res)))
}

