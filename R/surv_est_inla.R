
#' Generate survival estimates with a piecewise Cox model (using INLA)
#'
#' @param inla.formula   The formula for PEM which must be a \code{inla.surv} object
#' @param dat            A dataframe for survival data with time (\code{death_t}) and event (\code{death})
#' @param cutpoints      A sequence of cut points for intervals in the baseline hazard
#' @param nsim           The number of simulations from posteriors; default 100
#'
#' @return
#'
surv_est_inla <- function(inla.formula = inla.surv(death_t, death) ~ -1,
                          data,
                          cutpoints,
                          nsim = 100) {

  # Convert a Cox proportional hazard model into Poisson regression
  p <- INLA::inla.coxph(
    inla.formula,
    data = data,
    control.hazard = list(
      constr = FALSE,
      cutpoints = cutpoints,
      model = "rw1"))

  # Fit the model
  INLA::inla(
    p$formula,
    family = p$family,
    data = c(as.list(p$data), p$data.list),
    E = p$E,
    control.compute = list(config = TRUE, dic = TRUE))
}

