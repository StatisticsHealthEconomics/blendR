
#' Generate survival estimates with a piecewise exponential Cox model (using INLA)
#'
#' @param inla.formula The formula for PEM which must be an `inla.surv` object
#' @param data A dataframe for survival data with time (`death_t`) and
#'    event (`death`)
#' @param cutpoints A sequence of cut points for intervals in the baseline hazard
#' @param nsim The number of simulations from posteriors; default 100
#' @param ... Additional arguments
#'
#' @return INLA object
#' @export
#'
#' @examples
#' \dontrun{
#' data("TA174_FCR", package = "blendR")
#' head(dat_FCR)
#'
#' obs_Surv <- fit_inla_pw(data = dat_FCR, cutpoints = seq(0, 180, by = 5))
#' }
#'
fit_inla_pw <- function(inla.formula = inla.surv(death_t, death) ~ -1,
                        data,
                        cutpoints,
                        nsim = 100, ...) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "The 'INLA' package is required to use this function. ",
      "Please install it from its repository by running: ",
      "install.packages('INLA', repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/stable'), dep = TRUE)"
    )
  }

  # Convert a Cox proportional hazard model into Poisson regression
  p <- INLA::inla.coxph(
    inla.formula,
    data = data,
    control.hazard = list(
      constr = FALSE,
      cutpoints = cutpoints,
      model = "rw1"))

  # Fit model
  INLA::inla(
    p$formula,
    family = p$family,
    data = c(as.list(p$data), p$data.list),
    E = p$E,
    control.compute = list(config = TRUE, dic = TRUE), ...)
}

