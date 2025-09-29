
#' Fit a Piecewise Exponential Survival Model using INLA
#'
#' @description
#' A convenience wrapper to fit a Bayesian piecewise exponential model (PEM) for
#' survival data using the `INLA` package. This function simplifies the process by
#' automatically handling the necessary data transformation from a standard
#' survival format to the Poisson regression format that `INLA` requires.
#'
#' @details
#' This function models the baseline hazard rate as a piecewise constant function.
#' The `cutpoints` define the time intervals within which the hazard is assumed
#' to be constant.
#'
#' Internally, the function first uses `INLA::inla.coxph` to convert the survival
#' data into the long format suitable for a Poisson generalized linear model (GLM).
#' It then fits this model using `INLA::inla`, applying a random walk of order 1
#' (`rw1`) prior to the log-hazards for each interval. This prior provides a
#' flexible and smoothed estimate of the baseline hazard over time.
#'
#' @param inla.formula A formula specifying the survival model. The response must
#'   be an `INLA::inla.surv` object (e.g., `inla.surv(time, event)`). The right-hand
#'   side specifies the linear predictor. The default fits a baseline-hazard-only model.
#' @param data A data frame containing the time-to-event data, including the
#'   variables named in the `inla.formula`.
#' @param cutpoints A numeric vector of cut points used to partition the time
#'   axis into intervals for the piecewise constant hazard.
#' @param ... Additional arguments to be passed directly to the `INLA::inla`
#'   function (e.g., `control.predictor`, `control.family`).
#'
#' @return An object of class `inla`, which contains the full results of the
#'   fitted Bayesian model.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # INLA may require configuration on your system.
#' # See: https://www.r-inla.org/download-install
#'  if (requireNamespace("INLA", quietly = TRUE)) {
#'   data("TA174_FCR", package = "blendR")
#'   head(dat_FCR)
#'
#'   # Fit a simple piecewise model with intervals every 5 time units
#'   obs_Surv <- fit_inla_pw(data = dat_FCR, cutpoints = seq(0, 180, by = 5))
#'
#'   # summary(obs_Surv)
#'  }
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
      "install.packages('INLA', repos = c(getOption('repos'),
      INLA = 'https://inla.r-inla-download.org/R/stable'), dep = TRUE)"
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

