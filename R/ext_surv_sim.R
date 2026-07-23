
#' Create an external survival data based on expert opinion
#'
#' @description
#' Generates an individual patient-level survival dataset from aggregate survival
#' probabilities that might be elicited from expert opinion.
#'
#' The function creates a synthetic dataset of event times for `n` patients,
#' consistent with a piecewise-constant hazard rate implied by the expert-provided
#' survival probabilities.
#'
#' @details
#' The simulation uses a two-step sampling process based on partitioning the time
#' horizon from `0` to `T_max`. First, the total number of patients `n` is
#' allocated to different time intervals `[t_i, t_{i+1}]`. The probability of an
#' event occurring in an interval is derived from the drop in the survival curve
#' over that interval. This allocation is done via a multinomial distribution:
#' \deqn{
#'     i \sim \text{Multinomial}(\pi)
#' }
#' where \eqn{\pi_i = S(t_{i-1}) - S(t_i)} is the probability of an event in interval `i`.
#'
#' Second, for the patients assigned to each interval, a specific event time is
#' simulated from a uniform distribution covering that interval:
#' \deqn{
#'     T | i \sim U(t_{i-1}, t_i)
#' }
#'
#' @param t_info A numeric vector of time points for which expert opinion is elicited.
#' @param S_info A numeric vector of mean survival probabilities estimated by experts
#'               corresponding to time points in `t_info`.
#' @param T_max  The maximum survival time for the simulation, at which the
#'   survival probability is assumed to be 0.
#' @param n      The total number of patients to construct the artificial external data set; default 100
#' @importFrom stats runif rmultinom
#'
#' @return A data frame with `n` rows and two columns:
#'   \itemize{
#'     \item **time**: The simulated event time for each patient.
#'     \item **event**: The event indicator, which is always `1` as this function does not simulate censoring.
#'   }
#'
#' @export
#'
#' @examples
#' dat <- ext_surv_sim(t_info = c(10,20,50),
#'                     S_info = c(0.9, 0.8, 0.2),
#'                     T_max = 100, n = 100)
#' if (require(survival)) {
#'     # Kaplan-Meier curve
#'     km_fit <- survfit(Surv(time, event) ~ 1, data = dat)
#'     plot(km_fit)
#' }
ext_surv_sim <- function(t_info, S_info, T_max, n = 100) {

  ## length(t_info) == length(S_info)?

  # Partition the time horizon into intervals
  S <- c(1, S_info, 0)
  t <- c(0, t_info, T_max)

  n_S <- length(S)
  n_t <- length(t)

  n_par <- vector(mode = "numeric", length = n_S - 1)

  S_delta <- rev(diff(rev(S)))

  n_par <-
    rmultinom(1, size = n, prob = S_delta) |>
    as.vector()

  n_sim <- sum(n_par)

  min_unif <- rep(t[-n_t], n_par)
  max_unif <- rep(t[-1], n_par)

  # Create survival times using uniform distribution
  time   <- runif(n_sim, min_unif, max_unif)
  status <- rep(1, n_sim)

  data.frame(time = time,
             event = status)
}
