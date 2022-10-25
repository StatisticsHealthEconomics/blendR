
#' Create an external survival curve based on the expert opinion
#'
#' @param t_info  A vector of times for which expert opinion is elicited
#' @param S_info  A vector of mean survival probabilities estimated by experts corresponding to timepoints in the t_pri
#' @param T_max   The maximum survival time to be used
#' @param n       The number of patients to construct the artificial external dataset; default 70
#'
#' @return
#' @examples
#' dat <- ext_surv_sim(t_info = c(10,20,50),
#'                     S_info = c(0.9, 0.8, 0.2),
#'                     T_max = 100, n = 100)
#' km_trt_fit <- survfit(Surv(time, event) ~ 1, data = dat)
#' plot(km_trt_fit)
#'
ext_surv_sim <- function(t_info, S_info, T_max, n = 100) {
  set.seed(1996)

  ## length(t_info) == length(S_info)?

  # Partition the time horizon into intervals
  S <- c(1, S_info, 0)
  t <- c(0, t_info, T_max)

  n_S <- length(S)
  n_t <- length(t)

  n_par <- vector(mode = "numeric", length = n_S - 1)

  for (i in seq_len(n_S - 1)) {
    n_par[i] <- round(n*(S[i] - S[i+1]), digits = 0)
  }

  n_sim <- sum(n_par)

  min_unif <- rep(t[-n_t], n_par)
  max_unif <- rep(t[-1], n_par)

  # Create survival times using uniform distribution
  time   <- runif(n_sim, min_unif, max_unif)

  status <- rep(1, n_sim)

  data.frame(time = time,
             event = status)
}
