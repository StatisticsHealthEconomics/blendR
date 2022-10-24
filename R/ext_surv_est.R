
#' Create an external survival curve based on the expert opinion
#'
#' @param t_info  A vector of times for which expert opinion is elicited
#' @param S_info  A vector of mean survival probabilities estimated by experts corresponding to timepoints in the t_pri
#' @param T_max   The maximum survival time to be used
#' @param n       The number of patients to construct the artificial external dataset; default 70
#'
#' @return
ext_surv_sim <- function(t_info, S_info, T_max){
  set.seed(1996)

  # Partition the time horizon into intervals
  S <- c(1, S_info, 0)
  t <- c(0, t_info, T_max)

  c <- length(S) - 1
  d <- length(t)

  n_par <- vector(mode = "numeric", length = c)

  for (i in seq_len(c)) {
    n_par[i] <- round(n*(S[i] - S[i+1]), digits = 0)
  }

  n_sim <- sum(n_par)

  min_unif <- rep(t[-d], n_par)
  max_unif <- rep(t[-1], n_par)

  # Create survival times using uniform distribution
  time   <- runif(n_sim, min_unif, max_unif)

  status <- rep(1, n_sim)

  data.frame(time = time,
             event = status)
}
