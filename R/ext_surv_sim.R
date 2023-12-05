
#' Create an external survival data based on expert opinion
#'
#' Generally, the sampling is done is two steps
#' \deqn{
#'    p(T) = p(T | interval i) p(interval i)
#' }
#'
#' In particular
#' \eqn{T ~ U(x_{i}, x_{i+1})}
#' \eqn{i ~ multinomial(\hat{\pi})}
#'
#' @param t_info  A vector of times for which expert opinion is elicited
#' @param S_info  A vector of mean survival probabilities estimated by experts
#'                corresponding to time points in `t_info`
#' @param T_max   The maximum survival time to be used
#' @param n       The number of patients to construct the artificial external data set; default 70
#' @importFrom stats runif rmultinom
#' @return Dataframe of times and censoring status.
#' @export
#'
#' @examples
#' dat <- ext_surv_sim(t_info = c(10,20,50),
#'                     S_info = c(0.9, 0.8, 0.2),
#'                     T_max = 100, n = 100)
#' if (require(survival)) {
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
