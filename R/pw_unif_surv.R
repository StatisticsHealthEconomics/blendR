
#' Piecewise uniform survival curve
#'
#' @param p Given survival probability constraints
#' @param times Times at which \code{p} are defined
#' @param epsilon Length between generated times points
#'
#' @return Data frame of time points \code{t} and
#'         survival probabilities \code{S}
#' @seealso \code{\link{ext_surv_sim}}
#' @export
#'
#' @examples
#' p <- c(0.8, 0.3, 0.2, 0)
#' times <- c(10, 20, 30, 40)
#' surv <- pw_unif_surv(p, times)
#' plot(surv, type = "l")
#'
#' # sample times
#' x <- runif(100)
#' purrr::map_dbl(x, ~max(surv$t[surv$S > .x]))
#'
pw_unif_surv <- function(p, times, epsilon = 0.1) {

  ts <- seq(0, max(times), epsilon)
  nt <- length(times)

  tgrps <- cut(ts, c(0,times), include.lowest = TRUE)
  counts <- table(tgrps)

  p1 <- rep(c(1, p[1:(nt-1)]), counts)
  t1 <- rep(c(0, times[1:(nt-1)]), counts)
  p2 <- rep(p, counts)
  t2 <- rep(times, counts)

  data.frame(
    t = ts,
    S = (p1 - p2)*(1 - (ts - t1)/(t2 - t1)) + p2)
}


#' Piece-wise uniform survival curve for uncertain inputs
#'
#' @param p Probability
#' @param times Event times
#' @param sn Scaling factor. This is like a pseudosample size.
#' @param epsilon Different in times
#' @param n Number of samples
#' @export
#'
#' @examples
#' surv <- sample_pw_unif_surv(p = c(0.8, 0.3, 0.2, 0),
#'                            times = c(10, 20, 30, 40), n = 100)
#' matplot(surv, type = "l")
#' surv <- sample_pw_unif_surv(p = c(0.8, 0.3, 0.2, 0),
#'                            times = c(10, 20, 30, 40), n = 100, sn = 100)
#' matplot(surv, type = "l")
#'
sample_pw_unif_surv <- function(p, times,
                                n = 2, sn = 10,
                                epsilon = 0.1) {
  delta_p <- diff(rev(c(1,p)))
  rprobs <- igraph::sample_dirichlet(n, delta_p*sn)
  rSurv <- as.data.frame(1 - apply(rprobs, 2, cumsum))

  purrr::map_dfc(rSurv, ~pw_unif_surv(p = .x, times, epsilon)$S) |>
    as.matrix()
}
