#
# pw_unif_surv <- function(p, times) {
#
#   nt <- length(times)
#   times <- c(0, times)
#   p <- c(1, p)
#
#   S <- NULL
#
#   for (i in seq_len(nt)) {
#     t1 <- times[i]
#     t2 <- times[i+1]
#     p1 <- p[i]
#     p2 <- p[i+1]
#     x <- t1:(t2-1)
#
#     S <- c(S, (p1 - p2)*(1 - (x - t1)/(t2 - t1)) + p2)
#   }
#
#   S
# }

p <- c(0.8, 0.3, 0.2, 0)
times <- c(10, 20, 30, 40)
surv <- pw_unif_surv(p, times)
plot(surv, type = "l")

# sample times
x <- runif(100)
purrr::map_dbl(x, ~max(surv$t[surv$S > .x]))
#
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




