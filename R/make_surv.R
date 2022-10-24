
#
make_surv <- function(Surv, ...)
  UseMethod("blendsurv", Surv)


#
make_surv.survHE <- function(Surv, t, nsim) {
  extr <- survHE::make.surv(Surv, t = t, nsim = nsim)
  as.matrix(extr$mat[[1]])[, -1]
}


#
make_surv.inla <- function(p, m, nsim) {

  # Draw samples from the joint posterior distribution
  jp <-
    INLA::inla.posterior.sample(
      n = nsim,
      m,
      selection = list(
        Predictor = -c(1:nrow(p$data)),
        baseline.hazard = c(1:(nrow(m$summary.random$baseline.hazard))))
    )

  sim <-
    lapply(jp, function(i) i$latent) |>
    unlist() |>
    matrix(nrow = nsim,byrow = TRUE) |>
    as_tibble(.name_repair = ~vctrs::vec_as_names(rownames(jp[[1]]$latent), quiet = TRUE))

  interval.t <- m$summary.random$baseline.hazard$ID

  # Transform baseline hazard
  logh0 <- select(sim, contains("baseline"))

  if (interval.t[2] < 1) {
    H0 <- (apply(exp(logh0) |>
                   select(., 1:length(interval.t)), 1, cumsum))*interval.t[2]

    tt <- interval.t + interval.t[2]
  } else {
    h0 <-
      exp(logh0) |>
      select(1:length(interval.t))

    h0_long <- h0[, rep(1:(length(interval.t) - 1), each = interval.t[2])]
    h0_long <- cbind(h0_long, h0[, length(interval.t)])
    H0 <- apply(h0_long, 1, cumsum)

    tt <- 1:(max(interval.t) + 1)
  }

  # Transform survival probabilities
  S0 <- t(exp(-t(H0)))

  row.names(S0) <- NULL

  rbind(rep(1, nsim), S0[-nrow(S0), ])
}

