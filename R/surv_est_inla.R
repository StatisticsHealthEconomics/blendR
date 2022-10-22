
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
                          nsim = 100){

  # Convert a Cox proportional hazard model into Poisson regression
  p <- INLA::inla.coxph(
    inla.formula,
    data = data,
    control.hazard = list(
      constr = FALSE,
      cutpoints = cutpoints,
      model = "rw1"))

  # Fit the model
  m <- INLA::inla(
    p$formula,
    family = p$family,
    data = c(as.list(p$data), p$data.list),
    E = p$E,
    control.compute = list(config = TRUE, dic = TRUE))

  # Draw samples from the joint posterior distribution
  jp <-
    INLA::inla.posterior.sample(
      n=nsim,
      m,
      selection=list(
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
                   select(.,1:length(interval.t)),1,cumsum))*interval.t[2]

    tt <- interval.t+interval.t[2]

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

  S_obs <- rbind(rep(1, nsim), S0[-nrow(S0), ])

  # Kaplan-Meier estimate
  km <- survfit(Surv(death_t, death) ~ 1, data = data)

  list(
    time = tt-tt[1],
    S_obs = S_obs,
    KM = km)
}

