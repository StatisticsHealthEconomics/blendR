
#' @export
make_surv <- function(Surv, ...)
  UseMethod("make_surv", Surv)


#' @importFrom survHE make.surv
#' @export
#'
make_surv.survHE <- function(Surv, t, nsim = 100) {
  extr <- survHE::make.surv(Surv, t = t, nsim = nsim)
  as.matrix(extr$mat[[1]])[, -1]
}


#' @importFrom survHE make.surv
#' @export
#'
make_surv.flexsurvreg <- function(Surv, t, nsim = 100) {

  # sample parameters
  sim <- flexsurv::normboot.flexsurvreg(Surv, B = nsim)

  distn_fn <- paste0("p", Surv$dlist$name)

  apply(sim, 1, function(x) {
    1 - do.call(distn_fn, args = c(list(q = t), as.list(x)))
  })
}


#' @importFrom INLA inla.posterior.sample
#' @export
#'
make_surv.inla <- function(Surv, t, nsim = 100) {

  n_data <- Surv$model.matrix@Dim[1]

  # draw samples from the joint posterior distribution
  joint_post <-
    INLA::inla.posterior.sample(
      n = nsim,
      result = Surv,
      selection = list(
        Predictor = -c(1:n_data),
        baseline.hazard = c(1:(nrow(Surv$summary.random$baseline.hazard))))
    )

  sim <-
    lapply(joint_post, function(x) x$latent) |>
    unlist() |>
    matrix(nrow = nsim, byrow = TRUE) |>
    as_tibble(.name_repair =
                ~vctrs::vec_as_names(rownames(joint_post[[1]]$latent),
                                     quiet = TRUE))

  interval.t <- Surv$summary.random$baseline.hazard$ID
  interval_width <- interval.t[2]

  # transform baseline hazard
  logh0 <- select(sim, contains("baseline"))

  if (interval_width < 1) {
    H0 <- (apply(exp(logh0) |>
                   select(., 1:length(interval.t)), 1, cumsum))*interval_width

    tt <- interval.t + interval_width
  } else {
    h0 <-
      exp(logh0) |>
      select(1:length(interval.t))

    h0_long <- h0[, rep(1:(length(interval.t) - 1), each = interval_width)]
    h0_long <- cbind(h0_long, h0[, length(interval.t)])
    H0 <- apply(h0_long, 1, cumsum)

    tt <- 1:(max(interval.t) + 1)
  }

  # transform survival probabilities
  S0 <- t(exp(-t(H0)))
  row.names(S0) <- NULL

  rbind(rep(1, nsim), S0[-nrow(S0), ])
}
