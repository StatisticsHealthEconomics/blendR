
#' @title Create survival probabilities
#' @name make_surv_methods
#'
#' @description These function are version of the \code{make.surv} function
#'    from \pkg{survHE}. These are needed prior to blending.
#'
#' @param Surv  survival analysis object
#' @param ... Additional arguments
#' @return matrix of survival probabilities
#' @export
#'
make_surv <- function(Surv, ...)
  UseMethod("make_surv", Surv)


#' @rdname make_surv_methods
#' @param t Time
#' @param nsim Number of simulations
#' @importFrom survHE make.surv
#' @export
#'
make_surv.survHE <- function(Surv, t, nsim = 100, ...) {
  extr <- survHE::make.surv(Surv, t = t, nsim = nsim)
  as.matrix(extr$mat[[1]])[, -1]
}


#' @rdname make_surv_methods
#' @param t Time
#' @param nsim Number of simulations
#' @importFrom survHE make.surv
#' @importFrom flexsurv normboot.flexsurvreg
#' @export
#'
make_surv.flexsurvreg <- function(Surv, t = NULL, nsim = 100, ...) {

  if (is.null(t)) t <- sort(unique(Surv$data$Y[, "stop"]))

  # sample parameters
  sim <- flexsurv::normboot.flexsurvreg(Surv, B = nsim)

  distn_fn <- paste0("p", Surv$dlist$name)

  apply(sim, 1, function(x) {
    1 - do.call(distn_fn, args = c(list(q = t), as.list(x)))
  })
}


#' @rdname make_surv_methods
#' @param t Time points; integer vector
#' @param nsim Number of simulations; integer
#' @import sn
#' @importFrom INLA inla.posterior.sample
#' @importFrom tibble as_tibble
#' @importFrom dplyr select contains
#' @export
#'
make_surv.inla <- function(Surv, t = NULL, nsim = 100, ...) {

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

  # matrix of baseline hazards
  h0 <-
    lapply(joint_post, function(x) x$latent) |>
    unlist() |>
    matrix(nrow = nsim, byrow = TRUE) |>
    `colnames<-`(rownames(joint_post[[1]]$latent)) |>
    as_tibble() |>
    select(contains("baseline")) |>
    exp()

  interval.t <- Surv$summary.random$baseline.hazard$ID
  interval_width <- interval.t[2]
  n_intervals <- length(interval.t)

  # additional tail times
  t_excess <- max(0, max(t) - max(interval.t))

  if (interval_width < 1) {
    H0 <- (apply(h0, 1, cumsum))*interval_width
  } else {
    h0_long <- h0[, rep(1:(n_intervals - 1), each = interval_width)]
    h0_long <- cbind(h0_long, h0[, rep(n_intervals, t_excess + 1)])
    H0 <- apply(h0_long, 1, cumsum)
  }

  # transform to survival probabilities
  S0 <- t(exp(-t(H0)))
  row.names(S0) <- NULL

  t_filter <- if (is.null(t)) TRUE else t + 1

  out <- rbind(rep(1, nsim), S0[-nrow(S0), ])

  out[t_filter, , drop = FALSE]
}

#' @rdname make_surv_methods
#'
make_surv.default <- function(Surv, t = 0:(length(Surv) - 1), ...) {
  Surv[t + 1]
}
