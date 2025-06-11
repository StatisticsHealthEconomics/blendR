
#' @title Create survival probabilities
#' @name make_surv_methods
#'
#' @description These function are version of the [survHE::make.surv()] function
#'    from \pkg{survHE}. These are needed prior to blending.
#'
#' @param Surv  survival analysis object
#' @param ... Additional arguments
#' @return matrix of survival probabilities
#' @export
#'
#' @examplesIf rlang::is_installed("survHEhmc")
#' library(survHE)
#'
#' ## trial data
#' data("TA174_FCR", package = "blendR")
#'
#' ## externally estimated data
#' data_sim <- ext_surv_sim(t_info = 144,
#'                          S_info = 0.05,
#'                          T_max = 180)
#'
#' ext_Surv <- fit.models(formula = Surv(time, event) ~ 1,
#'                        data = data_sim,
#'                        distr = "exponential",
#'                        method = "hmc")
#'
#' S_ext <- make_surv(ext_Surv, t = 1:100, nsim = 100)
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
#' @param t Time points; vector
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
      num.threads = Surv$.args$num.threads,
      n = nsim,
      result = Surv,
      selection = list(
        Predictor = -c(1:n_data),
        baseline.hazard = c(1:(nrow(Surv$summary.random$baseline.hazard))))
    )

  # matrix of baseline hazards for the intervals
  h0 <-
    lapply(joint_post, function(x) x$latent) |>
    unlist() |>
    matrix(nrow = nsim, byrow = TRUE) |>
    `colnames<-`(rownames(joint_post[[1]]$latent)) |>
    as_tibble() |>
    select(contains("baseline")) |>
    exp()

  # intervals for the hazards
  interval.t <- Surv$summary.random$baseline.hazard$ID
  interval_width <- interval.t[2]

  # matrix of cumulative hazards for the intervals
  H0 <- apply(h0, 1, cumsum)*interval_width

  # calculate survival probabilities for the intervals by default
  if (is.null(t)) t <- interval.t else t <- t

  # find the intervals for elements of vector t
  t_int <- findInterval(t, interval.t)

  # cumulative hazard for the specific time vector t
  H.t <- matrix(NA_real_, nrow = length(t), ncol = nsim)

  for (i in seq_along(t)) {
    if (t_int[i] > 1){
      H.t[i, ] <-
        H0[t_int[i] - 1, ] +
        unlist(h0[, t_int[i]] * (t[i] - interval.t[t_int[i]]))
    } else if (t_int[i] == 1) {
      H.t[i, ] <-
        unlist(h0[, t_int[i]] * (t[i] - interval.t[t_int[i]]))
    } else {
      H.t[i, ] <- 0
    }
  }

  # transform to survival probabilities
  S.t <- t(exp(-t(H.t)))

  S.t
}

#' @rdname make_surv_methods
#'
#' @export
make_surv.default <- function(Surv,
                              t = NULL,
                              nsim = 100, ...) {
  if (is.null(dim(Surv))) {
    if (any(is.null(t))) t <- 0:(length(Surv) - 1)

    return(matrix(rep(Surv[t + 1], nsim), ncol = nsim))
  }

  if (any(is.null(t))) t <- 0:(nrow(Surv) - 1)

  Surv[t + 1, ]
}
