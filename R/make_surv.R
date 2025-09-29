
#' @title Create survival probabilities
#' @name make_surv_methods
#'
#' @description
#' A generic S3 function and methods to generate a standardized matrix of survival
#' probabilities from various fitted survival model objects.
#'
#' This function standardizes the output from different survival modelling packages
#' into a consistent format: a matrix where rows represent discrete time points
#' and columns represent simulations from the model's posterior distribution. This
#' standardized format is essential for use in downstream evidence blending
#' functions. The methods are inspired by the [survHE::make.surv()] function.
#'
#' @param Surv A fitted survival model object or a matrix/vector of survival
#'   probabilities. Supported classes include `survHE`, `flexsurvreg`, `inla`,
#'   `matrix`, and `numeric`.
#' @param ... Additional arguments passed to specific methods (e.g., `t`, `nsim`).
#' @return A matrix with `length(t)` rows and `nsim` columns. Each element `[i, j]`
#'   is the survival probability at time `t[i]` for simulation `j`.
#'
#' @seealso [survHE::make.surv()]
#'
#' @export
#'
#' @examples
#' # Define common time points and number of simulations for examples
#' time_points <- 1:100
#' n_sim <- 50
#'
#' #--------------------------------------
#' ## Method for a 'survHE' object
#' #--------------------------------------
#' if (rlang::is_installed("survHE") && rlang::is_installed("survival")) {
#'   library(survHE)
#'   library(survival)
#'   data(ovarian)
#'
#'   # Fit a Weibull model using survHE (with MLE for speed)
#'   fit_she <- fit.models(
#'     formula = Surv(futime, fustat) ~ 1,
#'     data = ovarian,
#'     distr = "weibull",
#'     method = "mle"
#'   )
#'
#'   # Generate survival probability matrix
#'   surv_matrix_she <- make_surv(fit_she, t = time_points, nsim = n_sim)
#'   cat("survHE method output dimensions:", dim(surv_matrix_she), "\n")
#' }
#'
#' #--------------------------------------
#' ## Method for a 'flexsurvreg' object
#' #--------------------------------------
#' if (rlang::is_installed("flexsurv") && rlang::is_installed("survival")) {
#'   library(flexsurv)
#'   library(survival)
#'
#'   # Fit a log-logistic model using flexsurv
#'   fit_fsr <- flexsurvreg(
#'     formula = Surv(futime, fustat) ~ 1,
#'     data = ovarian,
#'     dist = "llogis"
#'   )
#'
#'   # Generate survival probability matrix
#'   surv_matrix_fsr <- make_surv(fit_fsr, t = time_points, nsim = n_sim)
#'   cat("flexsurvreg method output dimensions:", dim(surv_matrix_fsr), "\n")
#' }
#'
#' #--------------------------------------
#' ## Default method for a numeric vector (e.g., from a Kaplan-Meier curve)
#' #--------------------------------------
#' if (rlang::is_installed("survival")) {
#'   library(survival)
#'   km_fit <- survfit(Surv(futime, fustat) ~ 1, data = ovarian)
#'   # Extract survival probabilities at our time points
#'   km_summary <- summary(km_fit, times = time_points)
#'
#'   # Generate matrix by replicating the single survival curve
#'   surv_matrix_vec <- make_surv(km_summary$surv, t = 0:(length(km_summary$surv) - 1), nsim = n_sim)
#'   cat("Default (vector) method output dimensions:", dim(surv_matrix_vec), "\n")
#' }
#'
#' #--------------------------------------
#' ## Default method for a matrix (pre-simulated curves)
#' #--------------------------------------
#' # Create a sample matrix of survival probabilities (500 time points, 50 simulations)
#' pre_sim_matrix <- sapply(1:n_sim, function(i) 1 - pweibull(1:500, shape = 1.5, scale = 100 + i))
#'
#' # Use make_surv to subset the matrix for our desired time points
#' surv_matrix_mat <- make_surv(pre_sim_matrix, t = time_points)
#' cat("Default (matrix) method output dimensions:", dim(surv_matrix_mat), "\n")
#'
#' #--------------------------------------
#' ## Method for 'inla' objects (conceptual example)
#' #--------------------------------------
#' \dontrun{
#' if (rlang::is_installed("INLA")) {
#'   # This method requires a fitted 'inla' object, typically from a
#'   # piecewise exponential model (poisson likelihood).
#'
#'   # Assuming 'fit_inla' is a valid model object from INLA:
#'   # surv_matrix_inla <- make_surv(fit_inla, t = time_points, nsim = n_sim)
#'   # print(dim(surv_matrix_inla))
#' }
#' }
make_surv <- function(Surv, ...)
  UseMethod("make_surv", Surv)


#' @rdname make_surv_methods
#' @param t A numeric vector of time points at which to calculate survival
#'   probabilities. The behaviour for `NULL` varies by method.
#' @param nsim The number of simulations to generate from the model's posterior
#'   distribution. Defaults to 100.
#' @importFrom survHE make.surv
#' @export
#'
make_surv.survHE <- function(Surv, t, nsim = 100, ...) {
  extr <- survHE::make.surv(Surv, t = t, nsim = nsim)
  as.matrix(extr$mat[[1]])[, -1]
}


#' @rdname make_surv_methods
#' @details For `flexsurvreg` objects, parameters are sampled from the asymptotic
#'   normal distribution of the maximum likelihood estimates using
#'   `flexsurv::normboot.flexsurvreg()`. If `t` is `NULL`, the unique
#'   event/censoring times from the model's source data are used.
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
#'
#' @details
#' ### INLA Method
#' The `inla` method requires the **INLA** package. As it is not available on CRAN,
#' you must install it from its own repository:
#' `install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"), dep = TRUE)`
#'
#' This method is designed for `inla` objects fitted with a `poisson` likelihood
#' for piecewise exponential models. It samples from the joint posterior of the
#' baseline hazard to calculate survival probabilities. If `t` is `NULL`, the
#' interval cut-points for the baseline hazard from the model are used.
#'
#' @import sn
#' @importFrom tibble as_tibble
#' @importFrom dplyr select contains
#' @export
#'
make_surv.inla <- function(Surv, t = NULL, nsim = 100, ...) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop(
      "The 'INLA' package is required to use this function. ",
      "Please install it from its repository by running: ",
      "install.packages('INLA', repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/stable'), dep = TRUE)"
    )
  }

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
#' @details
#' ### Default Method
#' The default method handles pre-computed survival probabilities.
#'   - If `Surv` is a **vector**, it is treated as a single survival curve (e.g.,
#'     from a Kaplan-Meier estimate). The function replicates this curve `nsim`
#'     times to form the output matrix.
#'   - If `Surv` is a **matrix**, it is assumed to already be in the desired
#'     (time x simulations) format. The function will simply subset rows based on `t`.
#'
#' If `t` is `NULL`, a sequence `0, 1, 2, ...` is generated based on the length
#' or number of rows of `Surv`. Note that time points are used as 1-based indices,
#' so `t = 0` corresponds to the first row/element.
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
