
#' @title Get event time data
#' @name data_times
#'
data_times <- function(surv_res)
  UseMethod("data_times", surv_res)

#' @rdname data_times
#'
data_times.inla <- function(surv_res) {
  time_var <-
    surv_res[[".args"]][[".parent.frame"]][["inla.formula"]][[2]][[2]]

  surv_res[[".args"]][[".parent.frame"]]$data[[time_var]]
}

#' @rdname data_times
#'
data_times.survHE <- function(surv_res) {
  surv_res[["misc"]][["data.stan"]][[1]][["t"]]
}

#' @rdname data_times
#'
data_times.flexsurvreg <- function(surv_res) {
  as.numeric(surv_res[["data"]][["m"]][["Surv(time, event)"]])
}
