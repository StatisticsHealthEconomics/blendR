
#' @title Get event time data
#' @name data_times
#' @param S vector of survival data
#' @return A vector of event times
#' @keywords internal
#'
data_times <- function(S)
  UseMethod("data_times", S)

#' @rdname data_times
#' @keywords internal
#'
data_times.inla <- function(S) {
  time_var <-
    S[[".args"]][[".parent.frame"]][["inla.formula"]][[2]][[2]]

  S[[".args"]][[".parent.frame"]]$data[[time_var]]
}

#' @rdname data_times
#' @keywords internal
#'
data_times.survHE <- function(S) {
  S[["misc"]][["data.stan"]][[1]][["t"]]
}

#' @rdname data_times
#' @keywords internal
#'
data_times.flexsurvreg <- function(S) {
  as.numeric(S[["data"]][["m"]][["Surv(time, event)"]])
}

#' @rdname data_times
#' @keywords internal
#'
data_times.default <- function(S) {
  if (is.null(dim(S)))
    return(0:(length(S) - 1))

  0:(nrow(S) - 1)
}
