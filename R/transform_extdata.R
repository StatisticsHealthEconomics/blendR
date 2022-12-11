
#' Transform external data from risk set format
#'
#' @param extdat External data; data frame
#' @param tmax Maximum survival time
#'
#' @return Survival probabilities
#' @export
#'
#' @examples
#' extdat <- data.frame(start = c(5, 10, 15, 20),
#'                      stop =  c(10, 15, 20, 25),
#'                      n = c(100, 100, 100, 100),
#'                      r = c(50, 40, 30, 20))
#' transform_extdata(extdat, tmax = 100)
#'
transform_extdata <- function(extdat, tmax = 100) {
browser()

  extdat <- extdat |>
    mutate(prop = r/n,
           pcum = cumprod(prop))

  # need to assume something about the absolute survival probabilities
  # assume linear

  S_start <- 1 - (1/tmax)*extdat$start[1]

  data.frame(s = c(S_start, S_start*extdat$pcum),
             t = c(extdat$start[1], extdat$stop))
}
