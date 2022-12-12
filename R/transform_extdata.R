
#' Transform external data from risk set format
#'
#' @param extdat Long-term data typically from elicitation or
#'    external sources; data frame
#' @param tmax Maximum survival time
#' @param n Number of samples
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
transform_extdata <- function(extdat,
                              tmax = 100,
                              n = 100) {
  extdat <- extdat |>
    mutate(prop = r/n,
           m = n-r,
           pcum = cumprod(prop))

  # need to assume something about the absolute survival probabilities
  # assume linear
  ##TODO: need to think about best option

  S_start <- 1 - (1/tmax)*extdat$start[1]

  r_pcum <-
    purrr::map2_dfc(extdat$r, extdat$m, ~rbeta(n, .x, .y)) |>
    t() |>
    as.data.frame() |>
    mutate(across(everything(), ~round(cumprod(.x), 4)))
browser()
  # sample event times
  purrr::map(r_pcum, ~ext_surv_sim(extdat$stop, .x, T_max = 100, n = 2)) |>
    do.call(what = rbind)

  data.frame(t = c(extdat$start[1], extdat$stop),
             s = c(S_start, S_start*extdat$pcum),
             rbind(S_start, r_pcum))
}


##TODO: how to sample from ext_surv_sim() properly
##      refactor duplication with transform_extdata()
riskset_surv_sim <- function(extdat,
                             tmax = 100,
                             n = 100) {
  extdat <- extdat |>
    mutate(prop = r/n,
           m = n-r,
           pcum = cumprod(prop))

  S_start <- 1 - (1/tmax)*extdat$start[1]

  r_pcum <-
    purrr::map2_dfc(extdat$r, extdat$m, ~rbeta(n, .x, .y)) |>
    t() |>
    as.data.frame() |>
    mutate(across(everything(), ~round(cumprod(.x), 4)))

  # sample event times
  times <-
    purrr::map(
      r_pcum, ~ext_surv_sim(
        extdat$stop, .x, T_max = 100, n = 1)) |>
    do.call(what = rbind)

  times
}
