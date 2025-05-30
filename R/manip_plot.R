
#' Blended survival plot with manipulate
#'
#' RStudio bug
#' need to run base R first
#' `manipulate(plot(1:x), x = slider(5, 10))`
#'
#' @param obs_Surv Observed survival
#' @param ext_Surv External survival
#' @param blend_interv Blending interval
#' @return Blended survival plot
#' @import manipulate
#'
manip_plot <- function(obs_Surv, ext_Surv, blend_interv) {
  a <- b <- NULL
  manipulate::manipulate(
    {params <- list(obs_Surv = obs_Surv,
                    ext_Surv = ext_Surv,
                    blend_interv = blend_interv,
                    beta_params = list(alpha = a,
                                       beta = b))
    x <- do.call(blendsurv, params)
    plot(x)},
    a = slider(1, 5, initial = 1),
    b = slider(1, 5, initial = 1)
  )
}
