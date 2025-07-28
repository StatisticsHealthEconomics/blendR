
#' Blended survival curve based on short-term data and external information
#'
#' @param x A blended survival curve object obtain from [blendsurv()]
#' @param alpha A vector specifying the opacity of ribbon for the blended curve and other curves
#' @param ... Additional arguments
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @return A \pkg{ggplot2} object
#' @seealso [blendsurv()]
#' @method plot blended
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
#' obs_Surv <- fit.models(formula = Surv(death_t, death) ~ 1,
#'                        data = dat_FCR,
#'                        distr = "exponential",
#'                        method = "hmc")
#'
#' ext_Surv <- fit.models(formula = Surv(time, event) ~ 1,
#'                        data = data_sim,
#'                        distr = "exponential",
#'                        method = "hmc")
#'
#' blend_interv <- list(min = 48, max = 150)
#' beta_params <- list(alpha = 3, beta = 3)
#'
#' ble_Surv <- blendsurv(obs_Surv, ext_Surv, blend_interv, beta_params)
#'
#' plot(ble_Surv)
#'
plot.blended <- function(x, alpha = c(0.1,0.05), ...) {
  dots <- list(...)

  obs_Surv <- x$S_obs
  ext_Surv <- x$S_ext
  ble_Surv <- x$mat

  times <- x$times

  if ("xlim" %in% names(dots)) {
    keep_times <- times > dots$xlim[1] & times < dots$xlim[2]

    times <- times[keep_times]
    obs_Surv <- obs_Surv[keep_times, ]
    ext_Surv <- ext_Surv[keep_times, ]
    ble_Surv <- ble_Surv[keep_times, ]
  }

  xtext <- ifelse(is.null(dots$xlab), "Time", dots$xlab)

  ci <- list(low = 0.025, high = 0.975)

  ggplot() +
    ylim(0,1) +
    geom_line(aes(times, rowMeans(obs_Surv), colour = "Data fitting"),
              size = 1, linetype = "twodash") +
    geom_ribbon(aes(x = times, y = rowMeans(obs_Surv),
                    ymin = apply(obs_Surv, 1, quantile, probs = ci$low),
                    ymax = apply(obs_Surv, 1, quantile, probs = ci$high)), alpha = alpha[2]) +
    geom_line(aes(times, rowMeans(ext_Surv), colour = "External info"),
              size = 1, linetype = "longdash") +
    geom_ribbon(aes(x = times, y = rowMeans(ext_Surv),
                    ymin = apply(ext_Surv, 1, quantile, probs = ci$low),
                    ymax = apply(ext_Surv, 1, quantile, probs = ci$high)), alpha = alpha[2]) +
    geom_line(aes(times, rowMeans(ble_Surv), colour = "Blended curve"), size = 1.25) +
    geom_ribbon(aes(x = times, y = rowMeans(ble_Surv),
                    ymin = apply(ble_Surv, 1, quantile, probs = ci$low),
                    ymax = apply(ble_Surv, 1, quantile, probs = ci$high)), alpha = alpha[1]) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 8)) +
    xlab(xtext) + ylab("Survival") +
    scale_colour_manual(name = "Model",
                        values = c("Data fitting" = "#7CAE00",
                                   "External info" = "#00BFC4",
                                   "Blended curve" = "#F8766D",
                                   "Kaplan-Meier" = "brown")) +
    theme_bw() +
    theme(legend.position = c(0.9, 0.85),
          legend.background = element_rect(fill = 'transparent'))
}

#' Plots the weights for the blending procedure
#'
#' @param x A blended survival curve object obtained from [blendsurv()]
#' @param ... Additional arguments
#' @import ggplot2 tibble dplyr
#'
#' @return \pkg{ggplot2} object
#' @seealso [blendsurv()]
#' @importFrom stats pbeta
#' @export
#'
weightplot <- function(x, ...) {
  tibble(
    t = x$times,
    t_scaled = (t - x$blend_interv$min)/(x$blend_interv$max - x$blend_interv$min),
    y = stats::pbeta(.data$t_scaled, x$beta_params$alpha, x$beta_params$beta)) |>
    mutate(
      y = case_when(t_scaled < 0 ~ 0,
                    t_scaled > 1 ~ 1,
                    TRUE ~ y)) |>
    ggplot(aes(.data$t, .data$y)) +
    geom_line() +
    theme_bw() +
    xlab("Time") + ylab("Weight function") +
    annotate("text",
             x$blend_interv |> as.numeric() |> mean(),
             1.025, label = "Blending interval", hjust = 0.5, vjust = -1) +
    geom_segment(
      aes(x = x$blend_interv$min, y = 1.025,
          xend = x$blend_interv$max, yend=1.025),
      arrow = arrow(length = unit(0.2,"cm"), ends = "both", type = "closed")
    )
}
