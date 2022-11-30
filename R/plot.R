
#' Blended survival curve based on short-term data and external information
#'
#' @param x A blended survival curve object obtain from \code{blendsurv}
#' @param tp Time points
#' @param ... Additional arguments
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @return \pkg{ggplot2} object
#' @seealso \code{\link{blendsurv}}
#' @method plot blended
#' @export
plot.blended <- function(x, tp = seq(0, 180), ...) {

  obs_Surv <- x$S_obs
  ext_Surv <- x$S_ext
  ble_Surv <- x$mat

  alpha <- 0.1

  ggplot() +
    xlim(0, 180) + ylim(0,1) +
    geom_line(aes(tp, rowMeans(obs_Surv), colour = "Data fitting"),
              size = 1, linetype = "twodash") +
    geom_ribbon(aes(x = tp, y = rowMeans(obs_Surv),
                    ymin = apply(obs_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(obs_Surv, 1, quantile, probs = 0.975)), alpha = alpha) +
    geom_line(aes(tp, rowMeans(ext_Surv), colour = "External info"),
              size = 1, linetype = "longdash") +
    geom_ribbon(aes(x = tp, y = rowMeans(ext_Surv),
                    ymin = apply(ext_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(ext_Surv, 1, quantile, probs = 0.975)), alpha = alpha) +
    geom_line(aes(tp, rowMeans(ble_Surv), colour = "Blended curve"), size = 1.25) +
    geom_ribbon(aes(x = tp, y = rowMeans(ble_Surv),
                    ymin = apply(ble_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(ble_Surv, 1, quantile, probs = 0.975)), alpha = alpha) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 8)) +
    xlab("Time (months)") + ylab("Survival") +
    scale_colour_manual(name = "Model",
                        values = c("Data fitting" = "#7CAE00",
                                   "External info" = "#00BFC4",
                                   "Blended curve" = "#F8766D",
                                   "Kaplan-Meier" = "brown"))
}

#' Plots the weights for the blending procedure
#'
#' @param x A blended survival curve object obtain from \code{blendsurv}
#' @param ... Additional arguments
#' @import ggplot2
#'
#' @return \pkg{ggplot2} object
#' @seealso \code{\link{blendsurv}}
#' @export

weightplot <- function(x, ...) {
  tibble(
    t=x$times,
    t_scaled=(t-x$blend_interv$min)/(x$blend_interv$max-x$blend_interv$min),
    y=pbeta(t_scaled,x$beta_params$alpha,x$beta_params$beta)) |>
    mutate(
      y=case_when(t_scaled<0~0,t_scaled>1~1,TRUE~y)
    ) |>
    ggplot(aes(t,y)) + geom_line() + theme_bw() + xlab("Time") + ylab("Weight function") +
    annotate("text",x$blend_interv |> as.numeric() |> mean(),1.025,label="Blending interval",hjust=0.5,vjust=-1) +
    geom_segment(
      aes(x=x$blend_interv$min,y=1.025,xend=x$blend_interv$max,yend=1.025),arrow=arrow(length=unit(0.2,"cm"),ends="both",type="closed")
    )
}
