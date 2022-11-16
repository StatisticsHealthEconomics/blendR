
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
#' @export
#'
plot.blended <- function(x, tp = seq(0, 180), ...) {

  obs_Surv <- x$S_obs
  ext_Surv <- x$S_ext
  ble_Surv <- x$mat

  ggplot() +
    xlim(0, 180) + ylim(0,1) +
    geom_line(aes(tp, rowMeans(obs_Surv), colour = "Data fitting"),
              size = 1, linetype = "twodash") +
    geom_ribbon(aes(x = tp, y = rowMeans(obs_Surv),
                    ymin = apply(obs_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(obs_Surv, 1, quantile, probs = 0.975)), alpha = 0.1) +
    geom_line(aes(tp, rowMeans(ext_Surv), colour = "External info"),
              size = 1, linetype = "longdash") +
    geom_ribbon(aes(x = tp, y = rowMeans(ext_Surv),
                    ymin = apply(ext_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(ext_Surv, 1, quantile, probs = 0.975)), alpha = 0.1) +
    geom_line(aes(tp, rowMeans(ble_Surv), colour = "Blended curve"), size = 1.25) +
    geom_ribbon(aes(x = tp, y = rowMeans(ble_Surv),
                    ymin = apply(ble_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(ble_Surv, 1, quantile, probs = 0.975)), alpha = 0.1) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 8)) +
    xlab("Time (months)") + ylab("Survival") +
    scale_colour_manual(name = "model",
                        values = c("Data fitting"="#7CAE00", "External info"="#00BFC4",
                                   "Blended curve"="#F8766D", "Kaplan-Meier"="brown"))
}
