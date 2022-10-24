
#' Blended survival curve based on short-term data and external information
#'
#' @param ble_Surv
#'
#' @return
#' @export
#'
plot.blended <- function(ble_Surv) {

  obs_Surv <- ble_Surv$obs_Surv
  ext_Surv <- ble_Surv$ext_Surv
  ble_Surv <- ble_Surv$ble_Surv

  ggplot() +
    geom_line(aes(obs_Surv$KM$time, obs_Surv$KM$surv, colour = "Kaplan-Meier"),
              size = 1.25, linetype = "dashed") +
    xlim(0, 180) + ylim(0,1) +
    geom_line(aes(tp, rowMeans(obs_Surv$S_obs), colour = "Data fitting"),
              size = 1, linetype = "twodash") +
    geom_ribbon(aes(x = tp, y = rowMeans(obs_Surv$S_obs),
                    ymin = apply(obs_Surv$S_obs, 1, quantile, probs = 0.025),
                    ymax = apply(obs_Surv$S_obs, 1, quantile, probs = 0.975)), alpha = 0.1) +
    geom_line(aes(tp, rowMeans(ext_Surv$S_ext), colour = "External info"), size = 1, linetype = "longdash") +
    geom_ribbon(aes(x = tp, y = rowMeans(ext_Surv$S_ext), ymin = apply(ext_Surv$S_ext, 1, quantile, probs = 0.025),
                    ymax = apply(ext_Surv$S_ext, 1, quantile, probs = 0.975)), alpha = 0.1) +
    geom_line(aes(tp, rowMeans(ble_Surv), colour = "Blended curve"), size = 1.25) +
    geom_ribbon(aes(x = tp, y = rowMeans(ble_Surv), ymin = apply(ble_Surv, 1, quantile, probs = 0.025),
                    ymax = apply(ble_Surv, 1, quantile, probs = 0.975)), alpha = 0.1) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size = 8)) +
    xlab("Time (months)") + ylab("Survival") +
    scale_colour_manual(name = "model",
                        values = c("Data fitting"="#7CAE00", "External info"="#00BFC4",
                                   "Blended curve"="#F8766D", "Kaplan-Meier"="brown")) +
    annotate("text", x = 150, y = 0.9, label = "FCR arm", colour = "orange", fontface = 2)
}

