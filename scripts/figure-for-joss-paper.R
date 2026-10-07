library(survHE)
## trial data
data("dat_FCR", package = "blendR")
## externally estimated data
## externally estimated data
# data_sim <- ext_surv_sim(t_info = 144,
#                          S_info = 0.05,
#                          T_max = 240)

# 1. Update the simulated external data to match the caption
data_sim <- ext_surv_sim(t_info = 156,  # 13 years * 12 months
                         S_info = 0.10, # 10% expected survival
                         T_max = 240)

# [Re-run obs_Surv, ext_Surv, and ble_Surv here]

# observed survival times
obs_Surv <- fit.models(formula = Surv(death_t, death) ~ 1,
                      data = dat_FCR,
                      distr = "exponential",
                      method = "hmc")
# external survival times
ext_Surv <- fit.models(formula = Surv(time, event) ~ 1,
                      data = data_sim,
                      distr = "exponential",
                      method = "hmc")
# blending parameter values
blend_interv <- list(min = 48, max = 150)
beta_params <- list(alpha = 3, beta = 3)
ble_Surv <- blendsurv(obs_Surv, ext_Surv, blend_interv, beta_params)
# all survival curves
plot(ble_Surv)

km <- survfit(Surv(death_t, death) ~ 1, data = dat_FCR)

plot(ble_Surv) +
  geom_line(aes(km$time, km$surv, colour = "Kaplan-Meier"),
            linewidth = 1.25, linetype = "dashed")

###############

library(ggplot2)

# Calculate approximate Y position for the black dot at month 150
y_at_b <- 0.1

library(ggplot2)

plot(ble_Surv, t = seq(0, 240)) +
  geom_step(aes(x = km$time, y = km$surv, colour = "Kaplan-Meier"),
            linewidth = 1.25, linetype = "dashed") +
  scale_colour_manual(
    name = "model",
    values = c(
      "Data fitting" = "#7CAE00",
      "External info" = "#00BFC4",
      "Blended curve" = "#F8766D",
      "Kaplan-Meier" = "#C7A7D2"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    text = element_text(family = "serif")
  ) +
  geom_vline(xintercept = c(48, 150), linetype = "dotted") +
  scale_x_continuous(
    name = "Time since randomisation (years)",
    limits = c(0, 250),
    breaks = c(0, 48, 60, 120, 150, 180, 240),
    labels = c("0", "a", "5", "10", "b", "15", "20")
  ) +

  # The black point exactly at the expert information (13 years/156 months, 10% survival)
  annotate("point", x = 156, y = 0.10, size = 4, colour = "black") +

  # Follow-up
  annotate("text", x = 24, y = 0.1, label = "Follow-up", family = "serif") +
  annotate("segment", x = 12, xend = 0, y = 0.1, yend = 0.1,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 36, xend = 48, y = 0.1, yend = 0.1,
           arrow = arrow(length = unit(0.2, "cm"))) +

  # Blending interval
  annotate("text", x = 99, y = 0.9, label = "Blending interval", family = "serif") +
  annotate("segment", x = 78, xend = 48, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 120, xend = 150, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +

  # Long term
  annotate("text", x = 185, y = 0.9, label = "Long term", family = "serif") +
  annotate("segment", x = 168, xend = 150, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 202, xend = 240, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +

  guides(colour = guide_legend(
    override.aes = list(
      linetype = c(
        "Data fitting" = "dotdash",
        "External info" = "longdash",
        "Blended curve" = "solid",
        "Kaplan-Meier" = "dashed"
      )
    )
  ))
