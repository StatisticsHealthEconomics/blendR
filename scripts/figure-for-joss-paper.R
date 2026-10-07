
library(survHE)
library(blendR)
library(ggplot2)

data("dat_FCR", package = "blendR")

# 1. Increase sample size 'n' to eliminate random noise so the fit hits exactly 10%
set.seed(42) # Lock the random generation
data_sim <- ext_surv_sim(t_info = 156,
                         S_info = 0.10,
                         T_max = 240)

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

blend_interv <- list(min = 48, max = 150)
beta_params <- list(alpha = 3, beta = 3)

# 2. Pass the 'times' argument here to natively compute the curve out to 20 years
ble_Surv <- blendsurv(obs_Surv, ext_Surv, blend_interv, beta_params, times = seq(0, 240))

km <- survfit(Surv(death_t, death) ~ 1, data = dat_FCR)


# 1. Generate the base plot object
base_plot <- plot(ble_Surv, t = seq(0, 240))

# 2. Extract exactly from the line layer, explicitly ignoring the ribbon layers (which contain 'ymin')
pb <- ggplot_build(base_plot)
line_layer <- pb$data[[which(sapply(pb$data, function(l) "y" %in% names(l) && !("ymin" %in% names(l))))[1]]]

# The lowest curve exactly at 13 years (156 months) in the line layer is the External info curve
y_at_156 <- min(line_layer$y[abs(line_layer$x - 156) < 0.1], na.rm = TRUE)

# 1. Generate the base plot object
base_plot <- plot(ble_Surv, t = seq(0, 240))

# 2. Dynamically search ALL layers to find the exact y-coordinate of the lowest line at x = 156
pb <- ggplot_build(base_plot)
y_vals <- unlist(lapply(pb$data, function(layer) {
  # Only check layers that draw lines (contain 'y' but not 'ymin' ribbons)
  if ("y" %in% names(layer) && !("ymin" %in% names(layer))) {
    groups <- if ("group" %in% names(layer)) unique(layer$group) else 1
    sapply(groups, function(g) {
      g_data <- if ("group" %in% names(layer)) layer[layer$group == g, ] else layer
      # Interpolate the exact value at 156 months to avoid grid-step misses
      if (nrow(g_data) > 1 && min(g_data$x) <= 156 && max(g_data$x) >= 156) {
        approx(x = g_data$x, y = g_data$y, xout = 156)$y
      } else {
        NA
      }
    })
  }
}))

# The lowest valid line exactly at 13 years (156 months) is the External curve
y_at_156 <- min(y_vals, na.rm = TRUE)

# 1. Generate the base plot object and safely extract the exact coordinate
base_plot <- plot(ble_Surv, t = seq(0, 240))
pb <- ggplot_build(base_plot)
y_vals <- unlist(lapply(pb$data, function(layer) {
  if ("y" %in% names(layer) && !("ymin" %in% names(layer))) {
    groups <- if ("group" %in% names(layer)) unique(layer$group) else 1
    sapply(groups, function(g) {
      g_data <- if ("group" %in% names(layer)) layer[layer$group == g, ] else layer
      if (nrow(g_data) > 1 && min(g_data$x) <= 156 && max(g_data$x) >= 156) {
        approx(x = g_data$x, y = g_data$y, xout = 156)$y
      } else {
        NA
      }
    })
  }
}))
y_at_156 <- min(y_vals, na.rm = TRUE)

# 2. Build the final plot and assign it to an object
final_plot <- base_plot +
  geom_step(aes(x = km$time, y = km$surv, colour = "Kaplan-Meier"),
            linewidth = 3, linetype = "dotted") +
  scale_colour_manual(
    name = "Model",
    values = c(
      "Data fitting" = "#7CAE00",
      "External info" = "#00BFC4",
      "Blended curve" = "#F8766D",
      "Kaplan-Meier" = "black"
    )
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    # Increase base font size for axes and legend
    text = element_text(family = "serif", size = 16)
  ) +
  geom_vline(xintercept = c(48, 150), linetype = "dotted") +
  scale_x_continuous(
    name = "Time since randomisation (years)",
    limits = c(0, 250),
    breaks = c(0, 48, 60, 120, 150, 180, 240),
    labels = c("0", "a", "5", "10", "b", "15", "20")
  ) +

  annotate("point", x = 156, y = y_at_156, size = 4, colour = "black") +

  # Follow-up (Increased text size to 5)
  annotate("text", x = 24, y = 0.1, label = "Follow-up", family = "serif", size = 5) +
  annotate("segment", x = 12, xend = 0, y = 0.1, yend = 0.1,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 36, xend = 48, y = 0.1, yend = 0.1,
           arrow = arrow(length = unit(0.2, "cm"))) +

  # Blending interval (Increased text size to 5)
  annotate("text", x = 99, y = 0.9, label = "Blending interval", family = "serif", size = 5) +
  annotate("segment", x = 78, xend = 48, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 120, xend = 150, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +

  # Long term (Increased text size to 5)
  annotate("text", x = 185, y = 0.9, label = "Long term", family = "serif", size = 5) +
  annotate("segment", x = 168, xend = 150, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +
  annotate("segment", x = 202, xend = 240, y = 0.9, yend = 0.9,
           arrow = arrow(length = unit(0.2, "cm"))) +

  guides(colour = guide_legend(
    override.aes = list(
      linetype = c("solid", "dotdash", "dashed", "dotted")
    )
  ))

final_plot

# 3. Save the plot at publication resolution (300 dpi)
ggsave(filename = here::here("plots/blended_survival_curve.png"),
       plot = final_plot, width = 12, height = 7, dpi = 300)
