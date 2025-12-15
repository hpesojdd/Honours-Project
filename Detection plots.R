# Multi-panel detection effect plot

library(ggplot2)
library(patchwork)

n_grid_pts <- 200
scale1 <- function(x, mu, sd) (x - mu) / sd

mu_days <- mean(days,  na.rm = TRUE); sd_days <- sd(days,  na.rm = TRUE)
mu_rain <- mean(rain,  na.rm = TRUE); sd_rain <- sd(rain,  na.rm = TRUE)
mu_temp <- mean(tempv, na.rm = TRUE); sd_temp <- sd(tempv, na.rm = TRUE)
mu_veg  <- mean(veg,   na.rm = TRUE); sd_veg  <- sd(veg,   na.rm = TRUE)

view_fix <- round(mean(as.numeric(ViewOpen), na.rm = TRUE))

alpha_samps <- fit$alpha.samples
a0    <- alpha_samps[, "(Intercept)"]
aDays <- alpha_samps[, "Days"]
aRain <- alpha_samps[, "Rain"]
aTemp <- alpha_samps[, "Temp"]
aVegH <- alpha_samps[, "VegH"]
aView <- alpha_samps[, "ViewOpen"]

days_z_fix <- 0
rain_z_fix <- 0
temp_z_fix <- 0
veg_z_fix  <- 0

summ_ci <- function(p_mat) {
  data.frame(
    p   = colMeans(p_mat),
    lwr = apply(p_mat, 2, quantile, 0.025),
    upr = apply(p_mat, 2, quantile, 0.975)
  )
}

make_det_plot <- function(focal_name, xlab, title_txt, show_y = TRUE) {
  ylab_txt <- if (show_y) expression("Detection probability (" ~ italic(p) * ")") else NULL
  
  if (focal_name == "view") {
    focal_raw <- c(0, 1)
    
    eta <- cbind(
      a0 + aDays * days_z_fix + aRain * rain_z_fix + aTemp * temp_z_fix + aVegH * veg_z_fix + aView * focal_raw[1],
      a0 + aDays * days_z_fix + aRain * rain_z_fix + aTemp * temp_z_fix + aVegH * veg_z_fix + aView * focal_raw[2]
    )
    
    p_mat <- 1 / (1 + exp(-eta))
    s <- summ_ci(p_mat)
    
    plot_df <- data.frame(
      view_label = factor(focal_raw, levels = c(0, 1), labels = c("Closed view", "Open view")),
      p = s$p, lwr = s$lwr, upr = s$upr
    )
    
    return(
      ggplot(plot_df, aes(x = view_label, y = p)) +
        geom_point(size = 2.5) +
        geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.15, linewidth = 0.6) +
        coord_cartesian(ylim = c(0, 1)) +
        labs(x = xlab, y = ylab_txt, title = title_txt) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 10, hjust = 0),
          axis.title = element_text(size = 9),
          axis.text  = element_text(size = 8)
        )
    )
  }
  
  focal_raw <- switch(
    focal_name,
    days = seq(min(days,  na.rm = TRUE), max(days,  na.rm = TRUE), length.out = n_grid_pts),
    rain = seq(min(rain,  na.rm = TRUE), max(rain,  na.rm = TRUE), length.out = n_grid_pts),
    temp = seq(min(tempv, na.rm = TRUE), max(tempv, na.rm = TRUE), length.out = n_grid_pts),
    veg  = seq(min(veg,   na.rm = TRUE), max(veg,   na.rm = TRUE), length.out = n_grid_pts),
    stop("unknown focal_name")
  )
  
  focal_z <- switch(
    focal_name,
    days = scale1(focal_raw, mu_days, sd_days),
    rain = scale1(focal_raw, mu_rain, sd_rain),
    temp = scale1(focal_raw, mu_temp, sd_temp),
    veg  = scale1(focal_raw, mu_veg,  sd_veg)
  )
  
  focal_coef <- switch(focal_name, days = aDays, rain = aRain, temp = aTemp, veg = aVegH)
  
  n_grid <- length(focal_raw)
  
  eta_const <- a0 +
    aDays * days_z_fix +
    aRain * rain_z_fix +
    aTemp * temp_z_fix +
    aVegH * veg_z_fix +
    aView * view_fix
  
  eta <- (eta_const %*% matrix(1, 1, n_grid)) + (focal_coef %*% t(focal_z))
  p_mat <- 1 / (1 + exp(-eta))
  s <- summ_ci(p_mat)
  
  plot_df <- data.frame(
    focal_raw = focal_raw,
    p = s$p, lwr = s$lwr, upr = s$upr
  )
  
  ggplot(plot_df, aes(x = focal_raw, y = p)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    geom_line() +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = ylab_txt, title = title_txt) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, hjust = 0),
      axis.title = element_text(size = 9),
      axis.text  = element_text(size = 8)
    )
}

p_days <- make_det_plot("days", "Days since project began", "A", TRUE)
p_rain <- make_det_plot("rain", "Rainfall (mm)", "B", TRUE)
p_temp <- make_det_plot("temp", expression("Temperature (" * degree * "C)"), "C", FALSE)
p_veg  <- make_det_plot("veg",  "Vegetation height (cm)", "D", TRUE)
p_view <- make_det_plot("view", "Camera view (Open/Closed)", "E", FALSE)

bottom_grid <- (p_rain | p_temp) / (p_veg | p_view)
final_plot <- p_days / bottom_grid + plot_layout(heights = c(1.2, 2))

final_plot
