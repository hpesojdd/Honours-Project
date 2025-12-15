# Multi-panel occupancy effect plot (fixed effects, scaled covariates)

library(ggplot2)
library(patchwork)

n_grid_pts <- 100
n_sites <- nrow(occ)

mu_arable <- mean(occ$arable_area, na.rm = TRUE)
sd_arable <- sd(occ$arable_area, na.rm = TRUE)
mu_shrub  <- mean(occ$shrub_density, na.rm = TRUE)
sd_shrub  <- sd(occ$shrub_density, na.rm = TRUE)
mu_wood   <- mean(occ$woodland_density, na.rm = TRUE)
sd_wood   <- sd(occ$woodland_density, na.rm = TRUE)
mu_canopy <- mean(occ$canopy_cover, na.rm = TRUE)
sd_canopy <- sd(occ$canopy_cover, na.rm = TRUE)
mu_dist   <- mean(occ$distance_from_release_pen_m, na.rm = TRUE)
sd_dist   <- sd(occ$distance_from_release_pen_m, na.rm = TRUE)

scale1 <- function(x, mu, sd) (x - mu) / sd

make_plot <- function(focal_name, xlab, title_txt, ylab_txt = NULL) {
  focal_raw <- switch(
    focal_name,
    distance_from_release_pen_m = seq(min(occ$distance_from_release_pen_m, na.rm = TRUE),
                                      max(occ$distance_from_release_pen_m, na.rm = TRUE),
                                      length.out = n_grid_pts),
    arable_area = seq(min(occ$arable_area, na.rm = TRUE),
                      max(occ$arable_area, na.rm = TRUE),
                      length.out = n_grid_pts),
    shrub_density = seq(min(occ$shrub_density, na.rm = TRUE),
                        max(occ$shrub_density, na.rm = TRUE),
                        length.out = n_grid_pts),
    woodland_density = seq(min(occ$woodland_density, na.rm = TRUE),
                           max(occ$woodland_density, na.rm = TRUE),
                           length.out = n_grid_pts),
    canopy_cover = seq(min(occ$canopy_cover, na.rm = TRUE),
                       max(occ$canopy_cover, na.rm = TRUE),
                       length.out = n_grid_pts),
    stop("unknown focal_name")
  )
  
  focal_scl <- switch(
    focal_name,
    distance_from_release_pen_m = scale1(focal_raw, mu_dist, sd_dist),
    arable_area                 = scale1(focal_raw, mu_arable, sd_arable),
    shrub_density               = scale1(focal_raw, mu_shrub, sd_shrub),
    woodland_density            = scale1(focal_raw, mu_wood, sd_wood),
    canopy_cover                = scale1(focal_raw, mu_canopy, sd_canopy),
    stop("unknown focal_name")
  )
  
  n_grid <- length(focal_raw)
  
  # Non-focal covariates fixed at scaled means (0)
  arable_col <- rep(0, n_sites * n_grid)
  shrub_col  <- rep(0, n_sites * n_grid)
  wood_col   <- rep(0, n_sites * n_grid)
  canopy_col <- rep(0, n_sites * n_grid)
  dist_col   <- rep(0, n_sites * n_grid)
  
  rep_focal <- rep(focal_scl, each = n_sites)
  if (focal_name == "distance_from_release_pen_m") dist_col   <- rep_focal
  if (focal_name == "arable_area")                 arable_col <- rep_focal
  if (focal_name == "shrub_density")               shrub_col  <- rep_focal
  if (focal_name == "woodland_density")            wood_col   <- rep_focal
  if (focal_name == "canopy_cover")                canopy_col <- rep_focal
  
  occ_stack <- data.frame(
    arable_area = arable_col,
    distance_from_release_pen_m = dist_col,
    shrub_density = shrub_col,
    woodland_density = wood_col,
    canopy_cover = canopy_col
  )
  
  X.0 <- model.matrix(
    ~ arable_area + distance_from_release_pen_m + shrub_density + woodland_density + canopy_cover,
    data = occ_stack
  )
  
  beta_samps <- fit$beta.samples
  eta_draws <- beta_samps %*% t(X.0)
  psi_mat <- 1 / (1 + exp(-eta_draws))
  
  n_draws <- nrow(psi_mat)
  dim(psi_mat) <- c(n_draws, n_sites, n_grid)
  
  psi_site_mean_by_draw <- apply(psi_mat, c(1, 3), mean)
  
  plot_df <- data.frame(
    focal_raw = focal_raw,
    psi = colMeans(psi_site_mean_by_draw),
    lwr = apply(psi_site_mean_by_draw, 2, quantile, 0.025),
    upr = apply(psi_site_mean_by_draw, 2, quantile, 0.975)
  )
  
  ggplot(plot_df, aes(x = focal_raw, y = psi)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
    geom_line() +
    labs(x = xlab, y = ylab_txt, title = title_txt) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10),
      axis.title = element_text(size = 9),
      axis.text  = element_text(size = 8)
    )
}

p_dist <- make_plot("distance_from_release_pen_m", "Distance from release pen (m)", "A", NULL)
p_arable <- make_plot("arable_area", "Arable area (%)", "B", "Occupancy probability (ψ)")
p_shrub <- make_plot("shrub_density", "Shrub density (%)", "C", NULL)
p_wood <- make_plot("woodland_density", "Woodland density (%)", "D", NULL)
p_canopy <- make_plot("canopy_cover", "Canopy cover (%)", "E", NULL)

bottom_grid <- (p_arable | p_shrub) / (p_wood | p_canopy)

final_plot <- p_dist / bottom_grid +
  plot_layout(heights = c(1.2, 2))

final_plot
