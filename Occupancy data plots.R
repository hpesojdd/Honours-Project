# Habitat covariates vs pheasant detections (hist + detection strip)

library(ggplot2)
library(dplyr)
library(patchwork)

setwd("C:\\4th Year\\Dissertation\\Camera trap spreadsheets\\State model spreadsheets")

# Theme
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# Jitter points
point_layer <- geom_jitter(
  width = 0, height = 0.12,
  shape = 21, size = 1.8,
  fill = "black", colour = "white",
  stroke = 0.25, alpha = 0.7
)

# Helpers
clean_num <- function(x) as.numeric(gsub("[^0-9\\.-]", "", x))

make_hist <- function(df, x, xlab, binwidth = NULL, bins = NULL, xlim = NULL, breaks = NULL) {
  mean_x <- mean(df[[x]], na.rm = TRUE)
  
  p <- ggplot(df, aes(x = .data[[x]])) +
    { if (!is.null(binwidth)) geom_histogram(binwidth = binwidth, boundary = 0, closed = "left",
                                             fill = "grey30", colour = "black", linewidth = 0.5)
      else geom_histogram(bins = bins, fill = "grey30", colour = "black", linewidth = 0.5) } +
    geom_vline(xintercept = mean_x, colour = "red", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = mean_x, y = Inf, label = sprintf("Mean = %.2f", mean_x),
             vjust = -0.4, colour = "red", size = 3.5) +
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(t = 8, r = 5, b = 5, l = 5)) +
    labs(x = xlab, y = "Count of detections") +
    plot_theme
  
  if (!is.null(xlim) || !is.null(breaks)) p <- p + scale_x_continuous(limits = xlim, breaks = breaks)
  p
}

make_scatter <- function(df, x, xlab, xlim = NULL, breaks = NULL, ylab_txt = "Pheasant\ndetection") {
  p <- ggplot(df, aes(x = .data[[x]], y = factor(detected))) +
    point_layer +
    labs(x = xlab, y = ylab_txt) +
    plot_theme
  
  if (!is.null(xlim) || !is.null(breaks)) p <- p + scale_x_continuous(limits = xlim, breaks = breaks)
  p
}

# 1) Shrub density (%)
df_shrub <- read.csv("Detections and shrub density 300m.csv", check.names = FALSE)
nm <- tolower(trimws(names(df_shrub)))
det_col <- grep("detect", nm)[1]
x_col   <- grep("shrub",  nm)[1]
if (is.na(det_col) || is.na(x_col)) stop("Missing detection or shrub column (shrub dataset).")

df_shrub <- df_shrub %>%
  transmute(
    detected = as.numeric(.data[[names(df_shrub)[det_col]]]),
    shrub_density = clean_num(.data[[names(df_shrub)[x_col]]])
  ) %>%
  filter(!is.na(detected), !is.na(shrub_density)) %>%
  mutate(detected = ifelse(detected >= 0.5, 1, 0))

p_shrub_hist <- make_hist(df_shrub %>% filter(detected == 1), "shrub_density",
                          "Shrub density (%)", binwidth = 5,
                          xlim = c(0, 100), breaks = seq(0, 100, 10))
p_shrub_scatter <- make_scatter(df_shrub, "shrub_density",
                                "Shrub density (%)",
                                xlim = c(0, 100), breaks = seq(0, 100, 10),
                                ylab_txt = "Pheasant\ndetection")

# 2) Woodland density (%)
df_wood <- read.csv("Detections and woodland density.csv", check.names = FALSE)
nm <- tolower(trimws(names(df_wood)))
det_col <- grep("detect|detection|detected|presence|pheasant", nm)[1]
x_col   <- grep("wood|woodland|tree|canopy|forest|density|cover", nm)[1]
if (is.na(det_col) || is.na(x_col)) stop("Missing detection or woodland column (woodland dataset).")

df_wood <- df_wood %>%
  transmute(
    detected = as.numeric(.data[[names(df_wood)[det_col]]]),
    woodland_density = clean_num(.data[[names(df_wood)[x_col]]])
  ) %>%
  filter(!is.na(detected), !is.na(woodland_density)) %>%
  mutate(detected = ifelse(detected >= 0.5, 1, 0))

p_wood_hist <- make_hist(df_wood %>% filter(detected == 1), "woodland_density",
                         "Woodland density (%)", binwidth = 5,
                         xlim = c(0, 100), breaks = seq(0, 100, 10))
p_wood_scatter <- make_scatter(df_wood, "woodland_density",
                               "Woodland density (%)",
                               xlim = c(0, 100), breaks = seq(0, 100, 10))

# 3) Tree canopy cover (%)
df_canopy_raw <- read.csv("Detections and canopy cover csv.csv", check.names = FALSE)
df_canopy <- df_canopy_raw %>%
  transmute(
    detected = as.numeric(Detections),
    canopy_cover = as.numeric(`Tree canopy cover`)
  ) %>%
  filter(!is.na(detected), !is.na(canopy_cover)) %>%
  mutate(detected = ifelse(detected >= 0.5, 1, 0))

p_canopy_hist <- make_hist(df_canopy %>% filter(detected == 1), "canopy_cover",
                           "Tree canopy cover (%)", bins = 15,
                           xlim = c(0, 100), breaks = seq(0, 100, 10))
p_canopy_scatter <- make_scatter(df_canopy, "canopy_cover",
                                 "Tree canopy cover (%)",
                                 xlim = c(0, 100), breaks = seq(0, 100, 10))

# 4) Arable land (%)
df_arable <- read.csv("Detections and percentage arable.csv", stringsAsFactors = FALSE, check.names = FALSE)
df_arable <- df_arable %>%
  mutate(
    detected = as.numeric(detections),
    arable_pct = as.numeric(`Arable area`) * 100
  ) %>%
  filter(!is.na(detected), !is.na(arable_pct)) %>%
  mutate(detected = ifelse(detected >= 0.5, 1, 0))

p_arable_hist <- make_hist(df_arable %>% filter(detected == 1), "arable_pct",
                           "Density of arable land (%)", binwidth = 5)
p_arable_scatter <- make_scatter(df_arable, "arable_pct",
                                 "Density of arable land (%)")

# 5) Distance from nearest release pen (km)
df_dist_raw <- read.csv("Detections and distance from release sites.csv", check.names = FALSE)
nm <- tolower(trimws(names(df_dist_raw)))
det_col <- grep("detect|detection|detected|presence|pheasant", nm)[1]
x_col   <- grep("distance|dist|release", nm)[1]
if (is.na(det_col) || is.na(x_col)) stop("Missing detection or distance column (distance dataset).")

df_dist <- data.frame(
  detected_raw = df_dist_raw[[det_col]],
  distance_raw = df_dist_raw[[x_col]],
  stringsAsFactors = FALSE
) %>%
  mutate(
    detected = as.numeric(detected_raw),
    detected = ifelse(detected >= 0.5, 1, 0),
    distance_km = clean_num(distance_raw) / 1000
  ) %>%
  filter(!is.na(detected), !is.na(distance_km), distance_km <= 5)

p_dist_hist <- make_hist(df_dist %>% filter(detected == 1), "distance_km",
                         "Distance from nearest release pen (km)", binwidth = 0.5,
                         xlim = c(0, 5), breaks = seq(0, 5, 1))
p_dist_scatter <- make_scatter(df_dist, "distance_km",
                               "Distance from nearest release pen (km)",
                               xlim = c(0, 5), breaks = seq(0, 5, 1))

# Patchwork
design <- "
AB
CD
EF
GH
IJ
"

final_habitat_plot <-
  p_shrub_hist + p_shrub_scatter +
  p_wood_hist + p_wood_scatter +
  p_canopy_hist + p_canopy_scatter +
  p_arable_hist + p_arable_scatter +
  p_dist_hist + p_dist_scatter +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = design)

final_habitat_plot

