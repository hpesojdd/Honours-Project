# Detection covariates (hist + detection strip)

library(ggplot2)
library(dplyr)
library(patchwork)

setwd("C:\\4th Year\\Dissertation\\Final raw plots\\Raw plot datasets\\Detection datasets")

# Theme + points
plot_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

point_layer <- geom_jitter(
  width = 0, height = 0.12,
  shape = 21, size = 1.8,
  fill = "black", colour = "white",
  stroke = 0.25, alpha = 0.7
)

clean_num <- function(x) as.numeric(gsub("[^0-9\\.-]", "", x))

make_hist <- function(df, x, xlab, bins, xlim = NULL) {
  m <- mean(df[[x]], na.rm = TRUE)
  
  p <- ggplot(df[df$detected == 1, ], aes(x = .data[[x]])) +
    geom_histogram(bins = bins, fill = "grey30", colour = "black", linewidth = 0.5) +
    geom_vline(xintercept = m, colour = "red", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = m, y = Inf, label = sprintf("Mean = %.2f", m),
             vjust = -0.4, colour = "red", size = 3.5) +
    coord_cartesian(clip = "off", xlim = xlim) +
    theme(plot.margin = margin(t = 8, r = 5, b = 5, l = 5)) +
    labs(x = xlab, y = "Count") +
    plot_theme
  
  p
}

make_scatter <- function(df, x, xlab) {
  ggplot(df, aes(x = .data[[x]], y = factor(detected))) +
    point_layer +
    labs(x = xlab, y = "Pheasant\ndetection") +
    plot_theme
}

# Days since project began
df_days <- read.csv("days since project began and detections.csv", check.names = FALSE)
nm <- tolower(trimws(names(df_days)))
det_col  <- grep("detect", nm)[1]
days_col <- grep("day.*since.*project|^days$|^day$", nm)[1]
if (is.na(det_col) || is.na(days_col)) stop("Missing detection or days column (days dataset).")

df_days <- df_days %>%
  transmute(
    detected = as.numeric(.data[[names(df_days)[det_col]]]),
    days = clean_num(.data[[names(df_days)[days_col]]])
  ) %>%
  filter(!is.na(detected), !is.na(days)) %>%
  mutate(detected = ifelse(detected >= 0.5, 1, 0))

p_days_hist <- make_hist(df_days, "days", "Days since project began", bins = 15)
p_days_scatter <- make_scatter(df_days, "days", "Days since project began")

# Temperature
df_temp <- read.csv("Detections and temperature.csv", check.names = FALSE)
names(df_temp) <- c("detected_raw", "temperature")

df_temp <- df_temp %>%
  transmute(
    detected = ifelse(as.numeric(detected_raw) >= 0.5, 1, 0),
    temperature = as.numeric(temperature)
  ) %>%
  filter(!is.na(detected), !is.na(temperature))

p_temp_hist <- make_hist(df_temp, "temperature", "Temperature (°C)", bins = 12)
p_temp_scatter <- make_scatter(df_temp, "temperature", "Temperature (°C)")

# Vegetation height
df_veg <- read.csv("Detections and vegetation height.csv", check.names = FALSE)
nm <- tolower(trimws(names(df_veg)))
det_col <- grep("detect", nm)[1]
veg_col <- grep("vegetation|veg.*height|height", nm)[1]
if (is.na(det_col) || is.na(veg_col)) stop("Missing detection or vegetation column (veg dataset).")

df_veg <- df_veg %>%
  transmute(
    detected = ifelse(as.numeric(.data[[names(df_veg)[det_col]]]) >= 0.5, 1, 0),
    vegetation_height = clean_num(.data[[names(df_veg)[veg_col]]])
  ) %>%
  filter(!is.na(detected), !is.na(vegetation_height))

p_veg_hist <- make_hist(df_veg, "vegetation_height", "Vegetation height (cm)", bins = 10)
p_veg_scatter <- make_scatter(df_veg, "vegetation_height", "Vegetation height (cm)")

# Rainfall
df_rain <- read.csv("Camera traps daily rainfall spreadsheet .csv", check.names = FALSE)
nm <- tolower(trimws(names(df_rain)))
rain_col <- grep("rain|rainfall|mm", nm)[1]
det_col  <- grep("detect", nm)[1]
if (is.na(det_col) || is.na(rain_col)) stop("Missing detection or rainfall column (rain dataset).")

df_rain <- df_rain %>%
  transmute(
    rainfall_mm = clean_num(.data[[names(df_rain)[rain_col]]]),
    detected = ifelse(as.numeric(.data[[names(df_rain)[det_col]]]) >= 0.5, 1, 0)
  ) %>%
  filter(!is.na(detected), !is.na(rainfall_mm))

p_rain_hist <- make_hist(df_rain, "rainfall_mm", "Rainfall (mm)", bins = 20, xlim = c(0, 10))
p_rain_scatter <- make_scatter(df_rain, "rainfall_mm", "Rainfall (mm)")

# Field of view
df_view <- read.csv("detections and field of view.csv", check.names = FALSE)
nm <- tolower(trimws(names(df_view)))
det_col <- grep("detect", nm)[1]
cat_col <- grep("field\\s*of\\s*view|\\bfov\\b|open|closed|category|status|state", nm)[1]
if (is.na(det_col) || is.na(cat_col)) stop("Missing detection or view column (view dataset).")

df_view <- df_view %>%
  transmute(
    detected = ifelse(as.numeric(.data[[names(df_view)[det_col]]]) >= 0.5, 1, 0),
    view_str = tolower(trimws(as.character(.data[[names(df_view)[cat_col]]])))
  ) %>%
  mutate(
    view = case_when(
      grepl("^o", view_str) ~ "Open",
      grepl("^c", view_str) ~ "Closed",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(detected), !is.na(view))

p_view_hist <- ggplot(df_view[df_view$detected == 1, ], aes(x = view)) +
  geom_bar(fill = "grey30", colour = "black", linewidth = 0.5) +
  labs(x = "Field of view", y = "Count") +
  plot_theme

p_view_scatter <- ggplot(df_view, aes(x = view, y = factor(detected))) +
  point_layer +
  labs(x = "Field of view", y = "Pheasant\ndetection") +
  plot_theme

# Patchwork
design <- "
AB
CD
EF
GH
IJ
"

final_plot <-
  p_temp_hist + p_temp_scatter +
  p_veg_hist  + p_veg_scatter +
  p_rain_hist + p_rain_scatter +
  p_view_hist + p_view_scatter +
  p_days_hist + p_days_scatter +
  plot_annotation(tag_levels = "A") +
  plot_layout(design = design)

final_plot

