# Set working directory
setwd("C:\\4th Year\\Dissertation\\Datasets for model\\Cleaned datasets")

library(spOccupancy)
library(readr)

# --- Load cleaned data ---
y       <- as.matrix(read_csv("Detections_clean.csv", show_col_types = FALSE))
occ     <- read_csv("Occupancy_covs_clean.csv", show_col_types = FALSE) |> as.data.frame()
days    <- as.matrix(read_csv("Days_clean.csv", show_col_types = FALSE))
rain    <- as.matrix(read_csv("Rainfall_clean.csv", show_col_types = FALSE))
tempv   <- as.matrix(read_csv("Temperature_clean.csv", show_col_types = FALSE))
veg     <- as.matrix(read_csv("Vegetation_height_clean.csv", show_col_types = FALSE))
view_df <- read_csv("View_clean.csv", show_col_types = FALSE)
coords  <- as.matrix(read_csv("Coordinates_clean.csv", show_col_types = FALSE)[, c("x","y")])

# --- View matrix to binary (open = 1, closed = 0) ---
view_mat <- as.matrix(view_df)
if (is.character(view_mat[1, 1]) || is.factor(view_mat[1, 1])) {
  ViewOpen <- apply(view_mat, c(1, 2), function(x) as.integer(tolower(trimws(x)) == "open"))
} else {
  ViewOpen <- apply(view_mat, c(1, 2), function(x) as.integer(x))
}

# --- Scale covariates ---
# Site-level occupancy covariates (columns = covariates, rows = sites)
occ_sc <- as.data.frame(scale(occ))

# Detection covariates: columns = days -> scale each column independently
days_sc  <- scale(days)
rain_sc  <- scale(rain)
tempv_sc <- scale(tempv)
veg_sc   <- scale(veg)
ViewOpen_sc <- ViewOpen  # keep binary 0/1 (do not scale)

# --- Formulas ---
occ_formula <- ~ arable_area + distance_from_release_pen_m + shrub_density + woodland_density + canopy_cover
det_formula <- ~ Days + Rain + Temp + VegH + ViewOpen

# --- Detection covariates list ---
det.covs <- list(
  Days     = days_sc,
  Rain     = rain_sc,
  Temp     = tempv_sc,
  VegH     = veg_sc,
  ViewOpen = ViewOpen_sc
)

# --- Bundle data ---
data_list <- list(
  y        = y,
  det.covs = det.covs,
  occ.covs = occ_sc,
  coords   = coords
)

# --- Priors ---
priors <- list(
  alpha.normal = list(mean = 0, var = 2.72),
  beta.normal  = list(mean = 0, var = 2.72),
  nu.unif      = c(0.5, 2.5)
)

# --- Fit model ---
set.seed(1988)
fit <- spPGOcc(
  occ.formula  = occ_formula,
  det.formula  = det_formula,
  data         = data_list,
  priors       = priors,
  cov.model    = "matern",
  n.chains     = 4,
  n.batch      = 200,
  batch.length = 50,
  n.burn       = 200,
  n.thin       = 1,
  verbose      = TRUE
)

summary(fit)
