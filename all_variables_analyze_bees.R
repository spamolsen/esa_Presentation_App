###############################################################
# all_variables_analyze_bees.R
# Author: Sammy Olsen 
# Date: 2025-05-29
# Description: Analysis of bee abundance using all variables -
#   model fitting, diagnostics, and output for Pan Trap and Sweep Net data.
###############################################################

# Always clear the output file at the start of each run
file.create("allvar_file_all_model_console_output.txt")

# Helper function to append output to file
append_output <- function(expr) {
  capture.output(
    expr,
    file = "allvar_file_all_model_console_output.txt",
    append = TRUE
  )
}

# ========================
# 1. PACKAGE MANAGEMENT
# ========================
# Package management is now handled by setup_environment.R
# Run source("setup_environment.R") if you haven't already

# Load required packages
library(mgcv)       # For GAM modeling
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
library(lubridate)  # For date operations
library(readxl)     # For reading Excel files
library(DHARMa)     # For model diagnostics
library(qgam)       # For quantile GAMs
library(mgcViz)     # For enhanced GAM visualization
library(gap)        # For data manipulation
library(MuMIn)      # For model selection
library(Metrics)    # For model evaluation metrics
library(caret)      # For cross-validation
library(lattice)    # For trellis graphics
library(lme4)       # For linear mixed-effects models

# Set seed for reproducibility
set.seed(123)

# ========================
# 2. DATA IMPORT & PREPARATION
# ========================
cat("\nReading and preparing data...\n")
data <- read_excel("Final_SSO_ESA_Data.xlsx")
origin_date <- as.Date("2016-03-01")

# ========================
# 3. FEATURE ENGINEERING
# ========================
data <- data %>%
  mutate(
    # Temporal variables
    sample_date = as.factor(as.Date(ReadableDate)),
    days_since_origin = as.numeric(as.Date(ReadableDate) - origin_date),
    days_since_origin_scaled = scale(days_since_origin),
    year = as.numeric(format(as.Date(ReadableDate), "%Y")),
    month = as.numeric(format(as.Date(ReadableDate), "%m")),
    year_month = as.Date(paste(year, month, "01", sep = "-")),
    season = factor(case_when(
      month %in% c(3, 4, 5) ~ "Spring",
      month %in% c(6, 7, 8) ~ "Summer",
      month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ "Winter"
    )),
    ar_start = !duplicated(sample_site),
    pu_temp = as.numeric(pu_temp),
    pu_temp_scaled = scale(pu_temp),
    pu_humidity = as.numeric(pu_humidity),
    pu_humidity_scaled = scale(pu_humidity),
    pu_windspeed = as.numeric(pu_windspeed),
    pu_windspeed_log = log1p(pu_windspeed),
    pu_windspeed_scaled = scale(pu_windspeed_log),
    pu_cloudcover = as.numeric(pu_cloudcover),
    pu_cloudcover_scaled = scale(pu_cloudcover),
    pu_precip = as.numeric(pu_precip),
    pu_precip_scaled = scale(pu_precip),
    pu_uvindex = as.numeric(pu_uvindex),
    pu_uvindex_scaled = scale(pu_uvindex),
    pu_windspeed = as.numeric(pu_windspeed),
    pu_windspeed_log = log1p(pu_windspeed),
    pu_windspeed_scaled = scale(pu_windspeed_log),
    pu_windgust = as.numeric(pu_windgust),
    pu_windgust_log = log1p(pu_windgust),
    pu_windgust_scaled = scale(pu_windgust_log),
    pu_tempmax = as.numeric(pu_tempmax),
    pu_tempmax_scaled = scale(pu_tempmax),
    pu_tempmin = as.numeric(pu_tempmin),
    pu_tempmin_scaled = scale(pu_tempmin),
    pu_feelslikemax = as.numeric(pu_feelslikemax),
    pu_feelslikemax_scaled = scale(pu_feelslikemax),
    pu_feelslikemin = as.numeric(pu_feelslikemin),
    pu_feelslikemin_scaled = scale(pu_feelslikemin),
    pu_feelslike = as.numeric(pu_feelslike),
    pu_feelslike_scaled = scale(pu_feelslike),
    pu_dew = as.numeric(pu_dew),
    pu_dew_scaled = scale(pu_dew),
    pu_precipprob = as.numeric(pu_precipprob),
    pu_precipcover = as.numeric(pu_precipcover),
    pu_precipcover_scaled = scale(pu_precipcover),
    pu_snow = as.numeric(pu_snow),
    pu_snow_scaled = scale(pu_snow),
    pu_snowdepth = as.numeric(pu_snowdepth),
    pu_snowdepth_scaled = scale(pu_snowdepth),
    pu_windgust = as.numeric(pu_windgust),
    pu_windgust_log = log1p(pu_windgust),
    pu_windgust_scaled = scale(pu_windgust_log),
    pu_winddir = as.numeric(pu_winddir),
    pu_winddir_scaled = scale(pu_winddir),
    pu_winddir_sin = sin(pi / 180 * pu_winddir),
    pu_winddir_cos = cos(pi / 180 * pu_winddir),
    pu_winddir_sin_scaled = scale(pu_winddir_sin),
    pu_winddir_cos_scaled = scale(pu_winddir_cos),
    pu_pressure = as.numeric(pu_pressure),
    pu_pressure_scaled = scale(pu_pressure),
    pu_visibility = as.numeric(pu_visibility),
    pu_visibility_scaled = scale(pu_visibility),
    pu_solarradiation = as.numeric(pu_solarradiation),
    pu_solarradiation_scaled = scale(pu_solarradiation),
    pu_solarenergy = as.numeric(pu_solarenergy),
    pu_solarenergy_scaled = scale(pu_solarenergy),
    pu_moonphase = as.numeric(pu_moonphase),
    pu_moonphase_scaled = scale(pu_moonphase),
    pd_temp = as.numeric(pd_temp),
    pd_temp_scaled = scale(pd_temp),
    pd_tempmax = as.numeric(pd_tempmax),
    pd_tempmax_scaled = scale(pd_tempmax),
    pd_tempmin_night_before = as.numeric(pd_tempmin_night_before),
    pd_tempmin_night_before_scaled = scale(pd_tempmin_night_before),
    pd_feelslikemax = as.numeric(pd_feelslikemax),
    pd_feelslikemax_scaled = scale(pd_feelslikemax),
    pd_feelslikemin = as.numeric(pd_feelslikemin),
    pd_feelslikemin_scaled = scale(pd_feelslikemin),
    temp_diff = pu_temp - pd_temp,
    temp_diff_scaled = scale(temp_diff),
    sample_site = as.factor(sample_site)
  )

# Extend data transformations to include pd_ variables and extra predictors
data <- data %>%
  mutate(
    pd_temp = as.numeric(pd_temp),
    pd_temp_scaled = scale(pd_temp),
    temp_diff = pu_temp - pd_temp,
    temp_diff_scaled = scale(temp_diff),
    pu_precip = as.numeric(pu_precip),
    pu_precip_scaled = scale(pu_precip),
    pu_uvindex = as.numeric(pu_uvindex),
    pu_uvindex_scaled = scale(pu_uvindex)
  )


# ========================
# 4. FUNCTION DEFINITIONS
# ========================

# Helper to get average derivative for a model smooth
get_avg_derivative <- function(model) {
  deriv <- gratia::derivatives(model, select = "s(days_since_origin_scaled)")
  mean(deriv$.derivative, na.rm = TRUE)
}

# Function to calculate ACF for each site
temp_acf <- function(data, abundance_var) {
  data %>%
    group_by(sample_site) %>% # nolint: object_usage_linter.
    summarise(acf_lag1 = acf(get(abundance_var), plot = FALSE)$acf[2])
}

# Model summary function
summarize_model <- function(model, data, response) {
  dev_exp <- summary(model)$dev.expl
  adj_r2 <- summary(model)$r.sq
  aic <- AIC(model)
  pred <- predict(model, type = "response")
  obs <- data[[response]]
  mse <- Metrics::mse(obs, pred)
  rmse <- Metrics::rmse(obs, pred)
  n <- nrow(data)
  list(
    deviance_expl = dev_exp,
    adj_r2 = adj_r2,
    AIC = aic,
    MSE = mse,
    RMSE = rmse,
    n = n
  )
}

# GAM smooth slope extraction
get_gam_linear_slope <- function(model) {
  terms_matrix <- mgcv::predict.gam(model, type = "terms")
  smooth_col <- "s(days_since_origin_scaled)"
  if (smooth_col %in% colnames(terms_matrix)) {
    x <- model$model[[ # nolint: object_usage_linter.
      "days_since_origin_scaled"
    ]] # nolint: object_usage_linter.
    fx <- terms_matrix[, smooth_col] # nolint: object_usage_linter.
    coef(lm(fx ~ x))[2]
  } else {
    NA
  }
}

# GAM metrics helper
gam_metrics <- function(model, data, response) {
  dev_exp <- 1 - model$deviance / model$null.deviance
  pred <- predict(model, type = "response")
  obs <- data[[response]]
  mse <- mean((obs - pred)^2, na.rm = TRUE)
  rmse <- sqrt(mse)
  list(dev_exp = dev_exp, mse = mse, rmse = rmse)
}

# 5-fold CV helper
cv_5fold_mse <- function(model_formula, data, response) {
  folds <- createFolds(data[[response]], k = 5)
  mse_vec <- c()
  for (i in 1:5) {
    train_idx <- unlist(folds[-i])
    test_idx <- unlist(folds[i])
    train <- data[train_idx, ]
    test <- data[test_idx, ]
    m <- bam(
      model_formula,
      family = nb(),
      method = "fREML",
      discrete = TRUE,
      data = train
    )
    pred <- predict(m, newdata = test, type = "response")
    mse_vec <- c(mse_vec, mean((test[[response]] - pred)^2, na.rm = TRUE))
  }
  mean(mse_vec, na.rm = TRUE)
}

# GAM trend plot
gam_trend_plot <- function(model, method_name) {
  # Get smooth estimates using gratia package
  p <- gratia::smooth_estimates(
    model,
    smooth = "s(days_since_origin_scaled)",
    n = 100
  ) %>%
    gratia::add_confint() %>%
    gratia::draw() +
    labs(
      title = paste(method_name, "Trend Over Time"),
      x = "Scaled Days Since Origin",
      y = "Smooth Term"
    ) +
    theme_minimal()
  
  return(p)
}

# ========================
# 5. TEMPORAL AUTOCORRELATION
# ========================
cat("\nCalculating temporal autocorrelation for Pan Traps...\n")
pan_acf <- temp_acf(data, "Pan_trap_abundance")
print(paste(
  "Mean ACF at lag 1 for Pan Traps:",
  mean(pan_acf$acf_lag1, na.rm = TRUE)
))

cat("\nCalculating temporal autocorrelation for Sweep Nets...\n")
sweep_acf <- temp_acf(data, "Sweepnet_abundance")
print(paste(
  "Mean ACF at lag 1 for Sweep Nets:",
  mean(sweep_acf$acf_lag1, na.rm = TRUE)
))

# ========================
# 6. MODEL FITTING & ANALYSIS
# ========================

# Pan Trap Model
allvar_pan_vars <- c(
  "Pan_trap_abundance",
  "days_since_origin_scaled",
  "month",
  "year",
  "sample_date",
  "sample_site",
  "pu_temp_scaled",
  "pd_temp_scaled",
  "pu_humidity_scaled",
  "pu_cloudcover_scaled",
  "temp_diff_scaled",
  "pu_windspeed_scaled",
  "pu_dew",
  "pu_pressure",
  "pu_visibility"
)
allvar_pan_data <- na.omit(data[, unique(allvar_pan_vars)])

bam_pan_allvar <- bam(
  Pan_trap_abundance ~
    s(days_since_origin_scaled, bs = "cr") +
      s(month, bs = "cc", k = 7) +
      s(year, bs = "re") +
      s(sample_date, bs = "re") +
      s(sample_site, bs = "re") +
      te(pu_temp_scaled, pd_temp_scaled, bs = c("cr", "cr"), k = c(5, 5)) +
      te(
        pu_humidity_scaled,
        pu_cloudcover_scaled,
        bs = c("cr", "cr"),
        k = c(6, 6)
      ) +
      temp_diff_scaled + pu_windspeed_scaled +
      s(pu_dew, bs = "cr") +
      s(pu_pressure, bs = "cr", k = 3) +
      s(pu_visibility, bs = "cr", k = 3),
  family = nb(),
  method = "fREML",
  discrete = TRUE,
  data = allvar_pan_data
)

# Sweep Net Model
allvar_sweep_vars <- c(
  "Sweepnet_abundance",
  "days_since_origin_scaled",
  "month",
  "year",
  "sample_date",
  "sample_site",
  "pu_temp_scaled",
  "pd_temp_scaled",
  "pu_humidity_scaled",
  "pu_cloudcover_scaled",
  "temp_diff_scaled",
  "pu_windspeed_scaled",
  "pu_dew",
  "pu_pressure",
  "pu_visibility"
)

allvar_sweep_data <- na.omit(data[, unique(allvar_sweep_vars)])

bam_sweep_allvar <- bam(
  Sweepnet_abundance ~
    s(days_since_origin_scaled, bs = "cr") +
      s(month, bs = "cc", k = 7) +
      s(year, bs = "re") +
      s(sample_date, bs = "re") +
      s(sample_site, bs = "re") +
      te(pu_temp_scaled, pd_temp_scaled, bs = c("cr", "cr"), k = c(5, 5)) +
      te(
        pu_humidity_scaled,
        pu_cloudcover_scaled,
        bs = c("cr", "cr"),
        k = c(6, 6)
      ) +
      temp_diff_scaled + pu_windspeed_scaled +
      s(pu_dew, bs = "cr") +
      s(pu_pressure, bs = "cr", k = 3) +
      s(pu_visibility, bs = "cr", k = 3),
  family = nb(),
  method = "fREML",
  discrete = TRUE,
  data = allvar_sweep_data
)

# Create contour plot for humidity and cloud interaction
png("plots/humidity_cloud_interaction.png", width = 1200, height = 800, res = 300)

# Get actual data ranges
humidity_range <- range(allvar_sweep_data$pu_humidity_scaled, na.rm = TRUE)
cloud_range <- range(allvar_sweep_data$pu_cloudcover_scaled, na.rm = TRUE)

# Create grid of values for prediction
humidity_grid <- seq(humidity_range[1], humidity_range[2], length.out = 100)
cloud_grid <- seq(cloud_range[1], cloud_range[2], length.out = 100)

# Create data frame for prediction
pred_data <- expand.grid(
  pu_humidity_scaled = humidity_grid,
  pu_cloudcover_scaled = cloud_grid
)

# Add other variables at their mean values
pred_data$days_since_origin_scaled <- mean(allvar_sweep_data$days_since_origin_scaled)
pred_data$month <- mean(allvar_sweep_data$month)
pred_data$year <- mean(allvar_sweep_data$year)
pred_data$sample_date <- factor("1")
pred_data$sample_site <- factor("1")
pred_data$pu_temp_scaled <- mean(allvar_sweep_data$pu_temp_scaled)
pred_data$pd_temp_scaled <- mean(allvar_sweep_data$pd_temp_scaled)
pred_data$temp_diff_scaled <- mean(allvar_sweep_data$temp_diff_scaled)
pred_data$pu_windspeed_scaled <- mean(allvar_sweep_data$pu_windspeed_scaled)
pred_data$pu_dew <- mean(allvar_sweep_data$pu_dew)
pred_data$pu_pressure <- mean(allvar_sweep_data$pu_pressure)
pred_data$pu_visibility <- mean(allvar_sweep_data$pu_visibility)

# Get predictions
pred <- predict(bam_sweep_allvar, newdata = pred_data, type = "response")

# Add predictions to data frame
pred_data$abundance <- pred

# Create filled contour plot
filled.contour(
  x = humidity_grid, y = cloud_grid, z = matrix(pred, nrow = 100),
  color.palette = colorRampPalette(c("white", "blue", "green", "yellow", "red")),
  main = "Humidity and Cloud Cover Interaction",
  xlab = "Humidity (scaled)",
  ylab = "Cloud Cover (scaled)",
  key.title = title(main = "Bee Abundance"),
  key.axes = axis(4, cex.axis = 0.8),
  plot.axes = {
    par(new = TRUE)
    contour(x = humidity_grid, y = cloud_grid, z = matrix(pred, nrow = 100),
            add = TRUE, col = "white", lwd = 0.5)
  }
)

dev.off()



# Define model_list for AIC and variable importance calculations
model_list <- list(
  bam_pan_allvar = bam_pan_allvar,
  bam_sweep_allvar = bam_sweep_allvar
)
# Calculate AIC and Akaike weights
model_aic <- sapply(model_list, AIC)
akaike_weights <- MuMIn::Weights(model_aic)

# Get variables used in each model
model_vars <- lapply(model_list, function(m) all.vars(formula(m)))
all_vars <- unique(unlist(model_vars))

# Variable importance (I) as sum of weights for models containing each variable
var_importance <- sapply(all_vars, function(v) {
  sum(akaike_weights[sapply(model_vars, function(vars) v %in% vars)])
})

# Prepare output table
importance_table <- data.frame(
  Variable = names(var_importance),
  Importance = round(var_importance, 3)
)
importance_table <- importance_table[order(-importance_table$Importance), ]


# ---- Output summaries for all models ----
library(Metrics)
cat("\n============================\n")
cat("Model Summary Table\n")
cat("============================\n")


bam_pan_sum  <- summarize_model(
  bam_pan_allvar, allvar_pan_data, "Pan_trap_abundance"
)
bam_sweep_sum  <- summarize_model(
  bam_sweep_allvar, allvar_sweep_data, "Sweepnet_abundance"
)

cat(sprintf(
  "\n%-25s %-10s %-10s %-10s %-10s %-10s\n",
  "Model", "DevExp", "AdjR2", "AIC", "RMSE", "n"
))
cat("-------------------------------------------------------------------\n")

# ---- Print edf for s(days_since_origin_scaled) for all models ----
pan_red_edf <- summary(
  bam_pan_allvar
)$s.table["s(days_since_origin_scaled)", "edf"]
sweep_red_edf <- summary(
  bam_sweep_allvar
)$s.table["s(days_since_origin_scaled)", "edf"]
cat("\nEffective degrees of freedom for s(days_since_origin_scaled):\n")
cat(sprintf("Pan Trap Reduced:  %.3f\n", pan_red_edf))
cat(sprintf("Sweep Net Reduced: %.3f\n", sweep_red_edf))

# ---- Print slope for s(days_since_origin_scaled) when edf == 1 ----
get_gam_linear_slope <- function(model) {
  terms_matrix <- mgcv::predict.gam(model, type = "terms")
  smooth_col <- "s(days_since_origin_scaled)"
  if (smooth_col %in% colnames(terms_matrix)) {
    coef(
      lm(
        terms_matrix[, smooth_col] ~ model$model[["days_since_origin_scaled"]]
      )
    )[2]
  } else {
    NA
  }
}
cat(
  "\nS lope for s(days_since_origin_scaled) where edf == 1 (from GAM smooth):\n"
)
if (!is.null(pan_red_edf) &&
      abs(as.numeric(trimws(as.character(pan_red_edf))) - 1) < 1e-4) {
  slope <- get_gam_linear_slope(bam_pan_allvar)
  cat(sprintf("Pan Trap Reduced:  %.4f\n", slope))
} else {
  cat("Pan Trap Reduced:  nonlinear,\n",
      "slope not defined\n")
}
if (!is.null(sweep_red_edf) &&
      abs(as.numeric(trimws(as.character(sweep_red_edf))) - 1) < 1e-4) {
  slope <- get_gam_linear_slope(bam_sweep_allvar)
  cat(sprintf("Sweep Net Reduced: %.4f\n", slope))
} else {
  cat("Sweep Net Reduced: nonlinear,\n",
      "slope not defined\n")
}


cat("\nAverage derivatives for s(days_since_origin_scaled):\n")
if (!is.null(pan_red_edf)) {
  avg_deriv <- get_avg_derivative(bam_pan_allvar)
  cat(sprintf("Pan Trap Reduced:  %.4f\n", avg_deriv))
}
if (!is.null(sweep_red_edf)) {
  avg_deriv <- get_avg_derivative(bam_sweep_allvar)
  cat(sprintf("Sweep Net Reduced: %.4f\n", avg_deriv))
}

# Plot trends for all four models
gam_trend_plot(bam_pan_allvar, "Pan Trap Reduced")
gam_trend_plot(bam_sweep_allvar, "Sweep Net Reduced")


# Overwrite output file at the start of each run
unlink("allvar_file_all_model_console_output.txt")

# Write first output with append=FALSE to create new file
capture.output(
  "\nPan Trap Reduced Model Summary:\n",
  file = "allvar_file_all_model_console_output.txt",
  append = FALSE
)
capture.output(
  summary(bam_pan_allvar),
  file = "allvar_file_all_model_console_output.txt",
  append = TRUE
)

capture.output(
  "\nSweep Net Model Summary:\n",
  file = "allvar_file_all_model_console_output.txt",
  append = TRUE
)
capture.output(
  summary(bam_sweep_allvar),
  file = "allvar_file_all_model_console_output.txt",
  append = TRUE
)





# DHARMa residual diagnostics for all four models
cat("\n============================\n")
cat("DHARMa Residual Diagnostics\n")
cat("============================\n")

# Generate and save diagnostic plots for each model
cat("\nGenerating Pan Trap diagnostic plots...\n")
pdf("pan_trap_diagnostics.pdf", width = 12, height = 8)
pan_resids <- simulateResiduals(bam_pan_allvar)
plot(pan_resids)
testResiduals(pan_resids)
dev.off()
cat("Pan Trap diagnostics saved to: pan_trap_diagnostics.pdf\n")

cat("\nGenerating Sweep Net diagnostic plots...\n")
pdf("sweep_net_diagnostics.pdf", width = 12, height = 8)
sweep_resids <- simulateResiduals(bam_sweep_allvar)
plot(sweep_resids)
testResiduals(sweep_resids)
dev.off()
cat("Sweep Net diagnostics saved to: sweep_net_diagnostics.pdf\n")


cat("\n============================\n")
cat("Model AIC Values\n")
cat("============================\n")
cat(sprintf("Pan Trap:  AIC = %.1f\n", AIC(bam_pan_allvar)))
cat(sprintf("Sweep Net: AIC = %.1f\n", AIC(bam_sweep_allvar)))

cat("\n============================\n")
cat("Model RMSE Values\n")
cat("============================\n")
cat(sprintf("Pan Trap:  RMSE = %.3f\n", bam_pan_allvar$rmse))
cat(sprintf("Sweep Net: RMSE = %.3f\n", bam_sweep_allvar$rmse))


# 5-fold CV for all four models
cat("\n============================\n")
cat("5-fold Cross-Validation Mean MSE\n")
cat("============================\n")


cat(sprintf(
  "Pan Trap:  5-fold CV mean MSE = %.3f\n",
  cv_5fold_mse(
    formula(bam_pan_allvar),
    allvar_pan_data,
    "Pan_trap_abundance"
  )
))

# ========================
# 8. CREATE ABUNDANCE TIME SERIES PLOTS
# ========================
cat("\nCreating abundance time series plots...\n")

# Create directory for plots if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Function to create and save time series plot for a given method
create_abundance_plot <- function(data, method, method_name, color) {
  # Summarize data by date
  summary_data <- data %>%
    group_by(ReadableDate) %>%
    summarise(
      mean_abundance = mean(!!sym(paste0(method, "_abundance")), na.rm = TRUE),
      se_abundance = sd(!!sym(paste0(method, "_abundance")), na.rm = TRUE) / sqrt(n())
    )
  
  # Create plot
  p <- ggplot(summary_data, aes(x = as.Date(ReadableDate), y = mean_abundance)) +
    geom_line(color = color, linewidth = 1) +
    geom_ribbon(aes(ymin = pmax(0, mean_abundance - 1.96 * se_abundance),
                    ymax = mean_abundance + 1.96 * se_abundance),
                alpha = 0.2, fill = color) +
    geom_point(color = color, linewidth = 2) +
    labs(
      title = paste(method_name, "Abundance Over Time"),
      x = "Date",
      y = paste(method_name, "Abundance (mean ± 95% CI)")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
  
  # Save plot
  ggsave(
    filename = file.path("plots", paste0(tolower(gsub(" ", "_", method)), "_abundance_time_series.png")),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  return(p)
}

# Function to extract and prepare smooth data from GAM models
extract_smooth_data <- function(model, var_name = "days_since_origin_scaled") {
  # Create a prediction data frame
  pred_data <- data.frame(
    days_since_origin_scaled = seq(
      min(model$model[[var_name]]), 
      max(model$model[[var_name]]), 
      length.out = 200
    )
  )
  
  # Add other required variables with their mean/median values
  model_terms <- attr(terms(model), "term.labels")
  model_data <- model$model
  
  for (term in model_terms) {
    if (!grepl("s\\(|te\\(|\\)", term) && term != var_name) {
      if (is.numeric(model_data[[term]])) {
        pred_data[[term]] <- median(model_data[[term]], na.rm = TRUE)
      } else if (is.factor(model_data[[term]])) {
        # For factors, use the most common level
        pred_data[[term]] <- names(sort(table(model_data[[term]]), decreasing = TRUE))[1]
      }
    }
  }
  
  # Generate predictions
  pred <- predict(model, newdata = pred_data, type = "link", se.fit = TRUE)
  
  # Convert to response scale and combine with prediction data
  pred_df <- data.frame(
    days_since_origin_scaled = pred_data[[var_name]],
    fit = exp(pred$fit),
    se = exp(pred$fit) * pred$se.fit,  # Approximate SE on response scale
    upper = exp(pred$fit + 1.96 * pred$se.fit),
    lower = exp(pmax(pred$fit - 1.96 * pred$se.fit, -20))  # Prevent extreme lower bounds
  )
  
  return(pred_df)
}

# Function to create and save time series plot with GAM smooth
create_abundance_plot <- function(data, method, method_name, color, model) {
  # Summarize data by date
  summary_data <- data %>%
    group_by(ReadableDate) %>%
    summarise(
      mean_abundance = mean(!!sym(paste0(method, "_abundance")), na.rm = TRUE),
      se_abundance = sd(!!sym(paste0(method, "_abundance")), na.rm = TRUE) / sqrt(n()),
      days_since_origin_scaled = mean(days_since_origin_scaled, na.rm = TRUE)
    )
  
  # Extract GAM smooth for days_since_origin_scaled
  smooth_data <- extract_smooth_data(model)
  
  # Scale the smooth to match the observed range
  obs_range <- range(summary_data$mean_abundance, na.rm = TRUE)
  smooth_range <- range(smooth_data$fit)
  scale_factor <- diff(obs_range) / diff(smooth_range)
  smooth_data$fit_scaled <- (smooth_data$fit - min(smooth_data$fit)) * scale_factor + min(obs_range)
  smooth_data$upper_scaled <- (smooth_data$upper - min(smooth_data$fit)) * scale_factor + min(obs_range)
  smooth_data$lower_scaled <- (smooth_data$lower - min(smooth_data$fit)) * scale_factor + min(obs_range)
  
  # Create plot with individual points
  p <- ggplot() +
    # Individual data points in black
    geom_point(
      data = data,
      aes(x = as.Date(ReadableDate), y = !!sym(paste0(method, "_abundance"))),
      color = "black", size = 1.5, alpha = 0.6
    ) +
    
    # Add GAM smooth (scaled to match y-axis) as solid royal blue line
    geom_line(
      data = {
        date_range <- as.numeric(range(as.Date(data$ReadableDate)))
        smooth_range <- range(smooth_data$days_since_origin_scaled)
        smooth_df <- data.frame(
          ReadableDate = as.Date(
            as.numeric(as.Date(data$ReadableDate[1])) + 
            (smooth_data$days_since_origin_scaled - min(smooth_range)) * 
            diff(date_range) / diff(smooth_range),
            origin = "1970-01-01"
          ),
          fit = smooth_data$fit_scaled,
          ymin = smooth_data$lower_scaled,
          ymax = smooth_data$upper_scaled
        )
        smooth_df
      },
      aes(x = ReadableDate, y = fit),
      color = "royalblue", linewidth = 1.2
    ) +

    # Add confidence interval for the smooth
    geom_ribbon(
      data = {
        date_range <- as.numeric(range(as.Date(data$ReadableDate)))
        smooth_range <- range(smooth_data$days_since_origin_scaled)
        ribbon_df <- data.frame(
          ReadableDate = as.Date(
            as.numeric(as.Date(data$ReadableDate[1])) + 
            (smooth_data$days_since_origin_scaled - min(smooth_range)) * 
            diff(date_range) / diff(smooth_range),
            origin = "1970-01-01"
          ),
          ymin = smooth_data$lower_scaled,
          ymax = smooth_data$upper_scaled
        )
        ribbon_df
      },
      aes(x = ReadableDate, ymin = ymin, ymax = ymax),
      fill = "royalblue", alpha = 0.2
    ) +

    labs(
      title = paste(method_name, "Abundance Over Time"),
      x = "Year",
      y = paste(method_name, "Abundance")
    ) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 24, color = "black", margin = margin(b = 15)),
      axis.title = element_text(size = 20, color = "black", face = "bold"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 16, color = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", vjust = 1, margin = margin(t = 5)),
      axis.text.y = element_text(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    ) +
    scale_x_date(
      date_labels = "%Y",
      expand = expansion(mult = 0.02),
      date_minor_breaks = "6 months",
      breaks = function(x) {
        # Place breaks at the middle of each year
        year_starts <- seq(floor_date(min(x), "year"), 
                         ceiling_date(max(x), "year") - months(1), 
                         by = "1 year")
        year_middles <- year_starts + months(6)
        return(year_middles[year_middles >= min(x) & year_middles <= max(x)])
      }
    )
  
  # Save plot with wider dimensions
  plot_file <- file.path("plots", paste0(tolower(gsub(" ", "_", method_name)), "_abundance_time_series.png"))
  if (!dir.exists("plots")) dir.create("plots")
  ggsave(
    filename = plot_file,
    plot = p,
    width = 16,      # Increased from 12 to 16 for wider plot
    height = 8,      # Slightly increased height to maintain aspect ratio
    dpi = 300,
    units = "in"     # Explicitly set units to inches
  )
  
  return(p)
}

pan_plot <- create_abundance_plot(data, "Pan_trap", "Pan Trap", "#1f77b4", bam_pan_allvar)
sweep_plot <- create_abundance_plot(data, "Sweepnet", "Sweep Net", "#ff7f0e", bam_sweep_allvar)

cat("\nAbundance time series plots with GAM smooths saved to the 'plots' directory.\n")
cat(sprintf(
  "Sweep Net: 5-fold CV mean MSE = %.3f\n",
  cv_5fold_mse(
    formula(bam_sweep_allvar),
    allvar_sweep_data,
    "Sweepnet_abundance"
  )
))

# Function to create marginal effects plots for tensor product terms
create_marginal_effects_plot <- function(model, data, var1, var2, method_name, color) {
  # Create plot directory if it doesn't exist
  if (!dir.exists("plots")) {
    dir.create("plots")
  }
  
  # Create grid of values for prediction
  var1_range <- range(data[[var1]], na.rm = TRUE)
  var2_range <- range(data[[var2]], na.rm = TRUE)
  
  # Create data frame for prediction
  var1_values <- seq(var1_range[1], var1_range[2], length.out = 100)
  var2_values <- seq(var2_range[1], var2_range[2], length.out = 100)
  pred_data <- expand.grid(
    var1 = var1_values,
    var2 = var2_values
  )
  
  # Rename columns to match variable names
  names(pred_data) <- c(var1, var2)
  
  # Add other variables at their mean values
  model_data <- model$model
  model_vars <- all.vars(formula(model))
  
  # Create list of all variables needed
  required_vars <- unique(c(
    all.vars(terms(model)),
    all.vars(attr(terms(model), "factors"))
  ))
  
  for (var in required_vars) {
    if (!var %in% c(var1, var2)) {
      if (var %in% names(model_data)) {
        if (is.numeric(model_data[[var]])) {
          pred_data[[var]] <- mean(model_data[[var]], na.rm = TRUE)
        } else if (is.factor(model_data[[var]])) {
          pred_data[[var]] <- levels(model_data[[var]])[1]  # Use first level of factor
        } else {
          pred_data[[var]] <- model_data[[var]][1]  # Use first value for other types
        }
      }
    }
  }
  
  # Get predictions
  pred <- predict(model, newdata = pred_data, type = "response")
  pred_data$abundance <- pred
  
  # Create marginal effects plots
  p1 <- ggplot() +
    geom_line(
      data = pred_data %>% group_by(!!sym(var1)) %>% summarize(mean_pred = mean(abundance)),
      aes(x = !!sym(var1), y = mean_pred),
      color = color,
      linewidth = 1.2
    ) +
    labs(
      title = paste(method_name, "Marginal Effect of", sub("_scaled", "", var1)),
      x = sub("_scaled", "", var1),
      y = "Marginal Effect"
    ) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 24, color = "black", margin = margin(b = 15)),
      axis.title = element_text(size = 20, color = "black", face = "bold"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 16, color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", vjust = 1, margin = margin(t = 5)),
      axis.text.y = element_text(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
  
  p2 <- ggplot() +
    geom_line(
      data = pred_data %>% group_by(!!sym(var2)) %>% summarize(mean_pred = mean(abundance)),
      aes(x = !!sym(var2), y = mean_pred),
      color = color,
      linewidth = 1.2
    ) +
    labs(
      title = paste(method_name, "Marginal Effect of", sub("_scaled", "", var2)),
      x = sub("_scaled", "", var2),
      y = "Marginal Effect"
    ) +
    theme_minimal(base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 24, color = "black", margin = margin(b = 15)),
      axis.title = element_text(size = 20, color = "black", face = "bold"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      axis.text = element_text(size = 16, color = "black"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", vjust = 1, margin = margin(t = 5)),
      axis.text.y = element_text(color = "black"),
      axis.ticks = element_line(color = "black"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
  
  # Save plots
  ggsave(
    filename = file.path("plots", paste0(tolower(gsub(" ", "_", method_name)), "_", var1, "_marginal_effect.png")),
    plot = p1,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  ggsave(
    filename = file.path("plots", paste0(tolower(gsub(" ", "_", method_name)), "_", var2, "_marginal_effect.png")),
    plot = p2,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  return(list(var1_plot = p1, var2_plot = p2))
}

# Create marginal effects plots for temperature tensor product
pan_temp_plots <- create_marginal_effects_plot(
  bam_pan_allvar, allvar_pan_data, "pu_temp_scaled", "pd_temp_scaled",
  "Pan Trap", "#1f77b4"
)
sweep_temp_plots <- create_marginal_effects_plot(
  bam_sweep_allvar, allvar_sweep_data, "pu_temp_scaled", "pd_temp_scaled",
  "Sweep Net", "#ff7f0e"
)

# --- ICC and Likelihood Ratio Tests ---
append_output({
  cat("\n============================\n")
  cat("ICC and Likelihood Ratio Tests\n")
  cat("============================\n")

  # Pan Trap
  cat("\n--- PAN TRAP ---\n")
  # sample_site
  icc_model_pan_site <- lmer(
    Pan_trap_abundance ~ 1 + (1 | sample_site),
    data = allvar_pan_data
  )
  varcomp_pan_site <- as.data.frame(VarCorr(icc_model_pan_site))
  icc_pan_site <- varcomp_pan_site$vcov[1] / sum(varcomp_pan_site$vcov)
  cat(sprintf(
    "ICC for Pan_trap_abundance by sample_site: %.3f\n",
    icc_pan_site
  ))
  # year
  icc_model_pan_year <- lmer(
    Pan_trap_abundance ~ 1 + (1 | year),
    data = allvar_pan_data
  )
  varcomp_pan_year <- as.data.frame(VarCorr(icc_model_pan_year))
  icc_pan_year <- varcomp_pan_year$vcov[1] / sum(varcomp_pan_year$vcov)
  cat(sprintf(
    "ICC for Pan_trap_abundance by year: %.3f\n",
    icc_pan_year
  ))
  # month
  icc_model_pan_month <- lmer(
    Pan_trap_abundance ~ 1 + (1 | month),
    data = allvar_pan_data
  )
  varcomp_pan_month <- as.data.frame(VarCorr(icc_model_pan_month))
  icc_pan_month <- varcomp_pan_month$vcov[1] / sum(varcomp_pan_month$vcov)
  cat(sprintf(
    "ICC for Pan_trap_abundance by month: %.3f\n",
    icc_pan_month
  ))
  # LRTs
  cat("Likelihood ratio test for sample_site (Pan_trap_abundance):\n")
  print(anova(
    icc_model_pan_site,
    lm(Pan_trap_abundance ~ 1, data = allvar_pan_data)
  ))
  cat("Likelihood ratio test for year (Pan_trap_abundance):\n")
  print(anova(
    icc_model_pan_year,
    lm(Pan_trap_abundance ~ 1, data = allvar_pan_data)
  ))
  cat("Likelihood ratio test for month (Pan_trap_abundance):\n")
  print(anova(
    icc_model_pan_month,
    lm(Pan_trap_abundance ~ 1, data = allvar_pan_data)
  ))

  # Sweep Net
  cat("\n--- SWEEP NET ---\n")
  # sample_site
  icc_model_sweep_site <- lmer(
    Sweepnet_abundance ~ 1 + (1 | sample_site),
    data = allvar_sweep_data
  )
  varcomp_sweep_site <- as.data.frame(VarCorr(icc_model_sweep_site))
  icc_sweep_site <- varcomp_sweep_site$vcov[1] / sum(varcomp_sweep_site$vcov)
  cat(sprintf(
    "ICC for Sweepnet_abundance by sample_site: %.3f\n",
    icc_sweep_site
  ))
  # year
  icc_model_sweep_year <- lmer(
    Sweepnet_abundance ~ 1 + (1 | year),
    data = allvar_sweep_data
  )
  varcomp_sweep_year <- as.data.frame(VarCorr(icc_model_sweep_year))
  icc_sweep_year <- varcomp_sweep_year$vcov[1] / sum(varcomp_sweep_year$vcov)
  cat(sprintf(
    "ICC for Sweepnet_abundance by year: %.3f\n",
    icc_sweep_year
  ))
  # month
  icc_model_sweep_month <- lmer(
    Sweepnet_abundance ~ 1 + (1 | month),
    data = allvar_sweep_data
  )
  varcomp_sweep_month <- as.data.frame(VarCorr(icc_model_sweep_month))
  icc_sweep_month <- varcomp_sweep_month$vcov[1] / sum(varcomp_sweep_month$vcov)
  cat(sprintf(
    "ICC for Sweepnet_abundance by month: %.3f\n",
    icc_sweep_month
  ))
  # LRTs
  cat("Likelihood ratio test for sample_site (Sweepnet_abundance):\n")
  print(anova(
    icc_model_sweep_site,
    lm(Sweepnet_abundance ~ 1, data = allvar_sweep_data)
  ))
  cat("Likelihood ratio test for year (Sweepnet_abundance):\n")
  print(anova(
    icc_model_sweep_year,
    lm(Sweepnet_abundance ~ 1, data = allvar_sweep_data)
  ))
  cat("Likelihood ratio test for month (Sweepnet_abundance):\n")
  print(anova(
    icc_model_sweep_month,
    lm(Sweepnet_abundance ~ 1, data = allvar_sweep_data)
  ))
})

# --- Save models and objects for publication ---
save(
  bam_pan_allvar,
  bam_sweep_allvar,
  pan_resids,
  sweep_resids,
  allvar_pan_data,
  allvar_sweep_data,
  file = "publication_models.RData"
)