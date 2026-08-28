# -------------------------------
# LOAD LIBRARIES
# -------------------------------
library(mgcv)
library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra)

# -------------------------------
# READ DATA
# -------------------------------
data_file <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv"
df <- read_csv(data_file)

# -------------------------------
# CREATE OUTPUT FOLDER
# -------------------------------
output_folder <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/GAM_Output"
if(!dir.exists(output_folder)) dir.create(output_folder)

# -------------------------------
# RENAME VARIABLES (PHYSICAL MEANING)
# -------------------------------
df <- df %>%
  rename(
    utci_change_C        = weighted_utci_slope,
    urban_expansion      = urban_slope,
    precipitation_change = precipitation_slope,
    gdp_change           = gdp_slope
  )

# -------------------------------
# CLEAN DATA
# -------------------------------
df <- df %>%
  select(utci_change_C,
         urban_expansion,
         precipitation_change,
         gdp_change,
         latitude) %>%
  na.omit()

# -------------------------------
# GAM MODEL (CHANGE–CHANGE FRAMEWORK)
# -------------------------------
gam_model <- gam(utci_change_C ~
                   s(urban_expansion) +
                   s(precipitation_change) +
                   s(gdp_change) +
                   s(latitude),
                 data = df)

# -------------------------------
# MODEL OUTPUTS
# -------------------------------
cat("\n================ GAM SUMMARY ================\n")
print(summary(gam_model))

cat("\n================ ANOVA ================\n")
print(anova(gam_model))

cat("\n================ GAM CHECK ================\n")
gam.check(gam_model)

capture.output(summary(gam_model),
               file = file.path(output_folder, "GAM_Summary_UTCI_Change.txt"))

capture.output(anova(gam_model),
               file = file.path(output_folder, "GAM_Anova_UTCI_Change.txt"))

# -------------------------------
# CONCURVITY CHECK
# -------------------------------
conc <- concurvity(gam_model, full = TRUE)
print(conc)

capture.output(conc,
               file = file.path(output_folder, "GAM_Concurvity_UTCI_Change.txt"))

# -------------------------------
# PARTIAL EFFECT FUNCTION
# -------------------------------
create_partial <- function(model, varname, df) {
  
  predictors <- all.vars(formula(model))[-1]
  
  x_seq <- seq(min(df[[varname]], na.rm = TRUE),
               max(df[[varname]], na.rm = TRUE),
               length.out = 100)
  
  pred_df <- as.data.frame(matrix(ncol = length(predictors), nrow = 100))
  colnames(pred_df) <- predictors
  
  for (v in predictors) {
    if (v == varname) {
      pred_df[[v]] <- x_seq
    } else {
      pred_df[[v]] <- mean(df[[v]], na.rm = TRUE)
    }
  }
  
  pred <- predict(model, newdata = pred_df, se.fit = TRUE)
  
  data.frame(
    x     = x_seq,
    fit   = pred$fit,
    upper = pred$fit + 2 * pred$se.fit,
    lower = pred$fit - 2 * pred$se.fit
  )
}

# -------------------------------
# PARTIAL EFFECTS
# -------------------------------
partial_urban <- create_partial(gam_model, "urban_expansion",      df)
partial_prec  <- create_partial(gam_model, "precipitation_change", df)
partial_gdp   <- create_partial(gam_model, "gdp_change",           df)
partial_lat   <- create_partial(gam_model, "latitude",             df)

# -------------------------------
# PLOT FUNCTION
# -------------------------------
plot_partial <- function(partial_df, xlab, title, line_color, save_name) {
  
  p <- ggplot(partial_df, aes(x = x, y = fit)) +
    geom_line(color = line_color, size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = line_color, alpha = 0.2) +
    labs(x = xlab, y = "UTCI Change (°C/year)", title = title) +
    theme_minimal(base_size = 18)
  
  print(p)
  
  ggsave(
    filename = file.path(output_folder, paste0(save_name, ".png")),
    plot = p, width = 6, height = 5, dpi = 600
  )
}

# -------------------------------
# INDIVIDUAL PLOTS
# -------------------------------
plot_partial(partial_urban,
             "Urban Expansion",
             "Effect of Urban Expansion on UTCI Change",
             "steelblue",
             "UTCI_Urban_Expansion")

plot_partial(partial_prec,
             "Precipitation Change (mm/year)",
             "Effect of Precipitation Change on UTCI Change",
             "darkgreen",
             "UTCI_Precipitation_Change")

plot_partial(partial_gdp,
             "GDP Change ($/year)",
             "Effect of GDP Change on UTCI Change",
             "purple",
             "UTCI_GDP_Change")

plot_partial(partial_lat,
             "Latitude",
             "Effect of Latitude on UTCI Change",
             "firebrick",
             "UTCI_Latitude")

# -------------------------------
# COMBINED PLOT (1 x 4)
# -------------------------------
p_urban <- ggplot(partial_urban, aes(x = x, y = fit)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "steelblue", alpha = 0.2) +
  labs(x = "Urban Expansion", y = "UTCI Change (°C/year)") +
  theme_minimal(base_size = 18)

p_prec <- ggplot(partial_prec, aes(x = x, y = fit)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "darkgreen", alpha = 0.2) +
  labs(x = "Precipitation Change (mm/year)", y = "UTCI Change (°C/year)") +
  theme_minimal(base_size = 18)

p_gdp <- ggplot(partial_gdp, aes(x = x, y = fit)) +
  geom_line(color = "purple", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "purple", alpha = 0.2) +
  labs(x = "GDP Change ($/year)", y = "UTCI Change (°C/year)") +
  theme_minimal(base_size = 18)

p_lat <- ggplot(partial_lat, aes(x = x, y = fit)) +
  geom_line(color = "firebrick", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2) +
  labs(x = "Latitude", y = "UTCI Change (°C/year)") +
  theme_minimal(base_size = 18)

combined_partial <- grid.arrange(p_urban, p_prec, p_gdp, p_lat, ncol = 4)

print(combined_partial)

ggsave(
  filename = file.path(output_folder, "Combined_UTCI_Change_Processes.png"),
  plot = combined_partial,
  width = 20, height = 5, dpi = 600
)