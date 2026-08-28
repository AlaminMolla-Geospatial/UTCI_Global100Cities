# =========================================================
# GLOBAL CITIES:
# =========================================================


# =========================================================
# 1. LOAD LIBRARIES
# =========================================================

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)


# =========================================================
# 2. FILE PATHS
# =========================================================

input_file <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv"

out_dir <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/LCZ_UTCI_Output"


# Create output folder if it does not exist
if (!dir.exists(out_dir)) {
  dir.create(
    out_dir,
    recursive = TRUE
  )
}


# =========================================================
# 3. LOAD FINAL CSV
# =========================================================

df <- read_csv(
  input_file,
  show_col_types = FALSE
)


# =========================================================
# 4. CHECK REQUIRED COLUMNS
# =========================================================

required_columns <- c(
  "City",
  "Koppen_Climate",
  "diff_urban_compact_open",
  "diff_urban_compact_trees",
  "diff_bush_shrub_lowplants_trees",
  "diff_urban_compact_bush_shrub_lowplants",
  "bush_shrub_lowplants_utci_mean",
  "trees_utci_mean",
  "urban_compact_utci_mean",
  "urban_open_utci_mean"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste(
      "The following required columns are missing:",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# =========================================================
# 5. KÖPPEN CLIMATE GROUPING
# =========================================================

df <- df %>%
  mutate(
    
    Koppen_Main = case_when(
      
      grepl(
        "^A",
        Koppen_Climate
      ) ~ "Tropical",
      
      grepl(
        "^B",
        Koppen_Climate
      ) ~ "Arid",
      
      grepl(
        "^C",
        Koppen_Climate
      ) ~ "Temperate",
      
      grepl(
        "^D",
        Koppen_Climate
      ) ~ "Cold",
      
      grepl(
        "^E",
        Koppen_Climate
      ) ~ "Polar",
      
      TRUE ~ NA_character_
    )
  ) %>%
  
  filter(
    !is.na(Koppen_Main)
  )


# =========================================================
# 6. CLEAN MISSING VALUES
# -999 → NA
# =========================================================

variables_to_clean <- c(
  
  "diff_urban_compact_open",
  
  "diff_urban_compact_trees",
  
  "diff_bush_shrub_lowplants_trees",
  
  "diff_urban_compact_bush_shrub_lowplants",
  
  "bush_shrub_lowplants_utci_mean",
  
  "trees_utci_mean",
  
  "urban_compact_utci_mean",
  
  "urban_open_utci_mean"
)


df <- df %>%
  mutate(
    across(
      all_of(variables_to_clean),
      ~ ifelse(
        .x == -999,
        NA,
        .x
      )
    )
  )


# =========================================================
# 7. FUNCTION:
# MEDIAN + MEAN + SAMPLE SIZE
# =========================================================

get_medians <- function(
    data,
    var_name,
    plot_label
) {
  
  stats <- data %>%
    
    select(
      Koppen_Main,
      all_of(var_name)
    ) %>%
    
    rename(
      Value = all_of(var_name)
    ) %>%
    
    group_by(
      Koppen_Main
    ) %>%
    
    summarise(
      
      median_value = median(
        Value,
        na.rm = TRUE
      ),
      
      mean_value = mean(
        Value,
        na.rm = TRUE
      ),
      
      # Number of actual observations used
      n = sum(
        !is.na(Value)
      ),
      
      .groups = "drop"
    ) %>%
    
    mutate(
      Variable = plot_label
    )
  
  
  cat(
    "\n================ MEDIAN + MEAN:",
    plot_label,
    "================\n"
  )
  
  print(stats)
  
  return(stats)
}


# =========================================================
# 8. COLLECT KÖPPEN-BASED STATISTICS
# =========================================================

median_all <- bind_rows(
  
  get_medians(
    df,
    "diff_urban_compact_open",
    "Diff: Compact vs Open"
  ),
  
  get_medians(
    df,
    "diff_urban_compact_trees",
    "Diff: Compact vs Trees"
  ),
  
  get_medians(
    df,
    "diff_bush_shrub_lowplants_trees",
    "Diff: Bush/Shrub/Lowplants vs Trees"
  ),
  
  get_medians(
    df,
    "diff_urban_compact_bush_shrub_lowplants",
    "Diff: Compact vs Bush/Shrub/Lowplants"
  ),
  
  get_medians(
    df,
    "bush_shrub_lowplants_utci_mean",
    "UTCI Bush/Shrub/Lowplants"
  ),
  
  get_medians(
    df,
    "trees_utci_mean",
    "UTCI Trees"
  ),
  
  get_medians(
    df,
    "urban_compact_utci_mean",
    "UTCI Urban Compact"
  ),
  
  get_medians(
    df,
    "urban_open_utci_mean",
    "UTCI Urban Open"
  )
)


# =========================================================
# 9. SAVE KÖPPEN STATISTICS
# =========================================================

write.csv(
  median_all,
  file.path(
    out_dir,
    "ALL_Median_Mean_Values_By_Koppen.csv"
  ),
  row.names = FALSE
)


# =========================================================
# 10. COLOR PALETTE
# =========================================================

# Blue  = lower values
# White = middle / neutral
# Red   = higher values

utci_colors <- c(
  "#2166AC",
  "#F7F7F7",
  "#B2182B"
)


# =========================================================
# 11. FUNCTION FOR DIFFERENCE PLOTS
# =========================================================

make_plot <- function(
    data,
    var,
    title,
    filename,
    ylab
) {
  
  
  # -------------------------------------------------------
  # Select variable
  # -------------------------------------------------------
  
  df_sub <- data %>%
    
    select(
      Koppen_Main,
      all_of(var)
    ) %>%
    
    rename(
      Value = all_of(var)
    ) %>%
    
    filter(
      !is.na(Value)
    )
  
  
  # -------------------------------------------------------
  # ORDER KÖPPEN GROUPS BY MEDIAN VALUE
  #
  # LOWEST MEDIAN → HIGHEST MEDIAN
  # -------------------------------------------------------
  
  median_koppen <- df_sub %>%
    
    group_by(
      Koppen_Main
    ) %>%
    
    summarise(
      
      median_val = median(
        Value,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    
    arrange(
      median_val
    )
  
  
  df_sub$Koppen_Main <- factor(
    
    df_sub$Koppen_Main,
    
    levels = median_koppen$Koppen_Main
  )
  
  
  # -------------------------------------------------------
  # PLOT
  # -------------------------------------------------------
  
  p <- ggplot(
    
    df_sub,
    
    aes(
      x = Koppen_Main,
      y = Value,
      color = Value
    )
  ) +
    
    
    # -----------------------------------------------------
  # Violin
  # -----------------------------------------------------
  
  geom_violin(
    
    aes(
      group = Koppen_Main
    ),
    
    fill = "grey93",
    
    color = NA,
    
    alpha = 0.7,
    
    width = 0.9
  ) +
    
    
    # -----------------------------------------------------
  # Boxplot
  # -----------------------------------------------------
  
  geom_boxplot(
    
    width = 0.35,
    
    fill = "white",
    
    color = "black",
    
    alpha = 0.95,
    
    outlier.shape = NA,
    
    linewidth = 0.4
  ) +
    
    
    # -----------------------------------------------------
  # Individual cities
  # -----------------------------------------------------
  
  geom_jitter(
    
    width = 0.15,
    
    alpha = 0.60,
    
    size = 1.5
  ) +
    
    
    # -----------------------------------------------------
  # Color scale
  # -----------------------------------------------------
  
  scale_color_gradient2(
    
    low = utci_colors[1],
    
    mid = utci_colors[2],
    
    high = utci_colors[3],
    
    midpoint = 0,
    
    name = "Value"
  ) +
    
    
    # -----------------------------------------------------
  # Zero reference line
  # -----------------------------------------------------
  
  geom_hline(
    
    yintercept = 0,
    
    linetype = "dashed",
    
    color = "grey40"
  ) +
    
    
    # -----------------------------------------------------
  # Labels
  # -----------------------------------------------------
  
  labs(
    
    title = title,
    
    x = "Köppen Climate",
    
    y = ylab
  ) +
    
    
    # -----------------------------------------------------
  # Theme
  # -----------------------------------------------------
  theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 16,
        face = "bold"
      ),
      
      axis.text.y = element_text(
        size = 16
      ),
      
      axis.title.x = element_text(
        size = 16,
        face = "bold"
      ),
      
      axis.title.y = element_text(
        size = 16,
        face = "bold"
      ),
      
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 16
      ),
      
      strip.text = element_text(
        face = "bold",
        size = 16
      ),
      
      legend.title = element_text(
        face = "bold",
        size = 16
      ),
      
      legend.text = element_text(
        size = 16
      )
    )
  
  # -------------------------------------------------------
  # SAVE
  # -------------------------------------------------------
  
  ggsave(
    
    file.path(
      out_dir,
      filename
    ),
    
    p,
    
    width = 8,
    
    height = 6,
    
    dpi = 600,
    
    bg = "white"
  )
  
  
  print(p)
}


# =========================================================
# 12. DIFFERENCE PLOTS
# =========================================================

make_plot(
  
  df,
  
  "diff_urban_compact_open",
  
  "Urban Compact vs Urban Open",
  
  "diff_compact_open.png",
  
  "UTCI Difference (°C)"
)


make_plot(
  
  df,
  
  "diff_urban_compact_trees",
  
  "Urban Compact vs Trees",
  
  "diff_compact_trees.png",
  
  "UTCI Difference (°C)"
)


make_plot(
  
  df,
  
  "diff_bush_shrub_lowplants_trees",
  
  "Bush/Shrub/Lowplants vs Trees",
  
  "diff_bush_shrub_trees.png",
  
  "UTCI Difference (°C)"
)


make_plot(
  
  df,
  
  "diff_urban_compact_bush_shrub_lowplants",
  
  "Urban Compact vs Bush/Shrub/Lowplants",
  
  "diff_compact_bush_shrub_lowplants.png",
  
  "UTCI Difference (°C)"
)


# =========================================================
# 13. PREPARE UTCI DATA FOR FOUR-PANEL PLOT
# =========================================================

df_long <- df %>%
  
  select(
    
    Koppen_Main,
    
    bush_shrub_lowplants_utci_mean,
    
    trees_utci_mean,
    
    urban_compact_utci_mean,
    
    urban_open_utci_mean
    
  ) %>%
  
  pivot_longer(
    
    cols = -Koppen_Main,
    
    names_to = "Variable",
    
    values_to = "Value"
  ) %>%
  
  filter(
    !is.na(Value)
  )


# =========================================================
# 14. RENAME UTCI VARIABLES
# =========================================================

df_long$Variable <- recode(
  
  df_long$Variable,
  
  "bush_shrub_lowplants_utci_mean" =
    "Bush/Shrub/Lowplants",
  
  "trees_utci_mean" =
    "Trees",
  
  "urban_compact_utci_mean" =
    "Urban Compact",
  
  "urban_open_utci_mean" =
    "Urban Open"
)


# =========================================================
# 15. ORDER KÖPPEN GROUPS BY MEDIAN UTCI

# Lowest median UTCI → highest median UTCI
# =========================================================

median_koppen <- df_long %>%
  
  group_by(
    Koppen_Main
  ) %>%
  
  summarise(
    
    median_val = median(
      Value,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    median_val
  )


df_long$Koppen_Main <- factor(
  
  df_long$Koppen_Main,
  
  levels = median_koppen$Koppen_Main
)


# Print order so you can verify it
cat(
  "\n=========================================================\n",
  "Köppen climate order based on median UTCI:\n",
  "=========================================================\n"
)

print(median_koppen)


# =========================================================
# 16. DETERMINE COMMON Y-AXIS RANGE
# =========================================================

# All four UTCI variables are used to determine
# ONE common vertical scale.

utci_min <- floor(
  min(
    df_long$Value,
    na.rm = TRUE
  )
)


utci_max <- ceiling(
  max(
    df_long$Value,
    na.rm = TRUE
  )
)


# Calculate range
utci_range <- utci_max - utci_min


# Add 5% padding
common_y_min <- utci_min -
  0.05 * utci_range


common_y_max <- utci_max +
  0.05 * utci_range


# Print common scale
cat(
  "\n=========================================================\n",
  "COMMON UTCI Y-AXIS:\n",
  "=========================================================\n"
)

cat(
  "Minimum:",
  common_y_min,
  "°C\n"
)

cat(
  "Maximum:",
  common_y_max,
  "°C\n"
)


# =========================================================
# 17. UTCI FOUR-PANEL PLOT
#
# ALL FOUR PANELS HAVE THE SAME VERTICAL SCALE
# =========================================================

p_utci <- ggplot(
  
  df_long,
  
  aes(
    x = Koppen_Main,
    y = Value,
    color = Value
  )
) +
  
  
  # -------------------------------------------------------
# Violin
# -------------------------------------------------------

geom_violin(
  
  aes(
    group = Koppen_Main
  ),
  
  fill = "grey93",
  
  color = NA,
  
  alpha = 0.7,
  
  width = 0.9
) +
  
  
  # -------------------------------------------------------
# Boxplot
# -------------------------------------------------------

geom_boxplot(
  
  width = 0.35,
  
  fill = "white",
  
  color = "black",
  
  alpha = 0.95,
  
  outlier.shape = NA,
  
  linewidth = 0.4
) +
  
  
  # -------------------------------------------------------
# Individual cities
# -------------------------------------------------------

geom_jitter(
  
  width = 0.15,
  
  alpha = 0.60,
  
  size = 1.4
) +
  
  
  # -------------------------------------------------------
# UTCI COLOR SCALE
# -------------------------------------------------------

scale_color_gradient2(
  
  low = "#2166AC",
  
  mid = "#F7F7F7",
  
  high = "#B2182B",
  
  midpoint = 0,
  
  name = "UTCI (°C)"
) +
  
  
  # -------------------------------------------------------
# FOUR PANELS
# -------------------------------------------------------

facet_wrap(
  
  ~Variable,
  
  scales = "fixed",
  
  ncol = 2
) +
  
  
  # -------------------------------------------------------
# EXPLICIT COMMON Y LIMITS
# -------------------------------------------------------

coord_cartesian(
  
  ylim = c(
    common_y_min,
    common_y_max
  )
) +
  
  
  # -------------------------------------------------------
# LABELS
# -------------------------------------------------------

labs(
  
  title =
    "UTCI Across Local Climate Zones and Köppen Climates",
  
  x = "Köppen Climate",
  
  y = "UTCI Mean (°C)"
) +
  
  
  # -------------------------------------------------------
# THEME
# -------------------------------------------------------

theme_minimal(
  base_size = 12
) +
  
  theme(
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.position = "right"
  )


# =========================================================
# 18. SAVE UTCI FOUR-PANEL FIGURE
# =========================================================

ggsave(
  
  file.path(
    out_dir,
    "UTCI_Mean_Koppen_4panel.png"
  ),
  
  p_utci,
  
  width = 10,
  
  height = 8,
  
  dpi = 600,
  
  bg = "white"
)


# Display
print(p_utci)


# =========================================================
# 19. OVERALL SUMMARY
# =========================================================

overall_summary <- df %>%
  
  select(
    all_of(
      variables_to_clean
    )
  ) %>%
  
  summarise(
    
    across(
      
      everything(),
      
      list(
        
        mean = ~mean(
          .x,
          na.rm = TRUE
        ),
        
        median = ~median(
          .x,
          na.rm = TRUE
        ),
        
        sd = ~sd(
          .x,
          na.rm = TRUE
        ),
        
        min = ~min(
          .x,
          na.rm = TRUE
        ),
        
        max = ~max(
          .x,
          na.rm = TRUE
        ),
        
        n = ~sum(
          !is.na(.x)
        )
      ),
      
      .names = "{.col}_{.fn}"
    )
  ) %>%
  
  pivot_longer(
    
    cols = everything(),
    
    names_to = "Variable_Stat",
    
    values_to = "Value"
  ) %>%
  
  separate(
    
    Variable_Stat,
    
    into = c(
      "Variable",
      "Statistic"
    ),
    
    sep = "_(?=[^_]+$)"
  )


# =========================================================
# 20. SAVE OVERALL SUMMARY
# =========================================================

write.csv(
  
  overall_summary,
  
  file.path(
    out_dir,
    "Overall_Variable_Summary_No_Koppen.csv"
  ),
  
  row.names = FALSE
)


# =========================================================
# 21. FINAL MESSAGE
# =========================================================

cat(
  
  "\n=========================================================\n",
  "Analysis completed successfully.\n",
  "=========================================================\n",
  
  "Output folder:\n",
  
  out_dir,
  
  "\n=========================================================\n"
)