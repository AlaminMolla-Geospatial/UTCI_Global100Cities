# =========================================================
# LOAD LIBRARIES
# =========================================================
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# =========================================================
# FILE PATHS
# =========================================================
input_file <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv"
out_dir <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/LCZ_UTCI_Output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

# =========================================================
# LOAD DATA
# =========================================================
df <- read_csv(input_file)

# =========================================================
# KÖPPEN CLASSIFICATION
# =========================================================
df <- df %>%
  mutate(
    Koppen_Main = case_when(
      grepl("^A", Koppen_Climate) ~ "Tropical",
      grepl("^B", Koppen_Climate) ~ "Arid",
      grepl("^C", Koppen_Climate) ~ "Temperate",
      grepl("^D", Koppen_Climate) ~ "Cold",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Koppen_Main))

# =========================================================
# CLEAN ALL VARIABLES (-999 → NA)
# =========================================================
df <- df %>%
  mutate(
    diff_urban_compact_open = ifelse(diff_urban_compact_open == -999, NA, diff_urban_compact_open),
    diff_urban_compact_trees = ifelse(diff_urban_compact_trees == -999, NA, diff_urban_compact_trees),
    diff_bush_shrub_lowplants_trees = ifelse(diff_bush_shrub_lowplants_trees == -999, NA, diff_bush_shrub_lowplants_trees),
    diff_urban_compact_bush_shrub_lowplants = ifelse(diff_urban_compact_bush_shrub_lowplants == -999, NA, diff_urban_compact_bush_shrub_lowplants),
    bush_shrub_lowplants_utci_mean = ifelse(bush_shrub_lowplants_utci_mean == -999, NA, bush_shrub_lowplants_utci_mean),
    trees_utci_mean = ifelse(trees_utci_mean == -999, NA, trees_utci_mean),
    urban_compact_utci_mean = ifelse(urban_compact_utci_mean == -999, NA, urban_compact_utci_mean),
    urban_open_utci_mean = ifelse(urban_open_utci_mean == -999, NA, urban_open_utci_mean)
  )

# =========================================================
# FUNCTION: MEDIAN + MEAN + PRINT
# =========================================================
get_medians <- function(data, var_name, plot_label) {
  
  stats <- data %>%
    select(Koppen_Main, all_of(var_name)) %>%
    rename(Value = all_of(var_name)) %>%
    group_by(Koppen_Main) %>%
    summarise(
      median_value = median(Value, na.rm = TRUE),
      mean_value   = mean(Value, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(Variable = plot_label)
  
  cat("\n================ MEDIAN + MEAN:", plot_label, "================\n")
  print(stats)
  
  return(stats)
}

# =========================================================
# COLLECT KÖPPEN-BASED STATISTICS
# =========================================================
median_all <- bind_rows(
  get_medians(df, "diff_urban_compact_open", "Diff: Compact vs Open"),
  get_medians(df, "diff_urban_compact_trees", "Diff: Compact vs Trees"),
  get_medians(df, "diff_bush_shrub_lowplants_trees", "Diff: Bush vs Trees"),
  get_medians(df, "diff_urban_compact_bush_shrub_lowplants", "Compact vs Bush/Shrub/Lowplants"),
  get_medians(df, "bush_shrub_lowplants_utci_mean", "UTCI Bush/Shrub"),
  get_medians(df, "trees_utci_mean", "UTCI Trees"),
  get_medians(df, "urban_compact_utci_mean", "UTCI Compact"),
  get_medians(df, "urban_open_utci_mean", "UTCI Open")
)

write.csv(
  median_all,
  file.path(out_dir, "ALL_Median_Mean_Values_By_Koppen.csv"),
  row.names = FALSE
)

# =========================================================
# PLOT FUNCTION
# =========================================================
make_plot <- function(data, var, title, filename, ylab) {
  
  df_sub <- data %>%
    select(Koppen_Main, all_of(var)) %>%
    rename(Value = all_of(var)) %>%
    drop_na()
  
  median_koppen <- df_sub %>%
    group_by(Koppen_Main) %>%
    summarise(median_val = median(Value, na.rm = TRUE)) %>%
    arrange(median_val)
  
  df_sub$Koppen_Main <- factor(df_sub$Koppen_Main,
                               levels = median_koppen$Koppen_Main)
  
  p <- ggplot(df_sub, aes(Koppen_Main, Value, color = Value)) +
    
    # NEW: violin layer
    geom_violin(
      aes(group = Koppen_Main),
      fill = "grey90",
      color = NA,
      alpha = 0.6,
      width = 0.9
    ) +
    
    # ORIGINAL boxplot (slightly refined)
    geom_boxplot(
      width = 0.35,
      fill = "white",
      color = "black",
      alpha = 0.9,
      outlier.shape = NA
    ) +
    
    geom_jitter(width = 0.15, alpha = 0.55, size = 1.4) +
    scale_color_gradientn(colors = c("#2c7bb6", "#6a51a3", "#d73027")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = title, x = "Köppen Climate", y = ylab, color = "Value") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
  
  ggsave(file.path(out_dir, filename), p,
         width = 8, height = 6, dpi = 600)
  
  print(p)
}

# =========================================================
# DIFF PLOTS
# =========================================================
make_plot(df, "diff_urban_compact_open", "Urban Compact vs Urban Open", "diff_compact_open.png", "Difference")
make_plot(df, "diff_urban_compact_trees", "Urban Compact vs Trees", "diff_compact_trees.png", "Difference")
make_plot(df, "diff_bush_shrub_lowplants_trees", "Bush/Shrub vs Trees", "diff_bush_shrub_trees.png", "Difference")
make_plot(df, "diff_urban_compact_bush_shrub_lowplants", "Urban Compact vs Bush/Shrub/Lowplants", "diff_compact_bush_shrub_lowplants.png", "Difference")

# =========================================================
# UTCI MULTI-PANEL
# =========================================================
df_long <- df %>%
  select(
    Koppen_Main,
    bush_shrub_lowplants_utci_mean,
    trees_utci_mean,
    urban_compact_utci_mean,
    urban_open_utci_mean
  ) %>%
  pivot_longer(-Koppen_Main,
               names_to = "Variable",
               values_to = "Value") %>%
  drop_na()

df_long$Variable <- recode(df_long$Variable,
                           "bush_shrub_lowplants_utci_mean" = "Bush/Shrub/Lowplants",
                           "trees_utci_mean" = "Trees",
                           "urban_compact_utci_mean" = "Urban Compact",
                           "urban_open_utci_mean" = "Urban Open")

median_koppen <- df_long %>%
  group_by(Koppen_Main) %>%
  summarise(median_val = median(Value, na.rm = TRUE)) %>%
  arrange(median_val)

df_long$Koppen_Main <- factor(df_long$Koppen_Main,
                              levels = median_koppen$Koppen_Main)

p_utci <- ggplot(df_long, aes(Koppen_Main, Value, color = Value)) +
  
  # NEW violin
  geom_violin(aes(group = Koppen_Main),
              fill = "grey90",
              color = NA,
              alpha = 0.6,
              width = 0.9) +
  
  # ORIGINAL boxplot
  geom_boxplot(width = 0.35,
               fill = "white",
               color = "black",
               alpha = 0.9,
               outlier.shape = NA) +
  
  geom_jitter(width = 0.15, alpha = 0.55, size = 1.4) +
  scale_color_gradientn(colors = c("#2c7bb6", "#6a51a3", "#d73027")) +
  facet_wrap(~Variable, scales = "free_y", ncol = 2) +
  labs(title ="UTCI for Local Climate Zone VS. Koppen Climate",
       x = "Köppen Climate",
       y = "UTCI Mean",
       color = "UTCI") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(file.path(out_dir, "UTCI_Mean_Koppen_4panel.png"),
       p_utci, width = 10, height = 8, dpi = 600)

print(p_utci)

# =========================================================
# OVERALL SUMMARY
# =========================================================
overall_summary <- df %>%
  select(
    diff_urban_compact_open,
    diff_urban_compact_trees,
    diff_bush_shrub_lowplants_trees,
    diff_urban_compact_bush_shrub_lowplants,
    bush_shrub_lowplants_utci_mean,
    trees_utci_mean,
    urban_compact_utci_mean,
    urban_open_utci_mean
  ) %>%
  summarise(
    across(
      everything(),
      list(
        mean   = ~mean(.x, na.rm = TRUE),
        median = ~median(.x, na.rm = TRUE),
        sd     = ~sd(.x, na.rm = TRUE),
        min    = ~min(.x, na.rm = TRUE),
        max    = ~max(.x, na.rm = TRUE),
        n      = ~sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variable_Stat",
    values_to = "Value"
  ) %>%
  separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_(?=[^_]+$)")

write.csv(
  overall_summary,
  file.path(out_dir, "Overall_Variable_Summary_No_Koppen.csv"),
  row.names = FALSE
)