# =========================================================
# LOAD LIBRARIES
# =========================================================
library(tidyverse)
library(ggrepel)

# =========================================================
# LOAD DATA
# =========================================================
df <- read.csv(
  "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv",
  stringsAsFactors = FALSE
)

# =========================================================
# SAFETY CHECK
# =========================================================
df <- df %>%
  filter(totalpop_2000 > 0,
         totalpop_2020 > 0)

# =========================================================
# CALCULATE SEVERITY SCORES
# =========================================================
df <- df %>%
  mutate(
    
    severity_score_2000 =
      (
        pop_moderate_2000 * 1 +
          pop_strong_2000 * 2 +
          pop_very_strong_2000 * 3 +
          pop_extreme_2000 * 4
      ) / totalpop_2000,
    
    severity_score_2020 =
      (
        pop_moderate_2020 * 1 +
          pop_strong_2020 * 2 +
          pop_very_strong_2020 * 3 +
          pop_extreme_2020 * 4
      ) / totalpop_2020,
    
    delta_severity =
      severity_score_2020 -
      severity_score_2000
  )

# =========================================================
# SAVE UPDATED CSV
# =========================================================
write.csv(
  df,
  "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv",
  row.names = FALSE
)

cat("✓ Severity scores calculated\n")

# =========================================================
# THRESHOLDS
# =========================================================
x_threshold <- 1.0
y_threshold <- 0.0
y_buffer <- 0.005

y_upper <- y_threshold + y_buffer
y_lower <- y_threshold - y_buffer

# =========================================================
# CLASSIFY CITIES
# =========================================================
df <- df %>%
  mutate(
    
    severity_class = case_when(
      
      abs(delta_severity - y_threshold) <= y_buffer
      ~ "Transitional",
      
      severity_score_2000 >= x_threshold &
        delta_severity > y_upper
      ~ "Crisis",
      
      severity_score_2000 >= x_threshold &
        delta_severity < y_lower
      ~ "Improving",
      
      severity_score_2000 < x_threshold &
        delta_severity > y_upper
      ~ "Emerging Risk",
      
      severity_score_2000 < x_threshold &
        delta_severity < y_lower
      ~ "Low Risk",
      
      TRUE ~ NA_character_
    ),
    
    severity_class = factor(
      severity_class,
      levels = c(
        "Crisis",
        "Improving",
        "Emerging Risk",
        "Low Risk",
        "Transitional"
      )
    ),
    
    abs_severity = abs(delta_severity)
  )

# =========================================================
# PRINT SUMMARY BY CLASS
# =========================================================
for (cls in levels(df$severity_class)) {
  
  cities <- df %>%
    filter(severity_class == cls) %>%
    arrange(desc(abs_severity)) %>%
    pull(city)
  
  cat("\n===", cls,
      "(n =", length(cities), ") ===\n")
  
  cat(paste(cities, collapse = ", "), "\n")
}

# =========================================================
# LABEL TOP 5 PER CATEGORY
# =========================================================
label_df <- df %>%
  filter(severity_class != "Transitional") %>%
  group_by(severity_class) %>%
  arrange(desc(abs_severity), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup()

# =========================================================
# QUADRANT PLOT
# =========================================================
p <- ggplot(
  df,
  aes(
    x = severity_score_2000,
    y = delta_severity,
    color = severity_class
  )
) +
  
  annotate(
    "rect",
    xmin = x_threshold,
    xmax = Inf,
    ymin = y_upper,
    ymax = Inf,
    fill = "#d73027",
    alpha = 0.06
  ) +
  
  annotate(
    "rect",
    xmin = x_threshold,
    xmax = Inf,
    ymin = -Inf,
    ymax = y_lower,
    fill = "#1a9850",
    alpha = 0.06
  ) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = x_threshold,
    ymin = y_upper,
    ymax = Inf,
    fill = "#fee090",
    alpha = 0.08
  ) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = x_threshold,
    ymin = -Inf,
    ymax = y_lower,
    fill = "#91bfdb",
    alpha = 0.08
  ) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = y_lower,
    ymax = y_upper,
    fill = "#888888",
    alpha = 0.12
  ) +
  
  geom_hline(
    yintercept = y_upper,
    linetype = "dotted",
    color = "grey50"
  ) +
  
  geom_hline(
    yintercept = y_lower,
    linetype = "dotted",
    color = "grey50"
  ) +
  
  geom_vline(
    xintercept = x_threshold,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey40"
  ) +
  
  geom_point(
    size = 3.5,
    alpha = 0.85
  ) +
  
  geom_text_repel(
    data = label_df,
    aes(label = city),
    size = 3,
    fontface = "bold",
    max.overlaps = 30
  ) +
  
  scale_color_manual(
    values = c(
      "Crisis" = "#d73027",
      "Improving" = "#1a9850",
      "Emerging Risk" = "#e08214",
      "Low Risk" = "#4575b4",
      "Transitional" = "#7b2d8b"
    )
  ) +
  
  scale_x_continuous(
    limits = c(0,4),
    breaks = seq(0,4,0.5)
  ) +
  
  labs(
    x = "Heat Stress Severity Score (2000)",
    y = expression(Delta*" Severity Score (2020 - 2000)"),
    color = "Class"
  ) +
  
  theme_bw(base_size = 16) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# =========================================================
# SAVE FIGURE
# =========================================================
ggsave(
  "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Plots_Output/Quadrant_SeverityScore.png",
  p,
  width = 12,
  height = 9,
  dpi = 600
)

cat("\n✓ Quadrant plot created and saved\n")