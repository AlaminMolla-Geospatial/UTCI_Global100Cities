# =========================================================
# GLOBAL CITY UTCI ANALYSIS AND PLOTS
# =========================================================


# =========================================================
# 1. LOAD LIBRARIES
# =========================================================

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)


# =========================================================
# 2. INPUT CSV
# =========================================================

input_file <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv"

df <- read_csv(
  input_file,
  show_col_types = FALSE
)


# =========================================================
# 3. CHECK REQUIRED COLUMNS
# =========================================================

required_columns <- c(
  "City",
  "Koppen_Climate",
  "weighted_utci_slope",
  "weighted_utci_slope_p",
  "latitude",
  "longitude"
)

missing_columns <- setdiff(
  required_columns,
  names(df)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste(
      "The following required columns are missing:",
      paste(missing_columns, collapse = ", ")
    )
  )
}


# =========================================================
# 4. PREPARE DATA
# =========================================================

df <- df %>%
  mutate(
    
    # Ensure correct data types
    City = as.character(City),
    
    latitude = as.numeric(latitude),
    
    longitude = as.numeric(longitude),
    
    weighted_utci_slope = as.numeric(
      weighted_utci_slope
    ),
    
    weighted_utci_slope_p = as.numeric(
      weighted_utci_slope_p
    ),
    
    # -----------------------------------------------------
    # Major Köppen climate groups
    # -----------------------------------------------------
    
    Koppen_Main = case_when(
      
      grepl("^A", Koppen_Climate) ~ "Tropical",
      
      grepl("^B", Koppen_Climate) ~ "Arid",
      
      grepl("^C", Koppen_Climate) ~ "Temperate",
      
      grepl("^D", Koppen_Climate) ~ "Cold",
      
      grepl("^E", Koppen_Climate) ~ "Polar",
      
      TRUE ~ NA_character_
    ),
    
    # UTCI trend
    utci_change = weighted_utci_slope
  )


# =========================================================
# 5. OUTPUT FOLDER
# =========================================================

out_dir <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Plots_Output"

if (!dir.exists(out_dir)) {
  
  dir.create(
    out_dir,
    recursive = TRUE
  )
}


# =========================================================
# 6. SAVE-PLOT FUNCTION
# =========================================================

save_plot <- function(
    plot,
    filename,
    width = 8,
    height = 6
) {
  
  ggsave(
    filename = file.path(
      out_dir,
      filename
    ),
    plot = plot,
    width = width,
    height = height,
    dpi = 600,
    bg = "white"
  )
}


# =========================================================
# 7. PLOT 1
# UTCI RATE OF CHANGE BY KÖPPEN CLIMATE
# =========================================================

plot1_data <- df %>%
  filter(
    !is.na(Koppen_Main),
    !is.na(weighted_utci_slope)
  )


p1 <- ggplot(
  plot1_data,
  aes(
    x = Koppen_Main,
    y = weighted_utci_slope,
    color = utci_change
  )
) +
  
  # Violin distribution
  geom_violin(
    fill = "grey93",
    color = NA,
    alpha = 0.7
  ) +
  
  # Boxplot
  geom_boxplot(
    width = 0.28,
    fill = "white",
    color = "black",
    outlier.shape = NA,
    linewidth = 0.4
  ) +
  
  # Individual cities
  geom_jitter(
    width = 0.13,
    alpha = 0.75,
    size = 1.8
  ) +
  
  # Color scale
  scale_color_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 0,
    name = "UTCI trend\n(°C/year)"
  ) +
  
  # Theme
  theme_minimal(
    base_size = 12
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    axis.title = element_text(
      size = 12,
      face = "bold"
    ),
    
    axis.text = element_text(
      size = 11
    ),
    
    panel.grid.major.x = element_blank(),
    
    legend.title = element_text(
      size = 10,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 9
    )
  ) +
  
  labs(
    title = "UTCI Rate of Change Across Köppen Climate Zones",
    x = "Köppen Climate",
    y = "UTCI Rate of Change (°C/year)"
  )


# =========================================================
# 8. WORLD MAP DATA
# =========================================================

map_data <- df %>%
  filter(
    !is.na(latitude),
    !is.na(longitude),
    !is.na(weighted_utci_slope)
  )


# =========================================================
# 9. IDENTIFY TOP 5 WARMING CITIES
# =========================================================

top5_pos <- map_data %>%
  arrange(
    desc(weighted_utci_slope)
  ) %>%
  slice_head(
    n = 5
  ) %>%
  mutate(
    label_group = "Top 5 Warming"
  )


# =========================================================
# 10. IDENTIFY TOP 5 COOLING CITIES
# =========================================================

top5_neg <- map_data %>%
  arrange(
    weighted_utci_slope
  ) %>%
  slice_head(
    n = 5
  ) %>%
  mutate(
    label_group = "Top 5 Cooling"
  )


# =========================================================
# 11. COMBINE LABEL CITIES
# =========================================================

labeled_cities <- bind_rows(
  top5_pos,
  top5_neg
)


# =========================================================
# 12. WORLD SHAPEFILE
# =========================================================

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)


# =========================================================
# 13. SYMMETRIC UTCI COLOR LIMIT
# =========================================================

utci_lim <- max(
  abs(map_data$weighted_utci_slope),
  na.rm = TRUE
)


# =========================================================
# 14. WORLD MAP
# =========================================================

p_map <- ggplot() +
  
  # -------------------------------------------------------
# VERY LIGHT WORLD BACKGROUND
# -------------------------------------------------------

geom_sf(
  data = world,
  fill = "grey97",
  color = "grey70",
  linewidth = 0.25
) +
  
  
  # -------------------------------------------------------
# ALL GLOBAL CITIES
# -------------------------------------------------------

geom_point(
  data = map_data,
  
  aes(
    x = longitude,
    y = latitude,
    fill = weighted_utci_slope
  ),
  
  shape = 21,
  
  # Slightly smaller so 100 cities don't overwhelm map
  size = 3.0,
  
  # IMPORTANT:
  # black outline makes near-zero/yellow cities visible
  color = "black",
  
  stroke = 0.45,
  
  alpha = 1
) +
  
  
  # -------------------------------------------------------
# LABEL TOP 5 WARMING + TOP 5 COOLING
# -------------------------------------------------------

geom_label_repel(
  data = labeled_cities,
  
  aes(
    x = longitude,
    y = latitude,
    label = City
  ),
  
  size = 3.0,
  
  fontface = "bold",
  
  # White label background
  fill = "white",
  
  color = "black",
  
  alpha = 0.95,
  
  box.padding = 0.55,
  
  point.padding = 0.35,
  
  min.segment.length = 0,
  
  segment.color = "grey35",
  
  segment.linewidth = 0.3,
  
  max.overlaps = Inf
) +
  
  
  # -------------------------------------------------------
# UTCI COLOR SCALE
#
# Blue  = cooling
# White = near zero
# Red   = warming
#
# White midpoint avoids yellow blending with land
# -------------------------------------------------------

scale_fill_gradient2(
  
  low = "#2166AC",
  
  mid = "#F7F7F7",
  
  high = "#B2182B",
  
  midpoint = 0,
  
  limits = c(
    -utci_lim,
    utci_lim
  ),
  
  name = "UTCI trend\n(°C/year)"
) +
  
  
  # -------------------------------------------------------
# GLOBAL MAP EXTENT
# -------------------------------------------------------

coord_sf(
  xlim = c(
    -180,
    180
  ),
  
  ylim = c(
    -60,
    85
  ),
  
  expand = FALSE
) +
  
  
  # -------------------------------------------------------
# MAP THEME
# -------------------------------------------------------

theme_void() +
  
  theme(
    
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5,
      margin = margin(
        b = 10
      )
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 16
    ),
    
    legend.key.height = unit(
      1.2,
      "cm"
    ),
    
    plot.margin = margin(
      10,
      10,
      10,
      10
    )
  ) +
  
  
  # -------------------------------------------------------
# TITLE
# -------------------------------------------------------

labs(
  title = "UTCI Rate of Change Across Global Cities"
)


# =========================================================
# 15. SAVE PLOT 1
# =========================================================

save_plot(
  p1,
  "utci_koppen.png",
  width = 8,
  height = 6
)


# =========================================================
# 16. SAVE WORLD MAP
# =========================================================

save_plot(
  p_map,
  "utci_world_map.png",
  width = 16,
  height = 8.5
)


# =========================================================
# 17. DISPLAY PLOTS
# =========================================================

print(p1)

print(p_map)