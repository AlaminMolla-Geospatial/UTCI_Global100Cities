# =========================================================
# STEP 1: CREATE TREND DATA
# =========================================================

library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(broom)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)

folder_path <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/Weighted_UTCI_HOMOGENIZED"

csv_files <- list.files(
  folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

calc_slope <- function(df, y, x = "year") {
  
  if (!y %in% names(df) || all(is.na(df[[y]]))) {
    return(c(slope = NA, p_value = NA))
  }
  
  model <- lm(as.formula(paste(y, "~", x)), data = df)
  
  c(
    slope   = coef(summary(model))[2,1],
    p_value = coef(summary(model))[2,4]
  )
}

process_city_file <- function(file_path){
  
  city_name <- str_extract(basename(file_path), "^[^_]+")
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  map_df(unique(df$season), function(s){
    
    d <- filter(df, season == s)
    
    urban_pct    <- calc_slope(d, "urban_pct")
    urban_utci   <- calc_slope(d, "urban_utci")
    weighted_utci <- calc_slope(d, "weighted_utci")
    
    tibble(
      City = city_name,
      Warm_Season = s,
      Koppen_Climate = d$Koppen_Climate[1],
      
      urban_pct_slope     = urban_pct["slope"],
      urban_pct_pvalue    = urban_pct["p_value"],
      
      urban_UTCI_slope    = urban_utci["slope"],
      urban_UTCI_pvalue   = urban_utci["p_value"],
      
      weighted_utci_slope  = weighted_utci["slope"],
      weighted_utci_pvalue = weighted_utci["p_value"]
    )
  })
}

trend_df <- map_df(csv_files, process_city_file)

# =========================================================
# STEP 2: MERGE WITH CITY METADATA
# =========================================================

city_meta <- read_csv(
  "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Comprehensive_GlobalCities.csv"
)

df <- trend_df %>%
  left_join(
    city_meta %>%
      select(
        City,
        latitude,
        longitude
      ),
    by = "City"
  )

# =========================================================
# OUTPUT FOLDER
# =========================================================

out_dir <- "E:/GIS_SWIFL_Research/GlobalCities/CSV_Files/GlobalCities_RESULT/Plots_Output"

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

save_plot <- function(plot, filename,
                      width = 8,
                      height = 6){
  
  ggsave(
    filename = file.path(out_dir, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 600
  )
}

# =========================================================
# KOPPEN GROUPING
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
  )

df$utci_change <- df$weighted_utci_slope

# =========================================================
# PLOT 1
# =========================================================

p1 <- ggplot(
  df,
  aes(
    x = Koppen_Main,
    y = weighted_utci_slope,
    color = utci_change
  )
) +
  geom_violin(
    fill = "grey90",
    color = NA,
    alpha = 0.6
  ) +
  geom_boxplot(
    width = 0.3,
    fill = "white",
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.6,
    size = 1.5
  ) +
  scale_color_gradientn(
    colors = c("#2c7bb6","#6a51a3","#d73027")
  ) +
  theme_minimal() +
  labs(
    title = "UTCI Rate of Change Across Köppen Climate Zones",
    x = "Köppen Climate",
    y = "UTCI Rate of Change (°C/year)"
  )

# =========================================================
# PLOT 2: WORLD MAP
# =========================================================

top5_pos <- df %>%
  arrange(desc(weighted_utci_slope)) %>%
  slice_head(n = 5) %>%
  mutate(label_group = "Top 5 Warming")

top5_neg <- df %>%
  arrange(weighted_utci_slope) %>%
  slice_head(n = 5) %>%
  mutate(label_group = "Top 5 Cooling")

labeled_cities <- bind_rows(top5_pos, top5_neg)

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

utci_lim <- max(
  abs(df$weighted_utci_slope),
  na.rm = TRUE
)

p_map <- ggplot() +
  
  geom_sf(
    data = world,
    fill = "#e8e4de",
    color = "grey60",
    linewidth = 0.2
  ) +
  
  geom_point(
    data = df,
    aes(
      longitude,
      latitude,
      fill = weighted_utci_slope
    ),
    shape = 21,
    size = 3,
    color = "white"
  ) +
  
  geom_label_repel(
    data = labeled_cities,
    aes(
      longitude,
      latitude,
      label = City
    ),
    size = 2.7
  ) +
  
  scale_fill_gradient2(
    low = "#1a9641",
    mid = "#ffffbf",
    high = "#d7191c",
    midpoint = 0,
    limits = c(-utci_lim, utci_lim)
  ) +
  
  coord_sf(expand = FALSE) +
  theme_void() +
  labs(
    title = "UTCI Rate of Change Across Global Cities"
  )

# =========================================================
# SAVE ONLY TWO FIGURES
# =========================================================

save_plot(p1, "utci_koppen.png")

save_plot(
  p_map,
  "utci_world_map.png",
  width = 14,
  height = 8
)