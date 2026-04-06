#######################################################################################
#water availability map as a main figure in the research paper 

# Load required libraries
library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Prepare water availability data
data_wide <- data %>%
  pivot_wider(names_from = variable, values_from = value)

data_terra <- data_wide %>%
  filter(data_source == "terraclimate") %>%
  mutate(W = tp - e)

# 2. Compute trend and p-value
water_trends <- data_terra %>%
  group_by(city, Continent) %>%
  summarise(
    trend_W = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop"
  )

summary(water_trends$Continent)
# 3. Merge with coordinates, all cities
map_data <- data_terra %>%
  left_join(water_trends, by = "city") %>%
  distinct(city, .keep_all = TRUE) %>%
  filter(!is.na(trend_W)) %>%
  mutate(significance = ifelse(p_value < 0.05, "Significant", "Not Significant")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# 4. Bin trend values
map_data <- map_data %>%
  mutate(trend_bin = cut(trend_W,
                         breaks = c(-Inf, -7.5, -5, -2.5, 0, 2.5, 5, 7.5, 10, 20),
                         labels = c("< -7.5", "-7.5 – -5", "-5 – -2.5", "-2.5 – 0",
                                    "0 – 2.5", "2.5 – 5", "5 – 7.5", "7.5 – 10", "10 – 20"),
                         right = FALSE))

# 5. Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# 6. Define custom bin colors (your version)
bin_colors <- c(
  "< -7.5" = "darkred",
  "-7.5 – -5" = "red",
  "-5 – -2.5" = "orange",
  "-2.5 – 0" = "yellow",
  "0 – 2.5" = "gray60",
  "2.5 – 5" = "darkblue",
  "5 – 7.5" = "purple",
  "7.5 – 10" = "green",
  "10 – 20" = "darkgreen")

# 7. Plot the map
ggplot() +
  # World map background
  geom_sf(data = world, fill = "gray95", color = "grey40", size = 0.2) +
  
  # Plot points with fill = trend bin and shape = significance
  geom_sf(data = map_data,
          aes(fill = trend_bin, shape = significance),
          size = 2.5, color = "black", stroke = 0.3) +
  
  # Color scale for trend bins
  scale_fill_manual(
    values = bin_colors, name = "Trend (mm/year)", drop = TRUE,
    na.translate = FALSE, guide = guide_legend(
      override.aes = list(shape = 21, color = "black", size = 4)
    )
  ) +
  
  # Shape scale for significance
  scale_shape_manual( values = c("Significant" = 24, "Not Significant" = 21),
    name = "Significance (p < 0.05)"
  ) +
  # Robinson projection
  coord_sf(crs = "+proj=robin") +
  # Theme and grid lines
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray40", size = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

###########################################################################################
# Water availability map (with significance stars)
# Load required libraries
library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# 1. Prepare water availability data
data_wide <- data %>%
  pivot_wider(names_from = variable, values_from = value)

data_terra <- data_wide %>%
  filter(data_source == "terraclimate") %>%
  mutate(W = tp - e)

# 2. Compute trend (Sen's slope + p-value)
water_trends <- data_terra %>%
  group_by(city, Continent) %>%
  summarise(
    trend_W = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop")
# 3. Merge with coordinates, all cities
map_data <- data_terra %>%
  left_join(water_trends, by = "city") %>%
  distinct(city, .keep_all = TRUE) %>%
  filter(!is.na(trend_W)) %>%
  arrange(desc(trend_W)) %>%  
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

map_data <- map_data %>%
  mutate(trend_bin = cut(trend_W,
                         breaks = c(-Inf, -12, -8, -3, -1, 0, 1, 3, 8, 12, Inf),
                         labels = c("< -12", "-12 – -8", "-8 – -3", "-3 – -1", "-1 – 0",
                                    "0 – 1", "1 – 3", "3 – 8", "8 – 12", "> 12"),
                         right = FALSE))

# Reorder factor levels: Positive trends first, then negatives
map_data$trend_bin <- factor(map_data$trend_bin,
                             levels = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                                        "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"))


# Add trend direction
map_data <- map_data %>%
  mutate( trend_dir = ifelse(trend_W >= 0, "Positive", "Negative"),
    sig_star = ifelse(p_value < 0.05, "*", NA)   ### NEW significance flag
  )

# 5. Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")
library(rnaturalearth)
library(sf)
# Download the global bounding box as an sf polygon (WGS84)
world_bb <- ne_download(category = "physical", type = "wgs84_bounding_box", returnclass = "sf")
sf::sf_use_s2(TRUE)

#Label helpers 
label_lon <- function(x) ifelse(x == 0, "0°",
                                paste0(abs(x), "°", ifelse(x < 0, "W", "E")))
label_lat <- function(y) ifelse(y == 0, "0°",
                                paste0(abs(y), "°", ifelse(y < 0, "S", "N")))

# Create label points (in WGS84); they get reprojected automatically 
# Lat labels at -60, -30, 0, 30, 60 placed near the left side
labs_y <- data.frame(lon = -170, lat = c(-60, -30, 0, 30, 60)) |>
  transform(label = label_lat(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Lon labels at -120, -60, 0, 60, 120 placed near the bottom
# (use -82 so they sit just inside the oval after projection)
labs_x <- data.frame(lon = c(-120, -60, 0, 60, 120), lat = -82) |>
  transform(label = label_lon(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

bin_colors <- c( "< -12"   = "#8B0000",   
                 "-12 – -8" = "red3",  
                 "-8 – -3"  = "#FFA500",  
                 "-3 – -1"  = "yellow2",  
                 "-1 – 0"   = "pink",  
                 "0 – 1"    = "#87CEFA",  
                 "1 – 3"    = "#4169E1",  
                 "3 – 8"    = "blue",  
                 "8 – 12"   = "blue3",  
                 "> 12"     = "darkblue" )

# Make a subset once
map_data_sig <- map_data %>% filter(p_value < 0.05)

plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "grey40", size = 0.1) +
  geom_sf_text(data = labs_y, aes(label = label), size = 4, color = "gray20") +
  geom_sf_text(data = labs_x, aes(label = label), size = 4, color = "gray20") +
  geom_sf(data = world_bb, fill = NA, color = "black", size = 0.5) +
  
  # base triangles (direction) with fill (bin)
  geom_sf(data = map_data,
          aes(fill = trend_bin, shape = trend_dir),
          size = 3, color = "black", stroke = 0.3 , show.legend = TRUE) +
  
  # --- significance overlay (no legend entries) ---
  # white circle halo
  geom_sf(data = map_data_sig,
          shape = 21, fill = "white", color = "black",
          size = 1.7, stroke = 0.3, inherit.aes = FALSE, show.legend = FALSE) +
  # black star centered on top
  geom_sf(data = map_data_sig,
          shape = 8, color = "black",
          size = 1, stroke = 0.1, inherit.aes = FALSE, show.legend = FALSE) +


scale_fill_manual(
  values = bin_colors,
  name = "Trend (mm/year)",
  breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
             "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
  labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
             "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
  drop = FALSE, na.translate = FALSE,
  guide = guide_legend( nrow= 1, override.aes = list(shape = 21, color = "black", size = 4))
) +
  scale_shape_manual(
    values = c("Positive" = 24, "Negative" = 25),
    name = "Trend Direction",
    guide = guide_legend(nrow = 1) , 
    breaks = c("Positive", "Negative")
  ) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 10)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray70", size = 0.1),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16) , 
    legend.position = "bottom",
    legend.title.position = "top",
    legend.text.position = "top",
    legend.spacing.x = unit(1.2 , "cm"),
    legend.box.spacing = unit(0.8, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.box = "horizontal")+
      guides(
        color = guide_legend(nrow = 1) , 
        shape = guide_legend(nrow = 1)# Direction (if needed)
      )


# --- Manual legend for significant trend direction (bottom-left inset) ---
library(patchwork)

# Keep the same circle:star ratio as on the map
circle_size_map <- 1.7
star_size_map   <- 1.0
ratio_star      <- star_size_map / circle_size_map  # ~0.588

# Choose triangle size and derive inner sizes so the circle fits nicely inside
triangle_size   <- 3
circle_size_inset <- triangle_size * 0.55
star_size_inset   <- circle_size_inset * ratio_star

legend_sig <- ggplot() +
  xlim(0, 1) + ylim(0, 1) +
  
  # Title
  annotate("text", x = 0.05, y = 0.95, label = "Significant Trend Direction",
           hjust = 0, vjust = 1, size = 4.5) +
  
  # --- Row 1: Significant Positive (triangle up) ---
  annotate("point", x = 0.12, y = 0.60, shape = 24,  # triangle up
           size = triangle_size, fill = "white", color = "black", stroke = 0.4) +
  annotate("point", x = 0.12, y = 0.60, shape = 21,  # white circle halo
           size = circle_size_inset, fill = "white", color = "black", stroke = 0.3) +
  annotate("point", x = 0.12, y = 0.60, shape = 8,   # black star
           size = star_size_inset, color = "black", stroke = 0.1) +
  annotate("text",  x = 0.22, y = 0.60, label = "Significant Positive",
           hjust = 0, vjust = 0.35, size = 4) +
  
  # --- Row 2: Significant Negative (triangle down) ---
  annotate("point", x = 0.12, y = 0.30, shape = 25,  # triangle down
           size = triangle_size, fill = "white", color = "black", stroke = 0.4) +
  annotate("point", x = 0.12, y = 0.30, shape = 21,  # white circle halo
           size = circle_size_inset, fill = "white", color = "black", stroke = 0.3) +
  annotate("point", x = 0.12, y = 0.30, shape = 8,   # black star
           size = star_size_inset, color = "black", stroke = 0.1) +
  annotate("text",  x = 0.22, y = 0.30, label = "Significant Negative",
           hjust = 0, vjust = 0.35, size = 4) +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = scales::alpha("white", 0.92),
                                   color = "black", linewidth = 0.3)
  )

# Inset position: a bit lower-left than before
plot <- plot + inset_element(
  legend_sig,
  left   = 0.02,  # move rightward from left edge (0..1)
  bottom = 0.008,  # move upward from bottom edge
  right  = 0.20,  # controls width
  top    = 0.15,  # controls height
  align_to = "panel"
)



ggsave(
  "F1_W_Trend.png",
  plot = plot,
  width = 16, height = 10,
  dpi = 800,
  bg = "white"
)

#In the Below code all four triangle appear 
#manual legend is added
#######################################################################################

library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(gridExtra)
library(cowplot)

# DATA PREP (assumes `data` exists) 
data_wide <- data %>% pivot_wider(names_from = variable, values_from = value)

data_terra <- data_wide %>%
  filter(data_source == "terraclimate") %>%
  mutate(W = tp - e)

water_trends <- data_terra %>%
  group_by(city, Continent) %>%
  summarise(
    trend_W = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop"
  )


sum(water_trends$p_value < 0.05)
map_data <- data_terra %>%
  left_join(water_trends, by = "city") %>%
  distinct(city, .keep_all = TRUE) %>%
  filter(!is.na(trend_W)) %>%
  arrange(desc(trend_W)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

map_data <- map_data %>%
  mutate(trend_bin = cut(trend_W,
                         breaks = c(-Inf, -12, -8, -3, -1, 0, 1, 3, 8, 12, Inf),
                         labels = c("< -12", "-12 – -8", "-8 – -3", "-3 – -1", "-1 – 0",
                                    "0 – 1", "1 – 3", "3 – 8", "8 – 12", "> 12"),
                         right = FALSE))

map_data$trend_bin <- factor(map_data$trend_bin,
                             levels = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                                        "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"))

map_data <- map_data %>%
  mutate(
    trend_dir = ifelse(trend_W >= 0, "Positive", "Negative"),
    p_sig = p_value < 0.05,
    sig_star = ifelse(p_sig, "*", NA),
    trend_cat = case_when(
      trend_W >= 0 & p_sig ~ "SP",
      trend_W >= 0 & !p_sig ~ "NSP",
      trend_W <  0 & p_sig ~ "SN",
      trend_W <  0 & !p_sig ~ "NSN"
    )
  )
map_data$trend_cat <- factor(map_data$trend_cat, levels = c("SP","NSP","SN","NSN"))

#  MAP + LABELS 
world <- ne_countries(scale = "medium", returnclass = "sf")
world_bb <- ne_download(category = "physical", type = "wgs84_bounding_box", returnclass = "sf")
sf::sf_use_s2(TRUE)

label_lon <- function(x) ifelse(x == 0, "0°",
                                paste0(abs(x), "°", ifelse(x < 0, "W", "E")))
label_lat <- function(y) ifelse(y == 0, "0°",
                                paste0(abs(y), "°", ifelse(y < 0, "S", "N")))

labs_y <- data.frame(lon = -170, lat = c(-60, -30, 0, 30, 60)) |>
  transform(label = label_lat(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

labs_x <- data.frame(lon = c(-120, -60, 0, 60, 120), lat = -82) |>
  transform(label = label_lon(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

bin_colors <- c(
  "< -12"    = "#8B0000",
  "-12 – -8" = "red3",
  "-8 – -3"  = "#FFA500",
  "-3 – -1"  = "yellow2",
  "-1 – 0"   = "pink",
  "0 – 1"    = "#87CEFA",
  "1 – 3"    = "#4169E1",
  "3 – 8"    = "blue",
  "8 – 12"   = "blue3",
  "> 12"     = "darkblue"
)

circle_size_map <- 1.7
star_size_map   <- 0.85

main_map <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "grey40", size = 0.1) +
  geom_sf_text(data = labs_y, aes(label = label), size = 4, color = "gray20") +
  geom_sf_text(data = labs_x, aes(label = label), size = 4, color = "gray20") +
  geom_sf(data = world_bb, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = map_data, aes(fill = trend_bin, shape = trend_cat),
          size = 3, color = "black", stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP","SN")),
          aes(shape = trend_cat), shape = 21, fill = "white", color = "black",
          size = circle_size_map, stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP","SN")),
          aes(shape = trend_cat), shape = 8, color = "black",
          size = star_size_map, stroke = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = bin_colors,
                    name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                               "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                               "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    drop = FALSE, na.translate = FALSE,
                    guide = guide_legend(nrow = 1,
                                         override.aes = list(shape = 21, color = "black", size = 4))) +
  scale_shape_manual(values = c("SP" = 24, "NSP" = 24, "SN" = 25, "NSN" = 25), guide = "none") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 10)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray70", size = 0.1),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title.position = "top",
    legend.text.position = "top",
    legend.spacing.x = unit(1.2 , "cm"),
    legend.box.spacing = unit(0.8, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.box = "horizontal"
  )

# Build fill legend (single-row) 
legend_for_fill <- ggplot(data = map_data) +
  geom_point(aes(x = 1, y = 1, fill = trend_bin), shape = 21, size = 4) +
  scale_fill_manual(values = bin_colors,
                    name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                               "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                               "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    guide = guide_legend(nrow = 1, override.aes = list(shape = 21, color = "black", size = 4))) +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.6, "cm"))

fill_legend_grob <- cowplot::get_legend(legend_for_fill)

# Compact custom legend (short labels) 
make_key_grob <- function(triangle_fill = "white",
                          show_circle = FALSE,
                          show_star = FALSE,
                          label_text = "",
                          tri_pch = 24,
                          base_pt = 7) {
  x <- unit(0.06, "npc"); y <- unit(0.5, "npc")
  tri <- pointsGrob(x = x, y = y, pch = tri_pch,
                    gp = gpar(col = "black", fill = triangle_fill, fontsize = base_pt * 2))
  grob_list <- list(tri)
  if (show_circle) {
    circ <- pointsGrob(x = x, y = y, pch = 21,
                       gp = gpar(col = "black", fill = "white", fontsize = base_pt * 1.2))
    grob_list <- c(grob_list, list(circ))
  }
  if (show_star) {
    star <- pointsGrob(x = x, y = y, pch = 8,
                       gp = gpar(col = "black", fontsize = base_pt * 1))
    grob_list <- c(grob_list, list(star))
  }
  txt <- textGrob(label_text, x = unit(0.30, "npc"), y = y, just = "left", gp = gpar(fontsize = 12))
  grob_list <- c(grob_list, list(txt))
  grobTree(children = do.call(gList, grob_list))
}

key_base_pt <- 7
key_SP  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SP",  tri_pch = 24, base_pt = key_base_pt)
key_NSP <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSP", tri_pch = 24, base_pt = key_base_pt)
key_SN  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SN",  tri_pch = 25, base_pt = key_base_pt)
key_NSN <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSN", tri_pch = 25, base_pt = key_base_pt)

legend_grob_custom <- arrangeGrob(grobs = list(key_SP, key_NSP, key_SN, key_NSN),
                                  ncol = 4, widths = rep(unit(1, "null"), 4), padding = unit(2, "pt"))
legend_title_custom <- textGrob("Trend Direction", gp = gpar(fontsize = 16))
legend_with_title_custom <- arrangeGrob(legend_title_custom, legend_grob_custom,
                                       ncol = 1, heights = unit.c(unit(10, "pt"), unit(1, "null")))

#  Convert both legends to ggdraw objects and ensure they occupy one horizontal cell 
left_legend_draw  <- ggdraw() + draw_grob(legend_with_title_custom, x = 0.05, y = 0.5, width = 1, height = 1)
# draw fill legend and right-align it within its cell so it hugs the right side
right_legend_draw <- ggdraw() + draw_grob(fill_legend_grob, x = 0.97, y = 0.5, width = 1, height = 1, hjust = 1)

# Arrange legends on ONE ROW, side-by-side 
gap_grob <- ggdraw() + theme_void()

# Arrange legends with a “gap” in the middle
legend_row <- plot_grid(
  left_legend_draw,  # left legend
  gap_grob,          # empty space
  right_legend_draw, # right legend
  ncol = 3,
  rel_widths = c(0.22, 0.03, 0.75),  # left : gap : right
  align = "h"
)                      

# Final assembly: map above, legend_row below (single row) 
final_plot <- plot_grid(ggdraw(main_map), legend_row, ncol = 1, rel_heights = c(1, 0.06))

# Save + display
ggsave("F1_terra_Water_Availability_Trend.png", final_plot, width = 16, height = 10, dpi = 800, bg = "white")

#########################################################################################################################
#Water avilabilty trend for Era-5-land 

library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(gridExtra)
library(cowplot)

#DATA PREP 
data_wide <- data %>% pivot_wider(names_from = variable, values_from = value)

data_era <- data_wide %>%
  filter(data_source == "era5-land") %>%
  mutate(W = tp - e)

water_trends <- data_era %>%
  group_by(city, Continent) %>%
  summarise(
    trend_W = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop"
  )

sum(water_trends$p_value < 0.05)

map_data <- data_era %>%
  left_join(water_trends, by = "city") %>%
  distinct(city, .keep_all = TRUE) %>%
  filter(!is.na(trend_W)) %>%
  arrange(desc(trend_W)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


summary(map_data$trend_W)

map_data <- map_data %>%
  mutate(trend_bin = cut(trend_W,
                         breaks = c(-Inf, -12, -8, -3, -1, 0, 1, 3, 8, 12, Inf),
                         labels = c("< -12", "-12 – -8", "-8 – -3", "-3 – -1", "-1 – 0",
                                    "0 – 1", "1 – 3", "3 – 8", "8 – 12", "> 12"),
                         right = FALSE))

map_data$trend_bin <- factor(map_data$trend_bin,
                             levels = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                                        "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"))

map_data <- map_data %>%
  mutate(
    trend_dir = ifelse(trend_W >= 0, "Positive", "Negative"),
    p_sig = p_value < 0.05,
    sig_star = ifelse(p_sig, "*", NA),
    trend_cat = case_when(
      trend_W >= 0 & p_sig ~ "SP",
      trend_W >= 0 & !p_sig ~ "NSP",
      trend_W <  0 & p_sig ~ "SN",
      trend_W <  0 & !p_sig ~ "NSN"
    )
  )
map_data$trend_cat <- factor(map_data$trend_cat, levels = c("SP","NSP","SN","NSN"))

# MAP + LABELS 
world <- ne_countries(scale = "medium", returnclass = "sf")
world_bb <- ne_download(category = "physical", type = "wgs84_bounding_box", returnclass = "sf")
sf::sf_use_s2(TRUE)

label_lon <- function(x) ifelse(x == 0, "0°",
                                paste0(abs(x), "°", ifelse(x < 0, "W", "E")))
label_lat <- function(y) ifelse(y == 0, "0°",
                                paste0(abs(y), "°", ifelse(y < 0, "S", "N")))

labs_y <- data.frame(lon = -170, lat = c(-60, -30, 0, 30, 60)) |>
  transform(label = label_lat(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

labs_x <- data.frame(lon = c(-120, -60, 0, 60, 120), lat = -82) |>
  transform(label = label_lon(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

bin_colors <- c(
  "< -12"    = "#8B0000",
  "-12 – -8" = "red3",
  "-8 – -3"  = "#FFA500",
  "-3 – -1"  = "yellow2",
  "-1 – 0"   = "pink",
  "0 – 1"    = "#87CEFA",
  "1 – 3"    = "#4169E1",
  "3 – 8"    = "blue",
  "8 – 12"   = "blue3",
  "> 12"     = "darkblue"
)

circle_size_map <- 1.7
star_size_map   <- 0.85

main_map <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "grey40", size = 0.1) +
  geom_sf_text(data = labs_y, aes(label = label), size = 4, color = "gray20") +
  geom_sf_text(data = labs_x, aes(label = label), size = 4, color = "gray20") +
  geom_sf(data = world_bb, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = map_data, aes(fill = trend_bin, shape = trend_cat),
          size = 3, color = "black", stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP","SN")),
          aes(shape = trend_cat), shape = 21, fill = "white", color = "black",
          size = circle_size_map, stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP","SN")),
          aes(shape = trend_cat), shape = 8, color = "black",
          size = star_size_map, stroke = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = bin_colors,
                    name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                               "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                               "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    drop = FALSE, na.translate = FALSE,
                    guide = guide_legend(nrow = 1,
                                         override.aes = list(shape = 21, color = "black", size = 4))) +
  scale_shape_manual(values = c("SP" = 24, "NSP" = 24, "SN" = 25, "NSN" = 25), guide = "none") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 10)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray70", size = 0.1),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title.position = "top",
    legend.text.position = "top",
    legend.spacing.x = unit(1.2 , "cm"),
    legend.box.spacing = unit(0.8, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.box = "horizontal"
  )

# Build fill legend (single-row) 
legend_for_fill <- ggplot(data = map_data) +
  geom_point(aes(x = 1, y = 1, fill = trend_bin), shape = 21, size = 4) +
  scale_fill_manual(values = bin_colors,
                    name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                               "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                               "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    guide = guide_legend(nrow = 1, override.aes = list(shape = 21, color = "black", size = 4))) +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.2, "cm"))

fill_legend_grob <- cowplot::get_legend(legend_for_fill)

# Compact custom legend (short labels) 
make_key_grob <- function(triangle_fill = "white",
                          show_circle = FALSE,
                          show_star = FALSE,
                          label_text = "",
                          tri_pch = 24,
                          base_pt = 7) {
  x <- unit(0.06, "npc"); y <- unit(0.5, "npc")
  tri <- pointsGrob(x = x, y = y, pch = tri_pch,
                    gp = gpar(col = "black", fill = triangle_fill, fontsize = base_pt * 2))
  grob_list <- list(tri)
  if (show_circle) {
    circ <- pointsGrob(x = x, y = y, pch = 21,
                       gp = gpar(col = "black", fill = "white", fontsize = base_pt * 1.2))
    grob_list <- c(grob_list, list(circ))
  }
  if (show_star) {
    star <- pointsGrob(x = x, y = y, pch = 8,
                       gp = gpar(col = "black", fontsize = base_pt * 1))
    grob_list <- c(grob_list, list(star))
  }
  txt <- textGrob(label_text, x = unit(0.30, "npc"), y = y, just = "left", gp = gpar(fontsize = 12))
  grob_list <- c(grob_list, list(txt))
  grobTree(children = do.call(gList, grob_list))
}

key_base_pt <- 7
key_SP  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SP",  tri_pch = 24, base_pt = key_base_pt)
key_NSP <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSP", tri_pch = 24, base_pt = key_base_pt)
key_SN  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SN",  tri_pch = 25, base_pt = key_base_pt)
key_NSN <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSN", tri_pch = 25, base_pt = key_base_pt)

legend_grob_custom <- arrangeGrob(grobs = list(key_SP, key_NSP, key_SN, key_NSN),
                                  ncol = 4, widths = rep(unit(1.8, "cm"), 4), padding = unit(6, "pt"))
legend_title_custom <- textGrob("Trend Direction", gp = gpar(fontsize = 16))
legend_with_title_custom <- arrangeGrob(legend_title_custom, legend_grob_custom,
                                        ncol = 1, heights = unit.c(unit(10, "pt"), unit(1, "null")))

# Convert both legends to ggdraw objects and ensure they occupy one horizontal cell 
left_legend_draw  <- ggdraw() + draw_grob(legend_with_title_custom, x = 0.05, y = 0.5, width = 1, height = 1)
# draw fill legend and right-align it within its cell so it hugs the right side
right_legend_draw <- ggdraw() + draw_grob(fill_legend_grob, x = 0.97, y = 0.5, width = 1, height = 1, hjust = 1)

# Arrange legends on ONE ROW, side-by-side
gap_grob <- ggdraw() + theme_void()

# Arrange legends with a “gap” in the middle
legend_row <- plot_grid(
  left_legend_draw,  # left legend
  gap_grob,          # empty space
  right_legend_draw, # right legend
  ncol = 3,
  rel_widths = c(0.22, 0.03, 0.75),  # left : gap : right
  align = "h"
)                      

# Final assembly: map above, legend_row below (single row) 
final_plot <- plot_grid(ggdraw(main_map), legend_row, ncol = 1, rel_heights = c(1, 0.06))

# Save + display
ggsave("F1_erra_Water_Availability_Trend.png", final_plot, width = 16, height = 10, dpi = 800, bg = "white")


#########################################################################################################################
#Water avilabilty trend for mswep/gleam

library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(gridExtra)
library(cowplot)


#naming both the datasets in a single one 
data1 <- data %>%
  mutate(data_source = ifelse(data_source %in% c("mswep-v2-8", "gleam-v4-1a"),
                              "mswep/gleam",
                              data_source))

summary(data1)
unique(data1$data_source)
unique(data1$variable)

#DATA PREP 
data_wide <- data1 %>% pivot_wider(names_from = variable, values_from = value)

data_mswepGleam <- data_wide %>%
  filter(data_source == "mswep/gleam") %>%
  mutate(W = tp - e)

water_trends <- data_mswepGleam %>%
  group_by(city, Continent) %>%
  summarise(
    trend_W = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop"
  )


sum(water_trends$p_value < 0.05)
map_data <- data_mswepGleam %>%
  left_join(water_trends, by = "city") %>%
  distinct(city, .keep_all = TRUE) %>%
  filter(!is.na(trend_W)) %>%
  arrange(desc(trend_W)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)



map_data <- map_data %>%
  mutate(trend_bin = cut(trend_W,
                         breaks = c(-Inf, -12, -8, -3, -1, 0, 1, 3, 8, 12, Inf),
                         labels = c("< -12", "-12 – -8", "-8 – -3", "-3 – -1", "-1 – 0",
                                    "0 – 1", "1 – 3", "3 – 8", "8 – 12", "> 12"),
                         right = FALSE))

map_data$trend_bin <- factor(map_data$trend_bin,
                             levels = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                                        "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"))

map_data <- map_data %>%
  mutate(
    trend_dir = ifelse(trend_W >= 0, "Positive", "Negative"),
    p_sig = p_value < 0.05,
    sig_star = ifelse(p_sig, "*", NA),
    trend_cat = case_when(
      trend_W >= 0 & p_sig ~ "SP",
      trend_W >= 0 & !p_sig ~ "NSP",
      trend_W <  0 & p_sig ~ "SN",
      trend_W <  0 & !p_sig ~ "NSN"
    )
  )
map_data$trend_cat <- factor(map_data$trend_cat, levels = c("SP","NSP","SN","NSN"))

# MAP + LABELS 
world <- ne_countries(scale = "medium", returnclass = "sf")
world_bb <- ne_download(category = "physical", type = "wgs84_bounding_box", returnclass = "sf")
sf::sf_use_s2(TRUE)

label_lon <- function(x) ifelse(x == 0, "0°",
                                paste0(abs(x), "°", ifelse(x < 0, "W", "E")))
label_lat <- function(y) ifelse(y == 0, "0°",
                                paste0(abs(y), "°", ifelse(y < 0, "S", "N")))

labs_y <- data.frame(lon = -170, lat = c(-60, -30, 0, 30, 60)) |>
  transform(label = label_lat(lat)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

labs_x <- data.frame(lon = c(-120, -60, 0, 60, 120), lat = -82) |>
  transform(label = label_lon(lon)) |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

bin_colors <- c(
  "< -12"    = "#8B0000",
  "-12 – -8" = "red3",
  "-8 – -3"  = "#FFA500",
  "-3 – -1"  = "yellow2",
  "-1 – 0"   = "pink",
  "0 – 1"    = "#87CEFA",
  "1 – 3"    = "#4169E1",
  "3 – 8"    = "blue",
  "8 – 12"   = "blue3",
  "> 12"     = "darkblue"
)

circle_size_map <- 1.7
star_size_map   <- 0.85

main_map <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "grey40", size = 0.1) +
  geom_sf_text(data = labs_y, aes(label = label), size = 4, color = "gray20") +
  geom_sf_text(data = labs_x, aes(label = label), size = 4, color = "gray20") +
  geom_sf(data = world_bb, fill = NA, color = "black", size = 0.5) +
  geom_sf(data = map_data, aes(fill = trend_bin, shape = trend_cat),
          size = 3, color = "black", stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP","SN")),
          aes(shape = trend_cat), shape = 21, fill = "white", color = "black",
          size = circle_size_map, stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP","SN")),
          aes(shape = trend_cat), shape = 8, color = "black",
          size = star_size_map, stroke = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = bin_colors,
                    name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                               "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                               "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    drop = FALSE, na.translate = FALSE,
                    guide = guide_legend(nrow = 1,
                                         override.aes = list(shape = 21, color = "black", size = 4))) +
  scale_shape_manual(values = c("SP" = 24, "NSP" = 24, "SN" = 25, "NSN" = 25), guide = "none") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 10)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray70", size = 0.1),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title.position = "top",
    legend.text.position = "top",
    legend.spacing.x = unit(1.2 , "cm"),
    legend.box.spacing = unit(0.8, "cm"),
    legend.key.width = unit(2, "cm"),
    legend.box = "horizontal"
  )

# Build fill legend (single-row) 
legend_for_fill <- ggplot(data = map_data) +
  geom_point(aes(x = 1, y = 1, fill = trend_bin), shape = 21, size = 4) +
  scale_fill_manual(values = bin_colors,
                    name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                               "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                               "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    guide = guide_legend(nrow = 1, override.aes = list(shape = 21, color = "black", size = 4))) +
  theme_void() +
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        legend.key.width = unit(1.2, "cm"))

fill_legend_grob <- cowplot::get_legend(legend_for_fill)

# Compact custom legend (short labels) 
make_key_grob <- function(triangle_fill = "white",
                          show_circle = FALSE,
                          show_star = FALSE,
                          label_text = "",
                          tri_pch = 24,
                          base_pt = 7) {
  x <- unit(0.06, "npc"); y <- unit(0.5, "npc")
  tri <- pointsGrob(x = x, y = y, pch = tri_pch,
                    gp = gpar(col = "black", fill = triangle_fill, fontsize = base_pt * 2))
  grob_list <- list(tri)
  if (show_circle) {
    circ <- pointsGrob(x = x, y = y, pch = 21,
                       gp = gpar(col = "black", fill = "white", fontsize = base_pt * 1.2))
    grob_list <- c(grob_list, list(circ))
  }
  if (show_star) {
    star <- pointsGrob(x = x, y = y, pch = 8,
                       gp = gpar(col = "black", fontsize = base_pt * 1))
    grob_list <- c(grob_list, list(star))
  }
  txt <- textGrob(label_text, x = unit(0.30, "npc"), y = y, just = "left", gp = gpar(fontsize = 12))
  grob_list <- c(grob_list, list(txt))
  grobTree(children = do.call(gList, grob_list))
}

key_base_pt <- 7
key_SP  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SP",  tri_pch = 24, base_pt = key_base_pt)
key_NSP <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSP", tri_pch = 24, base_pt = key_base_pt)
key_SN  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SN",  tri_pch = 25, base_pt = key_base_pt)
key_NSN <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSN", tri_pch = 25, base_pt = key_base_pt)

legend_grob_custom <- arrangeGrob(grobs = list(key_SP, key_NSP, key_SN, key_NSN),
                                  ncol = 4, widths = rep(unit(1.8, "cm"), 4), padding = unit(4, "pt"))
legend_title_custom <- textGrob("Trend Direction", gp = gpar(fontsize = 16))
legend_with_title_custom <- arrangeGrob(legend_title_custom, legend_grob_custom,
                                        ncol = 1, heights = unit.c(unit(10, "pt"), unit(1, "null")))

# Convert both legends to ggdraw objects and ensure they occupy one horizontal cell 
left_legend_draw  <- ggdraw() + draw_grob(legend_with_title_custom, x = 0.05, y = 0.5, width = 1, height = 1)
# draw fill legend and right-align it within its cell so it hugs the right side
right_legend_draw <- ggdraw() + draw_grob(fill_legend_grob, x = 0.97, y = 0.5, width = 1, height = 1, hjust = 1)

# Arrange legends on ONE ROW, side-by-side
gap_grob <- ggdraw() + theme_void()

# Arrange legends with a “gap” in the middle
legend_row <- plot_grid(
  left_legend_draw,  # left legend
  gap_grob,          # empty space
  right_legend_draw, # right legend
  ncol = 3,
  rel_widths = c(0.22, 0.03, 0.75),  # left : gap : right
  align = "h"
)                      

# Final assembly: map above, legend_row below (single row) 
final_plot <- plot_grid(ggdraw(main_map), legend_row, ncol = 1, rel_heights = c(1, 0.06))

# Save + display
ggsave("F1_MSWEPGleam_Water_Availability_Trend.png", final_plot, width = 16, height = 10, dpi = 800, bg = "white")

print(final_plot)


##############################################################################################
#10 highest and 10 lowest trend cities for water availability 
# Required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)
library(patchwork)  # for combining plots

# Step 1: Prepare data and compute W = P - E
data_w <- data1 %>%
  filter(data_source == "era5-land") %>%
  spread(key = variable, value = value) %>%
  mutate(W = tp - e)

# Step 2: Calculate Sen’s slope and p-value
sen_trends <- data_w %>%
  group_by(city) %>%
  summarise(
    slope = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop"
  )

# First plot: Top 10 increasing and decreasing cities
top_bottom <- sen_trends %>%
  arrange(desc(slope)) %>%
  slice_head(n = 10) %>%
  bind_rows(
    sen_trends %>% arrange(slope) %>% slice_head(n = 10)
  )
slope_range <- range(sen_trends$slope)
plot1 <- ggplot(top_bottom, aes(x = reorder(city, slope), y = slope, fill = slope)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradientn(
    colours = c("red3", "pink", "blue", "blue3"),  # 4 colors
    values = scales::rescale(c(-17, 0, 10, 22), from = slope_range),  # anchors
    limits = slope_range
  )  +
  labs(
    title = NULL, x = NULL,
    y = " (a) Overall Trends (Top 10 High & Low)", fill = "Trend" ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_text(size = 16, face = "bold"))

# Second plot: Statistically significant trends only (p < 0.05)
sig_trends <- sen_trends %>%
  filter(p_value < 0.05)
summary(sen_trends)

# Select top 10 positive and bottom 10 negative trends
# Only for the data from era5-land or mswep and gleam combined 
top_bottom_cities <- sig_trends %>%
  arrange(desc(slope)) %>%     # sort descending
  slice(1:10) %>%              # top 10 positive
  bind_rows(
    sig_trends %>%
      arrange(slope) %>%       # sort ascending
      slice(1:10)              # bottom 10 negative
  )

# Plot using only these cities
plot2 <- ggplot(top_bottom_cities, aes(x = reorder(city, slope), y = slope, fill = slope)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradientn(
    colours = c("red3", "pink", "blue", "blue3"),  # 4 colors
    values = scales::rescale(c(-17, 0, 10, 22), from = slope_range),  # anchors
    limits = slope_range
  ) +
  labs(
    title = NULL,
    x = NULL,
    y = "(b) Significant Trends (p < 0.05)",
    fill = "Trend mm/year"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    legend
  )

combined_plot <- plot1 | plot2   

ggsave(
  "F6_era_Highest_Lowest_W_Trend.png",
  plot  = combined_plot, dpi   = 800, width = 16, height = 10, bg    = "white")



###########################
# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)
library(patchwork)
library(purrr)

# Water availability for TerraClimate
data_w <- data1 %>%
  filter(data_source == "mswep/gleam") %>%
  spread(key = variable, value = value) %>%
  mutate(W = tp - e)

# Calculate Sen slope & p-value
sen_trends <- data_w %>%
  group_by(city) %>%
  summarise(
    slope = sens.slope(W)$estimates,
    p_value = sens.slope(W)$p.value,
    .groups = "drop"
  )

# full range of slopes
slope_range <- range(sen_trends$slope, na.rm = TRUE)

# Choose top 10 highest & 10 lowest
top_bottom <- sen_trends %>%
  arrange(desc(slope)) %>%
  slice(1:10) %>%
  bind_rows(
    sen_trends %>% arrange(slope) %>% slice(1:10)
  )

# Significant only
sig_trends <- sen_trends %>%
  filter(p_value < 0.05)
# Correct ordering for positive + negative slopes together
# Only significant cities, ordered by slope
top_bottom_cities <- sig_trends %>%
  arrange(desc(slope)) %>%
  slice(1:10) %>%
  bind_rows(
    sen_trends %>% arrange(slope) %>% slice(1:10)
  )


# Color scale
diverging_cols <- c(
  "red2",    # strong negative red
  "pink",    # moderate negative
  "#f7f7f7",    # zero grey
  "cyan",    # moderate positive cyan-blue
  "blue"     # strong positive deep blue
)

# ---------------- PLOT 1 (Top/Bottom) ------------------
plot1 <- ggplot(top_bottom,
                aes(x = reorder(city, slope), y = slope, fill = slope)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = 0.02)) +
  scale_fill_gradientn(
    colours = diverging_cols,
    values = scales::rescale(c(min(slope_range), -5, 0, 5, max(slope_range))),
    name = "Trend (mm/year)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )

# ---------------- PLOT 2 (Significant only) ------------------
plot2 <- ggplot(top_bottom_cities,
                aes(x = reorder(city, slope), y = slope, fill = slope)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = 0.02)) +
  scale_fill_gradientn(
    colours = diverging_cols,
    values = scales::rescale(c(min(slope_range), -5, 0, 5, max(slope_range))),
    name = "Trend (mm/year)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 14)
  )

# -------- Combine & save --------
combined_plot <- plot1 + plot2 
ggsave(
  "F6_mswepgleam_WaterTrend_Highest_Lowest.png",
  plot = combined_plot, dpi = 800, width = 16, height = 10, bg = "white"
)



library(dplyr)
library(ggplot2)
library(trend)

# Prepare significant cities, ordered by slope
sig_trends <- sen_trends %>%
  filter(p_value < 0.05) %>%
  arrange(desc(slope)) %>%
  mutate(city = factor(city, levels = city))  # preserve order

# Plot
ggplot(sig_trends, aes(x = slope, y = city)) +
  geom_col(fill = "lightgray", width = 0.6) +        # bars
  geom_text(aes(label = city), 
            hjust = 0.5, color = "black", size = 4) +  # city name inside bar
  geom_text(aes(label = round(slope, 2), x = slope), 
            hjust = -0.1, color = "black", size = 4) + # trend outside
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Trend (mm/year)", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_blank(),  # hide default y labels
        axis.ticks.y = element_blank())

