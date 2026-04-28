# 04_W_era5.R
# Water availability trend map - ERA5-Land

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

data <- readRDS("data/data_final.rds")

data_wide <- data %>% 
  pivot_wider(names_from = variable, values_from = value)

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
          "0 – 1", "1 – 3", "3 – 8", "8 – 12", "> 12"),right = FALSE))

map_data$trend_bin <- factor(map_data$trend_bin,
          levels = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
          "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"))

# SP = significant positive, NSP = non-significant positive, 
#SN = significant negative, NSN = non-significant negative
map_data <- map_data %>%
  mutate(
    trend_dir = ifelse(trend_W >= 0, "Positive", "Negative"),
    p_sig     = p_value < 0.05,
    sig_star  = ifelse(p_sig, "*", NA),
    trend_cat = case_when(
      trend_W >= 0 & p_sig  ~ "SP",
      trend_W >= 0 & !p_sig ~ "NSP",
      trend_W <  0 & p_sig  ~ "SN",
      trend_W <  0 & !p_sig ~ "NSN"
    )
  )

map_data$trend_cat <- factor(map_data$trend_cat, 
                             levels = c("SP", "NSP", "SN", "NSN"))

world    <- ne_countries(scale = "medium", returnclass = "sf")
world_bb <- ne_download(category = "physical", type = "wgs84_bounding_box", 
                        returnclass = "sf")
sf::sf_use_s2(TRUE)

label_lon <- function(x) ifelse(x == 0, "0°", paste0(abs(x), "°", 
                                                     ifelse(x < 0, "W", "E")))
label_lat <- function(y) ifelse(y == 0, "0°", paste0(abs(y), "°", 
                                                     ifelse(y < 0, "S", "N")))

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
  # significance overlay: white circle halo + black star
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP", "SN")),
          aes(shape = trend_cat), shape = 21, fill = "white", color = "black",
          size = circle_size_map, stroke = 0.3, show.legend = FALSE) +
  geom_sf(data = dplyr::filter(map_data, trend_cat %in% c("SP", "SN")),
          aes(shape = trend_cat), shape = 8, color = "black",
          size = star_size_map, stroke = 0.1, show.legend = FALSE) +
  scale_fill_manual(values = bin_colors, name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                     "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                    "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
                    drop = FALSE, na.translate = FALSE,
guide = guide_legend(nrow = 1, override.aes = list(shape = 21, 
                                                   color = "black", 
                                                   size = 4))) +
  scale_shape_manual(values = c("SP" = 24, "NSP" = 24, "SN" = 25, "NSN" = 25), 
                     guide = "none") +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, 20)) +
  scale_y_continuous(breaks = seq(-90, 90, 10)) +
  theme_minimal() +
  theme(
    panel.grid.major      = element_line(color = "gray70", size = 0.1),
    panel.grid.minor      = element_blank(),
    axis.text             = element_blank(),
    axis.title            = element_blank(),
    legend.title          = element_text(size = 20),
    legend.text           = element_text(size = 16),
    legend.title.position = "top",
    legend.text.position  = "top",
    legend.spacing.x      = unit(1.2, "cm"),
    legend.box.spacing    = unit(0.8, "cm"),
    legend.key.width      = unit(2, "cm"),
    legend.box            = "horizontal"
  )

# build fill legend separately to have full control over layout
legend_for_fill <- ggplot(data = map_data) +
  geom_point(aes(x = 1, y = 1, fill = trend_bin), shape = 21, size = 4) +
  scale_fill_manual(values = bin_colors, name = "Trend (mm/year)",
                    breaks = c("> 12", "8 – 12", "3 – 8", "1 – 3", "0 – 1",
                     "-1 – 0", "-3 – -1", "-8 – -3", "-12 – -8", "< -12"),
                    labels = c("> 12", "12 – 8", "8 – 3", "3 – 1", "1 – 0",
                    "0 – -1", "-1 – -3", "-3 – -8", "-8 – -12", "< -12"),
guide = guide_legend(nrow = 1, override.aes = list(shape = 21,
                                                   color = "black", 
                                                   size = 4))) +
  theme_void() +
  theme(legend.title     = element_text(size = 16),
        legend.text      = element_text(size = 12),
        legend.key.width = unit(1.2, "cm"))

fill_legend_grob <- cowplot::get_legend(legend_for_fill)

# custom legend grob for trend direction symbols
make_key_grob <- function(triangle_fill = "white", show_circle = FALSE, 
                          show_star = FALSE,
                          label_text = "", tri_pch = 24, base_pt = 7) {
  x <- unit(0.06, "npc"); y <- unit(0.5, "npc")
  tri <- pointsGrob(x = x, y = y, pch = tri_pch,
                    gp = gpar(col = "black", fill = triangle_fill, 
                              fontsize = base_pt * 2))
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
  txt <- textGrob(label_text, x = unit(0.30, "npc"), y = y, just = "left", 
                  gp = gpar(fontsize = 12))
  grob_list <- c(grob_list, list(txt))
  grobTree(children = do.call(gList, grob_list))
}

key_base_pt <- 7
key_SP  <- make_key_grob(triangle_fill = "white", show_circle = TRUE, 
show_star = TRUE,  label_text = "SP",  tri_pch = 24, base_pt = key_base_pt)
key_NSP <- make_key_grob(triangle_fill = "white", show_circle = FALSE,
show_star = FALSE, label_text = "NSP", tri_pch = 24, base_pt = key_base_pt)
key_SN  <- make_key_grob(triangle_fill = "white", show_circle = TRUE, 
show_star = TRUE,  label_text = "SN",  tri_pch = 25, base_pt = key_base_pt)
key_NSN <- make_key_grob(triangle_fill = "white", show_circle = FALSE, 
show_star = FALSE, label_text = "NSN", tri_pch = 25, base_pt = key_base_pt)

legend_grob_custom <- arrangeGrob(grobs = list(key_SP, key_NSP, 
                                               key_SN, key_NSN),
 ncol = 4, widths = rep(unit(1.8, "cm"), 4), padding = unit(6, "pt"))
legend_title_custom <- textGrob("Trend Direction", gp = gpar(fontsize = 16))
legend_with_title_custom <- arrangeGrob(legend_title_custom, 
                                        legend_grob_custom,
 ncol = 1, heights = unit.c(unit(10, "pt"), unit(1, "null")))

left_legend_draw  <- ggdraw() + draw_grob(legend_with_title_custom,
                                          x = 0.05, y = 0.5, width = 1,
                                          height = 1)
right_legend_draw <- ggdraw() + draw_grob(fill_legend_grob, 
                                          x = 0.97, y = 0.5, width = 1, 
                                          height = 1, hjust = 1)

# place both legends side by side in one row below the map
legend_row <- plot_grid(
  left_legend_draw,  # left legend
  ggdraw() + theme_void(),  # empty space
  right_legend_draw, # right legend
  ncol = 3,
  rel_widths = c(0.22, 0.03, 0.75),
  align = "h"
)

final_plot <- plot_grid(ggdraw(main_map), 
                        legend_row, ncol = 1, rel_heights = c(1, 0.06))

ggsave("outputs/F1_erra_Water_Availability_Trend.png",
       final_plot, width = 16, height = 10, dpi = 800, bg = "white")