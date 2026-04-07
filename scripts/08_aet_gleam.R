#08_aet_gleam.R
#Evapotranspiration trend map by continent - Gleam-v4-1a Dataset

library(dplyr)
library(tidyr)
library(trend)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)     

sf::sf_use_s2(TRUE)

# Data Gleam-v4-1a
data_gleam <- data1 %>%
  filter(data_source == "gleam-v4-1a")

trend_gleam <- data_gleam %>%
  group_by(city, lon, lat, variable) %>%
  summarise(
    Trend   = sens.slope(value)$estimates,
    p_value = sens.slope(value)$p.value,
    .groups = "drop"
  )
gleam_e <- trend_gleam %>% filter(variable == "e")
summary(gleam_e$Trend)
world <- ne_countries(scale = "medium", returnclass = "sf")
spatial_pts <- st_as_sf(gleam_e, coords = c("lon", "lat"), crs = 4326)
idx <- sf::st_nearest_feature(spatial_pts, world)
spatial_joined <- cbind(spatial_pts, world[idx, "region_un"])

continent_data <- spatial_joined %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  rename(Continent = region_un) %>%
  mutate(
    y tolerance band around zero
    trend_dir = case_when(
      Trend <= -0.0001 ~ "Negative",
      Trend >=  0.00001 ~ "Positive",
      TRUE ~ NA_character_
    ),
    sig_star = ifelse(p_value < 0.05, "*", NA_character_)
  )

global_min <- min(continent_data$Trend, na.rm = TRUE)
global_max <- max(continent_data$Trend, na.rm = TRUE)
min_val <- round(global_min, 2)
max_val <- round(global_max, 2)
trend_breaks_asc <- c(min_val, -2.50, -1.00, 0.00, 1.00, 2.50, Inf)
lab_neg3 <- paste0(format(min_val, nsmall = 2), " – -2.50")
lab_neg2 <- "-2.50 – -1.00"
lab_neg1 <- "-1.00 – 0.00"
lab_pos1 <- "0.00 – 1.00"
lab_pos2 <- "1.00 – 2.50"
lab_pos3 <- paste0("2.50 – ", format(max_val, nsmall = 2))

trend_labels_asc       <- c(lab_neg3, lab_neg2, lab_neg1, lab_pos1, lab_pos2, lab_pos3)
trend_labels_pos_first <- c(lab_pos3, lab_pos2, lab_pos1, lab_neg1, lab_neg2, lab_neg3)  # legend order

# Colors named to POSITIVE-FIRST order
trend_colors <- c("#8B0000", "#FF0000", "pink1", "yellow", "blue1", "darkblue")
names(trend_colors) <- trend_labels_pos_first


# (controls legend order)
continent_data <- continent_data %>%
  mutate(
    Trend_bin = cut(
      Trend,
      breaks = trend_breaks_asc,
      labels = trend_labels_asc,
      right = FALSE,
      include.lowest = TRUE
    ),
    Trend_bin = factor(Trend_bin, levels = trend_labels_pos_first)
  )

# Viewport bounds per continent 
continent_bounds <- list(
  Africa   = list(xmin = -20,  xmax =  55, ymin = -35, ymax =  40),
  Asia     = list(xmin =  25,  xmax = 150, ymin =   0, ymax =  60),
  Europe   = list(xmin = -25,  xmax =  40, ymin =  35, ymax =  72),
  Americas = list(xmin = -170, xmax = -30, ymin = -60, ymax =  70),
  # Oceania zoomed wider 
  Oceania  = list(
    xmin = 144.9631 - 30, 
    xmax = 151.2100 + 30,  
    ymin = -37.8142 - 20,  
    ymax = -33.8678 + 30   
  )
)

#  Plot function  
create_continent_plot <- function(data, world_map, continent_name, bounds) {
  ggplot() +
    geom_sf(data = world_map, fill = "grey95", color = "grey40", linewidth = 0.2) +
    geom_point(
      data = data %>% filter(Continent == continent_name, !is.na(trend_dir)),
      aes(x = lon, y = lat, fill = Trend_bin, shape = trend_dir),
      size = 3, color = "black", stroke = 0.3
    ) +
    geom_point(
      data = data %>% filter(Continent == continent_name, !is.na(sig_star)),
      aes(x = lon, y = lat),
      shape = 21, size = 1.7, fill = "white", color = "black", stroke = 0.3,
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    geom_text(
      data = data %>% filter(Continent == continent_name, !is.na(sig_star)),
      aes(x = lon, y = lat),
      label = "\u2605", size = 1.0, color = "black", fontface = "bold",
      inherit.aes = FALSE, show.legend = FALSE
    ) +
    scale_fill_manual(
      values = trend_colors,
      limits = trend_labels_pos_first,
      drop = FALSE, na.translate = FALSE
    ) +
    scale_shape_manual(
      values = c("Positive" = 24, "Negative" = 25),
      breaks = c("Positive", "Negative")
    ) +
    
    labs(title = continent_name) +
    coord_sf(
      xlim = c(bounds$xmin, bounds$xmax),
      ylim = c(bounds$ymin, bounds$ymax),
      expand = FALSE
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.title = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"  # IMPORTANT: hide legends on panels
    )
}

# Build per-continent panels 
plot_africa   <- create_continent_plot(continent_data, world, "Africa",   continent_bounds$Africa)
plot_asia     <- create_continent_plot(continent_data, world, "Asia",     continent_bounds$Asia)
plot_europe   <- create_continent_plot(continent_data, world, "Europe",   continent_bounds$Europe)
plot_americas <- create_continent_plot(continent_data, world, "Americas", continent_bounds$Americas)
plot_oceania  <- create_continent_plot(continent_data, world, "Oceania",  continent_bounds$Oceania)

#Build ONE master legend via a dummy plot 
legend_fill_df  <- data.frame(Trend_bin = factor(trend_labels_pos_first, levels = trend_labels_pos_first), x = 1, y = 1)
legend_shape_df <- data.frame(trend_dir = c("Positive", "Negative"), x = 1, y = 1)

library(cowplot)
library(grid)  

legend_fill_df  <- data.frame(
  Trend_bin = factor(trend_labels_pos_first, levels = trend_labels_pos_first),
  x = 1, y = 1
)
legend_shape_df <- data.frame(trend_dir = c("Positive", "Negative"), x = 1, y = 1)

base_leg_theme <- theme(
  legend.title       = element_text(size = 16),
  legend.text        = element_text(size = 14),
  legend.key.height  = unit(8, "mm"),
  legend.key.width   = unit(8, "mm"),
  legend.spacing.y   = unit(5, "pt")
)

dummy_fill <- ggplot() +
  geom_point(
    data = legend_fill_df,
    aes(x = x, y = y, fill = Trend_bin),
    shape = 22, size = 5, color = "black", stroke = 0.3, show.legend = TRUE
  ) +
  scale_fill_manual(
    values = trend_colors,
    limits = trend_labels_pos_first,
    labels = c("2.50 – 8.5",              
               "1 – 2.50",
               "0 – 1.0",
               "0 – -1.0",
               "-1 – -2.50",
               "-2.50 – -4.5"),
    drop = FALSE, na.translate = FALSE,
    name = "Trend (mm/year)"
  ) +
  guides(fill = guide_legend(order = 1)) +
  theme_void() + theme(legend.position = "right") + base_leg_theme

leg_fill <- cowplot::get_legend(dummy_fill)

# Shape legend (triangles)
dummy_shape <- ggplot() +
  geom_point(
    data = legend_shape_df,
    aes(x = x, y = y, shape = trend_dir),
    size = 5, color = "black", fill = "white", stroke = 0.3, show.legend = TRUE
  ) +
  scale_shape_manual(
    values = c("Positive" = 24, "Negative" = 25),
    breaks = c("Positive", "Negative"),
    name   = "Trend Direction"
  ) +
  guides(shape = guide_legend(order = 2, override.aes = list(fill = "white"))) +
  theme_void() + theme(legend.position = "right") + base_leg_theme

leg_shape <- cowplot::get_legend(dummy_shape)

library(grid)
library(gridExtra)   
library(cowplot)     

#build a single key (triangle + optional circle/star + label) 
make_key_grob <- function(triangle_fill = "white",
                          show_circle = FALSE,
                          show_star = FALSE,
                          label_text = "",
                          tri_pch = 24,
                          base_pt = 7) {
  x <- unit(0.08, "npc"); y <- unit(0.5, "npc")
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

# build four small keys and combine into a titled legend grob 
key_base_pt <- 7
key_SP  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SP",  tri_pch = 24, base_pt = key_base_pt)
key_NSP <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSP", tri_pch = 24, base_pt = key_base_pt)
key_SN  <- make_key_grob(triangle_fill = "white", show_circle = TRUE,  show_star = TRUE,  label_text = "SN",  tri_pch = 25, base_pt = key_base_pt)
key_NSN <- make_key_grob(triangle_fill = "white", show_circle = FALSE, show_star = FALSE, label_text = "NSN", tri_pch = 25, base_pt = key_base_pt)

legend_grob_custom <- arrangeGrob(grobs = list(key_SP, key_NSP, key_SN, key_NSN),
                                  ncol = 1, heights = rep(unit(27, "pt"), 4))
legend_title_custom <- textGrob("Trend Direction", gp = gpar(fontsize = 16) ,
                                x = unit(0.30, "npc"),  y= unit(0.5, "npc"),
                                just = c("center", "top"))
legend_with_title_custom <- arrangeGrob(legend_title_custom, legend_grob_custom,
                                        padding = unit(0, "line"), ncol = 1)

bottom_legend_draw <- ggdraw() + draw_grob(legend_with_title_custom,
                                           x = 0.3, y = 0.62, 
                                           width = 0.4, height = 0.48,
                                           hjust = 0.5, vjust = 0.5)

trend_legend_col <- cowplot::plot_grid(leg_fill, bottom_legend_draw, ncol = 2, rel_heights = c(1, 1), align = "v")
legend_panel <- cowplot::ggdraw(trend_legend_col)

place_legend <- function(position = c("bottom_right","bottom_center","top_right"),
                         tag_size = 12) {
  position <- match.arg(position)
  
  panels <- list(
    plot_africa, plot_asia, plot_europe,
    plot_americas, plot_oceania, legend_panel
  )
  # Tags for those panels (legend slot gets blank)
  panel_tags <- c("(a)", "(b)", "(c)", "(d)", "(e)", "")
  
  # slots (3 cols × 2 rows)
  order <- switch(position,
                  bottom_right  = c(1, 2, 3, 4, 5, 6),  
                  bottom_center = c(1, 2, 3, 4, 6, 5),  
                  top_right     = c(1, 2, 6, 3, 4, 5))  
  
  cowplot::plot_grid(
    plotlist = panels[order],
    ncol = 3, nrow = 2, align = "hv",
    labels = panel_tags[order],             
    label_size = tag_size,
    label_fontface = "bold",
    label_colour = "black",
    label_x = 0.02, label_y = 0.98,         # top-left inside each panel
    hjust = 0, vjust = 1
  )
}

final_plot <- place_legend("bottom_right")
ggsave(
  "outputs/F2_gleam_Evapotranspiration_Trend.png",
  plot  = final_plot,
  dpi   = 300,
  width = 16, height = 10,
  bg    = "white"
)