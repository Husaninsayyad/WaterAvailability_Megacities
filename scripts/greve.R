library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(cowplot)
library(grid)
# Periods
climate_periods <- list(
  "1980-2001" = c(1980, 2001),
  "2002-2023" = c(2002, 2023))


unique(data1$data_source)
# Helper function to compute Budyko period
compute_budyko_period <- function(data1, period_name, period_range) {
  data1 %>%
    filter(year >= period_range[1], year <= period_range[2], data_source == "mswep/gleam") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    group_by(city, lat, lon, country, Continent) %>%
    summarise(
      e = mean(e, na.rm = TRUE),
      tp = mean(tp, na.rm = TRUE),
      pet = mean(pet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      PET.P = pet / tp, 
      AET.P = e / tp,
      W = tp - e,
      time_period = period_name )
}

# Combine periods
data_budyko_combined <- bind_rows(lapply(names(climate_periods), function(p) {
  compute_budyko_period(data1, p, climate_periods[[p]])
}))


# Wide format for Δ(P-E) and trend
data_wide <- data_budyko_combined %>%
  select(city, lat, lon, country, Continent, time_period, W, PET.P, AET.P) %>%
  pivot_wider(names_from = time_period, values_from = c(W, PET.P, AET.P)) %>%
  mutate(
    delta_W = `W_2002-2023` - `W_1980-2001`,
    trend_direction = ifelse(delta_W >= 0, "Positive", "Negative")
  ) %>%
  filter(!is.na(delta_W)) 

# Long format for plotting points
plot_data <- data_wide %>%
  pivot_longer(
    cols = matches("^(PET\\.P|AET\\.P)_"),
    names_to = c("variable", "time_period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    time_period = gsub("\u2013|\u2014", "-", time_period),
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023"))
  )


# Wide format for arrows + slope categories (updated logic for right/left arrows)
plot_wide <- plot_data %>%
  select(city, PET.P, AET.P, time_period, trend_direction) %>%
  pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  { 
    pad <- 0.005
    mutate(.,
           dx = `PET.P_2002-2023` - `PET.P_1980-2001`,
           dy = `AET.P_2002-2023` - `AET.P_1980-2001`,
           slope = dy / dx,
           norm = sqrt(dx^2 + dy^2),
           ux = ifelse(norm > 0, dx / norm, 0),
           uy = ifelse(norm > 0, dy / norm, 0),
           xend_adj = `PET.P_2002-2023` - ux * pad,
           yend_adj = `AET.P_2002-2023` - uy * pad,
           arrow_group = case_when(
             dx > 0 & abs(slope) > 1  ~ "Right steep (>1)",
             dx > 0 & abs(slope) <= 1 ~ "Right mild (<1)",
             dx < 0 & abs(slope) > 1  ~ "Left steep (>1)",
             dx < 0 & abs(slope) <= 1 ~ "Left mild (<1)",
             TRUE ~ "No change"))}

# Arrow colors according to right/left + slope magnitude
arrow_colors <- c(
  "Right steep (>1)" = "brown",
  "Right mild (<1)"  = "coral1",
  "Left steep (>1)"  = "blue4",
  "Left mild (<1)"   = "cyan3",
  "No change"        = "grey50")

# Moving city categories (E->W / W->E)
plot_wide <- plot_wide %>%
  mutate(
    start_limit = ifelse(`PET.P_1980-2001` < 1, "EL", "WL"),
    end_limit = ifelse(`PET.P_2002-2023` < 1, "EL", "WL"),
    category = paste(start_limit, "→", end_limit) )


# Inset barplot
category_summary <- plot_wide %>%
  filter(category %in% c("EL → WL", "WL → EL")) %>%
  count(category) %>%
  mutate(percentage = n / nrow(plot_wide) * 100)

inset_plot <- ggplot(category_summary, aes(x = category, y = percentage, fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0,30), expand = c(0, 0)) +
  labs(y = "Percentage (%)", x = NULL) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "none")

# Budyko curves
budyko_curve_x <- seq(0.01, 3, by = 0.05)
budyko_curve_y <- (budyko_curve_x * tanh(1 / budyko_curve_x) * (1 - exp(-budyko_curve_x)))^0.5
df_budyko <- data.frame(x = budyko_curve_x, y = budyko_curve_y)
df_energy <- data.frame(x = seq(0, 1, by = 0.05), y = seq(0, 1, by = 0.05))
df_waterR <- data.frame(x = seq(1, 3, by = 0.05), y = 1)
df_diag   <- data.frame(x = seq(0, 1, by = 0.05), y = seq(0, 1, by = 0.05))

# Main Budyko plot
BUDYKO <- ggplot() +
  geom_line(data = df_budyko, aes(x = x, y = y), linetype = "dashed") +
  geom_line(data = df_energy, aes(x = y, y = x), color = "black") +
  geom_line(data = df_waterR, aes(x = x, y = y), color = "black") +
  geom_line(data = df_diag,   aes(x = x, y = y), linetype = "dotted", color = "black") +
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1), color = "grey50", linewidth = 0.8) +
  
  # Arrows with updated slope & direction categories
  geom_segment(
    data = plot_wide,
    aes(x = `PET.P_1980-2001`, y = `AET.P_1980-2001`, 
        xend = xend_adj, yend = yend_adj, color = arrow_group),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    alpha = 0.9, lineend = "round", linewidth = 0.55 ) +
  geom_point(
    data = plot_data,
    aes(x = PET.P, y = AET.P),
    shape= 21, size = 2.2, alpha = 0.70, color = "black", stroke = 0.2 , fill = "lightgrey"  ) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5) ) +
  annotate(
    "rect" , xmin = 0 , ymin = 0 , xmax = 2.624 , ymax = 1.5 , 
    color = "black" , fill = NA , linewidth = 0.8
  ) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)") +
  theme_minimal() +
  theme(
    plot.margin = margin(15, 80, 15 , 15),
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16) )

# Just rename the legend labels for the arrows
BUDYKO <- BUDYKO +
  scale_color_manual(
    name = "Water Availability",  # Legend title
    values = arrow_colors, # Keep the same colors
    labels = c(
      "Right steep (>1)" = "Strong water loss",
      "Right mild (<1)"  = "Mild water loss",
      "Left steep (>1)"  = "Strong water gain",
      "Left mild (<1)"   = "Mild water gain",
      "No change"        = "No significant change") )

#only use the bellow code to write the name of the cities, nothing else 
library(ggrepel)

BUDYKO_SIG <- BUDYKO +
  geom_text_repel(
    data = plot_wide , 
    aes(
      x = xend_adj, y = yend_adj, label = city
    ),
    size       = 3,         # text size
    color      = "black",   # label color
    segment.color = NA,     # no extra line connecting
    box.padding = 0.25,     # padding around labels
    point.padding = 0.25,   # space from the arrow tip
    max.overlaps = Inf,     # allow all labels
    min.segment.length = 0  # ensures repulsion works even for close points
  )
# Combine main plot & inset
final_plot <- ggdraw() +
  draw_plot(BUDYKO) +
  draw_plot(inset_plot, x = 0.08, y = 0.62, width = 0.18, height = 0.35)
# Save
ggsave("F5_mswep_h.png", plot = BUDYKO, dpi = 300, width = 16, height = 10, bg = "white")


