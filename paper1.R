getwd()
my_wd <- getwd()
setwd(my_wd)
setwd('C:/Users/Admin/OneDrive - CZU v Praze/Plocha/Thesis/Paper1/With_Mswep')
library(dplyr)
library(readxl)
# reading the data sent by Vishal 
data <- readRDS('C:/Users/Admin/OneDrive - CZU v Praze/Plocha/Thesis/Paper1/p_e_pet_cities_data_final_corrected_tp.rds')
continent_data <- read_excel("continent_data_clean.xlsx")

data <- data %>%
  mutate(year = format(as.Date(date), "%Y")) %>%  
  select(-date)  
data <- data %>%
  left_join(continent_data %>% select(city , Continent), by = "city")
data <- data %>%
  filter(data_source != "mswx-past")

#changing the name of mswep and gleam into one common name 
unique(data1$data_source)
data1 %>% distinct(data_source, variable)
data1 <- data %>%
  mutate(data_source = recode(data_source,
                              "gleam-v4-1a" = "mswep/gleam",
                              "mswep-v2-8" = "mswep/gleam"))

#Below is Boxplot 
###########################################################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)

# pick a plausible time column to align mswep (P) with GLEAM (ET)
TIME_COL <- names(data)[names(data) %in% c("year","Year","date","time")][1]
if (is.na(TIME_COL)) stop("Couldn't find a time column named year/Year/date/time.")

# function: W trend when P and ET are in the SAME source (TERRA, ERA5)
w_trend_same_source <- function(df_source, label){
  df_source %>%
    select(city, country, lon, lat, year, variable, value) %>%   # <-- use your time col (year)
    pivot_wider(names_from = variable, values_from = value) %>%
    # your variables are called "tp" and "e"
    mutate(W = tp - e) %>%
    group_by(city, country, lon, lat) %>%
    summarise(Trend = sens.slope(W)$estimates, .groups = "drop") %>%
    mutate(Dataset = label)
}


w_trend_cross_source <- function(df_p, df_et, label){
  left_join(
    df_p %>%
      filter(variable == "tp") %>%
      select(city, country, lon, lat, year, P = value),
    df_et %>%
      filter(variable == "e") %>%
      select(city, country, lon, lat, year, ET = value),
    by = c("city","country","lon","lat","year")
  ) %>%
    mutate(W = P - ET) %>%
    group_by(city, country, lon, lat) %>%
    summarise(Trend = sens.slope(W)$estimates, .groups = "drop") %>%
    mutate(Dataset = label)
}

unique(data$data_source)
#split data sources 

data_terra <- data %>% filter(data_source == "terraclimate")
data_era   <- data %>% filter(data_source == "era5-land")
data_mswep  <- data %>% filter(data_source == "mswep-v2-8")
data_gleam <- data %>% filter(data_source == "gleam-v4-1a")

#P & E trends as I already did 

trend_terra <- data_terra %>%
  group_by(city, country, lon, lat, variable) %>%
  summarise(Trend = sens.slope(value)$estimates, .groups = "drop")

trend_era <- data_era %>%
  group_by(city, country, lon, lat, variable) %>%
  summarise(Trend = sens.slope(value)$estimates, .groups = "drop")

trend_mswep <- data_mswep %>%
  group_by(city, country, lon, lat, variable) %>%
  summarise(Trend = sens.slope(value)$estimates, .groups = "drop")

trend_gleam <- data_gleam %>%
  group_by(city, country, lon, lat, variable) %>%
  summarise(Trend = sens.slope(value)$estimates, .groups = "drop")

terra_tp <- trend_terra %>% filter(variable == "tp") %>%
  transmute(city, country, `TERRA P` = Trend)
terra_et <- trend_terra %>% filter(variable == "e")  %>%
  transmute(city, country, `TERRA ET` = Trend)

era_tp   <- trend_era   %>% filter(variable == "tp") %>%
  transmute(city, country, `ERA5L P` = Trend)
era_et   <- trend_era   %>% filter(variable == "e")  %>%
  transmute(city, country, `ERA5L ET` = Trend)

mswep_tp  <- trend_mswep  %>% filter(variable == "tp") %>%
  transmute(city, country, `mswep P` = Trend)

gleam_et <- trend_gleam %>% filter(variable == "e") %>%
  transmute(city, country, `GLEAM ET` = Trend)

#W trends (computed from time series) 

terra_w <- w_trend_same_source(data_terra, "TERRA W")
era_w   <- w_trend_same_source(data_era,   "ERA5L W")
mswep_gleam_w <- w_trend_cross_source(data_mswep, data_gleam, "mswep/GLEAM W")

terra_w <- terra_w %>% transmute(city, country, `TERRA W` = Trend)
era_w   <- era_w   %>% transmute(city, country, `ERA5L W` = Trend)
mswep_gleam_w <- mswep_gleam_w %>% transmute(city, country, `mswep/GLEAM W` = Trend)

#combine, reshape, order 

combined_df <- terra_tp %>%
  inner_join(era_tp,        by = c("city","country")) %>%
  inner_join(mswep_tp,       by = c("city","country")) %>%
  inner_join(terra_et,      by = c("city","country")) %>%
  inner_join(era_et,        by = c("city","country")) %>%
  inner_join(gleam_et,      by = c("city","country")) %>%
  inner_join(terra_w,       by = c("city","country")) %>%
  inner_join(era_w,         by = c("city","country")) %>%
  inner_join(mswep_gleam_w,  by = c("city","country"))

long_df <- combined_df %>%
  pivot_longer(
    cols = c(`TERRA P`,`ERA5L P`,`mswep P`,
             `TERRA ET`,`ERA5L ET`,`GLEAM ET`,
             `TERRA W`,`ERA5L W`,`mswep/GLEAM W`),
    names_to = "Dataset", values_to = "Trend"
  )

# desired order P (TERRA, ERA, mswep)  ET (TERRA, ERA, GLEAM)  W (TERRA, ERA, mswep/GLEAM)
long_df$Dataset <- factor(long_df$Dataset,
                          levels = c("TERRA P","ERA5L P","mswep P",
                                     "TERRA ET","ERA5L ET","GLEAM ET",
                                     "TERRA W","ERA5L W","mswep/GLEAM W")
)

# quantity (for colors): P / ET / W
long_df$Quantity <- case_when(
  grepl(" P$",  long_df$Dataset) ~ "P",
  grepl(" ET$", long_df$Dataset) ~ "ET",
  TRUE                            ~ "W"
)

tick_labs <- c(
  "TERRA P"        = "TerraClimate",
  "ERA5L P"        = "ERA5-Land",
  "mswep P"         = "MSWEP-v2-8",
  "TERRA ET"       = "TerraClimate",
  "ERA5L ET"       = "ERA5-Land",
  "GLEAM ET"       = "GLEAM v4.1a",
  "TERRA W"        = "TerraClimate",
  "ERA5L W"        = "ERA5-Land",
  "mswep/GLEAM W"   = "MSWEP+GLEAM"
)

qty_cols <- c(P = "#D55E00" , ET = "#0072B2", W = "#009E73")  # orange/red, blue, green

BOXPLOT <- ggplot(long_df, aes(x = Dataset, y = Trend, fill = Quantity)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.95 , linewidth = 0.3 , width= 0.5) +
  scale_fill_manual(values = qty_cols, name = NULL,breaks = c("P", "ET", "W"),      # desired legend order (matches box order)
                    labels = c("P", "AET", "W")) +
  scale_x_discrete(labels = tick_labs)+
  labs(x = NULL, y = "Trend (mm/year)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 , size = 14),
    axis.text.y = element_text(size = 14) , 
    legend.position = "bottom"  )

ggsave(
  "F4_Boxplot.png", plot  = BOXPLOT, dpi   = 600 ,width = 16, height = 10, bg    = "white")

#Below is Budyko Framework
#########################################################################################
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
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023")),
    trend_direction = ifelse(city %in% data_wide$city[data_wide$delta_W >= 0], "Positive", "Negative"),
    trend_direction = factor(trend_direction, levels = c("Positive", "Negative"))
  )

# Wide format for arrows + slope categories (updated logic for right/left arrows)
plot_wide <- plot_data %>%
  select(city, PET.P, AET.P, time_period, trend_direction) %>%
  pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  { 
    pad <- 0.02
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
    start_limit = ifelse(`PET.P_1980-2001` < 1, "E", "W"),
    end_limit = ifelse(`PET.P_2002-2023` < 1, "E", "W"),
    category = paste(start_limit, "→", end_limit) )

# Inset barplot
category_summary <- plot_wide %>%
  filter(category %in% c("E → W", "W → E")) %>%
  count(category) %>%
  mutate(percentage = n / nrow(plot_wide) * 100)

inset_plot <- ggplot(category_summary, aes(x = category, y = percentage, fill = category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 12), expand = c(0, 0)) +
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
  scale_color_manual(values = arrow_colors, name = "Slope categories") +
  geom_point(
    data = plot_data,
    aes(x = PET.P, y = AET.P, shape = trend_direction),
    size = 1.8, alpha = 0.70, color = "black", stroke = 0.2 , fill = "lightgrey"  ) +
  scale_shape_manual( name = "Trend Direction", values = c("Positive" = 24, "Negative" = 25)) +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5)) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)") +
  theme_minimal() +
  theme(
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
    name = "Water Trend",  # Legend title
    values = arrow_colors, # Keep the same colors
    labels = c(
      "Right steep (>1)" = "Strong water loss",
      "Right mild (<1)"  = "Mild water loss",
      "Left steep (>1)"  = "Strong water gain",
      "Left mild (<1)"   = "Mild water gain",
      "No change"        = "No significant change") )

# Combine main plot & inset
final_plot <- ggdraw() +
  draw_plot(BUDYKO) +
  draw_plot(inset_plot, x = 0.07, y = 0.65, width = 0.15, height = 0.4)
# Save
ggsave("F5_mswepgleam_Budyko_Two_Periods_withInset.png", plot = final_plot, dpi = 300, width = 16, height = 10, bg = "white")

################################################################################################
# Select significant cities (overall) 
# If using Sen's slope results:
sig_cities <- water_trends %>%
  filter(p_value < 0.05) %>%
  pull(city) %>%
  unique()
summary(sig_cities)

# Rebuild Budyko data but ONLY for significant cities 
data_budyko_combined_sig <- bind_rows(lapply(names(climate_periods), function(p) {
  compute_budyko_period(data, p, climate_periods[[p]])
})) %>%
  dplyr::filter(city %in% sig_cities)

# Wide for Δ(P−E) & direction
data_wide_sig <- data_budyko_combined_sig %>%
  dplyr::select(city, lat, lon, country, Continent, time_period, W, PET.P, AET.P) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(W, PET.P, AET.P)) %>%
  dplyr::mutate(
    delta_W = `W_2002-2023` - `W_1980-2001`,
    trend_direction = ifelse(delta_W >= 0, "Positive", "Negative")
  ) %>%
  dplyr::filter(!is.na(delta_W))

# Long for plotting (two periods per city)
plot_data_sig <- data_wide_sig %>%
  tidyr::pivot_longer(
    cols = matches("^(PET\\.P|AET\\.P)_"),
    names_to = c("variable", "time_period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::mutate(
    time_period = gsub("\u2013|\u2014", "-", time_period),
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023")),
    trend_direction = ifelse(city %in% data_wide_sig$city[data_wide_sig$delta_W >= 0], "Positive", "Negative"),
    trend_direction = factor(trend_direction, levels = c("Positive", "Negative"))
  )

# For arrows: start -> end
plot_wide_sig <- plot_data_sig %>%
  dplyr::select(city, PET.P, AET.P, time_period, trend_direction) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  {
    pad <- 0.02
    dplyr::mutate(.,
                  dx   = `PET.P_2002-2023` - `PET.P_1980-2001`,
                  dy   = `AET.P_2002-2023` - `AET.P_1980-2001`,
                  norm = sqrt(dx^2 + dy^2),
                  ux   = ifelse(norm > 0, dx / norm, 0),
                  uy   = ifelse(norm > 0, dy / norm, 0),
                  xend_adj = `PET.P_2002-2023` - ux * pad,
                  yend_adj = `AET.P_2002-2023` - uy * pad
    )
  }

# Make the identical plot but with *_sig data 
BUDYKO_SIG <- ggplot() +
  # Theory curves (reuse df_budyko, df_energy, df_waterR, df_diag from your script)
  geom_line(data = df_budyko, aes(x = x, y = y), linetype = "dashed") +
  geom_line(data = df_energy, aes(x = y, y = x), color = "black") +
  geom_line(data = df_waterR, aes(x = x, y = y), color = "black") +
  geom_line(data = df_diag,   aes(x = x, y = y), linetype = "dotted", color = "black") +
  
  # Change vectors
  geom_segment(
    data = plot_wide_sig,
    aes(
      x = `PET.P_1980-2001`, y = `AET.P_1980-2001`,
      xend = xend_adj,        yend = yend_adj
    ),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    color     = "grey30",
    alpha     = 0.7,
    lineend   = "round",
    linewidth = 0.45
  ) +
  
  # Points (identical aesthetics & scales as original)
  geom_point(
    data = plot_data_sig,
    aes(x = PET.P, y = AET.P, fill = time_period, shape = trend_direction),
    size = 2.6, alpha = 0.85, color = "black", stroke = 0.25
  ) +
  scale_fill_manual(values = period_fills, name = "Time Period") +
  scale_shape_manual(values = c("Positive" = 24, "Negative" = 25),
                     breaks = c("Positive", "Negative"),
                     name   = "Trend Direction") +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5)) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)", title = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = element_blank() , 
    legend.title = element_text(size = 18),   # legend title size
    legend.text  = element_text(size = 14) , 
    axis.title.x = element_text(size = 18),   # "Aridity Index (PET/P)"
    axis.title.y = element_text(size = 18) , 
    axis.text.x = element_text(size = 16) ,
    axis.text.y = element_text(size = 16)
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  geom_segment(
    aes(x = 0, y = 0, xend = 0.3, yend = 0.3, linetype = "Trajectory"),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    color = "grey30",
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(values = c("Trajectory" = "solid"), name = NULL)

# Make sure ggrepel is installed
# install.packages("ggrepel")
library(ggrepel)

BUDYKO_SIG <- BUDYKO_SIG +
  geom_text_repel(
    data = plot_wide_sig,
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

ggsave(
  "F5_Terra_Budyko_Two_Periods_SIG.png",
  plot  = BUDYKO_SIG, dpi   = 600, width = 16, height = 10,
  bg    = "white")

##########################################################################################
#for era5-land (always check water trend)
# Budyko: two periods with vectors and styled legends, with period-label normalization 

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

#periods
climate_periods <- list(
  "1980-2001" = c(1980, 2001),
  "2002-2023" = c(2002, 2023))

#Helper: compute period means & ratios (TerraClimate only) 
compute_budyko_period <- function(data, period_name, period_range) {
  data %>%
    filter(
      year >= period_range[1], year <= period_range[2],
      data_source == "era5-land"
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    group_by(city, lat, lon, country, Continent) %>%
    summarise(
      e   = mean(e,   na.rm = TRUE),
      tp  = mean(tp,  na.rm = TRUE),
      pet = mean(pet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(   PET.P       = pet / tp,  AET.P       = e   / tp,   W   = tp - e,
      time_period = period_name
    )
}

#Build both periods 
data_budyko_combined <- bind_rows(lapply(names(climate_periods), function(p) {
  compute_budyko_period(data, p, climate_periods[[p]])
}))

#Wide for Δ(P−E) & direction 
data_wide <- data_budyko_combined %>%
  select(city, lat, lon, country, Continent, time_period, W, PET.P, AET.P) %>%
  pivot_wider(names_from = time_period, values_from = c(W, PET.P, AET.P)) %>%
  mutate(
    delta_W = `W_2002-2023` - `W_1980-2001`,
    trend_direction = ifelse(delta_W >= 0, "Positive", "Negative")
  ) %>%
  filter(!is.na(delta_W))

#Long for plotting (two periods per city)
plot_data <- data_wide %>%
  pivot_longer(
    cols = matches("^(PET\\.P|AET\\.P)_"),
    names_to = c("variable", "time_period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    #Normalize any en dash/em dash to hyphen to ensure scale labels match ---
    time_period = gsub("\u2013|\u2014", "-", time_period),
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023")),
    trend_direction = ifelse(city %in% data_wide$city[data_wide$delta_W >= 0], "Positive", "Negative"),
    trend_direction = factor(trend_direction, levels = c("Positive", "Negative"))
  )

#For arrows: wide (start -> end) 
plot_wide <- plot_data %>%
  select(city, PET.P, AET.P, time_period, trend_direction) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  { 
    pad <- 0.02  # arrow tip pull-back (closer than before)
    mutate(.,
           dx   = `PET.P_2002-2023` - `PET.P_1980-2001`,
           dy   = `AET.P_2002-2023` - `AET.P_1980-2001`,
           norm = sqrt(dx^2 + dy^2),
           ux   = ifelse(norm > 0, dx / norm, 0),
           uy   = ifelse(norm > 0, dy / norm, 0),
           xend_adj = `PET.P_2002-2023` - ux * pad,
           yend_adj = `AET.P_2002-2023` - uy * pad
    )
  }

#Budyko guide curves 
budyko_curve_x <- seq(0.01, 3, by = 0.05)
budyko_curve_y <- (budyko_curve_x * tanh(1 / budyko_curve_x) * (1 - exp(-budyko_curve_x)))^0.5

df_budyko <- data.frame(x = budyko_curve_x, y = budyko_curve_y)                   # Budyko curve
df_energy <- data.frame(x = seq(0, 1, by = 0.05), y = seq(0, 1, by = 0.05))       # Energy limit (AET/P = PET/P for x<=1)
df_waterR <- data.frame(x = seq(1, 3, by = 0.05), y = 1)                          # Water limit from RIGHT, ending at (1,1)
df_diag   <- data.frame(x = seq(0, 1, by = 0.05), y = seq(0, 1, by = 0.05))       # 1:1 within [0,1]

#Aesthetics 
period_fills <- c("1980-2001" = "skyblue", "2002-2023" = "pink3")

#Plot
BUDYKO <- ggplot() +
  # Theory curves
  geom_line(data = df_budyko, aes(x = x, y = y), linetype = "dashed") +
  geom_line(data = df_energy, aes(x = y, y = x), color = "black") +
  geom_line(data = df_waterR, aes(x = x, y = y), color = "black") +      # right side only; meets energy at (1,1)
  geom_line(data = df_diag,   aes(x = x, y = y), linetype = "dotted", color = "black") +
  # Change vectors (start 1980–2001 -> end 2002–2023), darker grey, tip pulled back
  geom_segment(
    data = plot_wide,
    aes(
      x = `PET.P_1980-2001`, y = `AET.P_1980-2001`,
      xend = xend_adj,        yend = yend_adj
    ),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    color     = "grey30",
    alpha     = 0.7,
    lineend   = "round",
    linewidth = 0.45
  ) +
  
  # Points (fill = time period, shape = trend direction). Triangles 24/25 use fill.
  geom_point(
    data = plot_data,
    aes(x = PET.P, y = AET.P, fill = time_period, shape = trend_direction),
    size = 2.6, alpha = 0.85, color = "black", stroke = 0.25
  ) +
  
  # Scales: your period colors; Positive listed above Negative in legend
  scale_fill_manual(values = period_fills, name = "Time Period") +
  scale_shape_manual(
    values = c("Positive" = 24, "Negative" = 25),
    breaks = c("Positive", "Negative"),
    name   = "Trend Direction"
  ) +
  
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5)) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)", title = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = element_blank() ,
    legend.title = element_text(size = 18),   # legend title size
    legend.text  = element_text(size = 14) , 
    axis.title.x = element_text(size = 18),   # "Aridity Index (PET/P)"
    axis.title.y = element_text(size = 18) , 
    axis.text.x = element_text(size = 16) ,
    axis.text.y = element_text(size = 16)
  )

BUDYKO <- BUDYKO +
  guides(
    fill  = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  geom_segment(
    aes(x = 0, y = 0, xend = 0.3, yend = 0.3, linetype = "Trajectory"),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    color = "grey30",
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(values = c("Trajectory" = "solid"), name = NULL)


ggsave(
  "F5_Era_Budyko_Two_Periods.png",
  plot  = BUDYKO,
  dpi   = 300,
  width = 16, height = 10,
  bg    = "white")

################################################################################################
# Select significant cities (overall) 
# If using Sen's slope results:
sig_cities <- water_trends %>%
  filter(p_value < 0.05) %>%
  pull(city) %>%
  unique()
summary(sig_cities)


# Rebuild Budyko data but ONLY for significant cities 
data_budyko_combined_sig <- bind_rows(lapply(names(climate_periods), function(p) {
  compute_budyko_period(data, p, climate_periods[[p]])
})) %>%
  dplyr::filter(city %in% sig_cities)

# Wide for Δ(P−E) & direction
data_wide_sig <- data_budyko_combined_sig %>%
  dplyr::select(city, lat, lon, country, Continent, time_period, W, PET.P, AET.P) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(W, PET.P, AET.P)) %>%
  dplyr::mutate(
    delta_W = `W_2002-2023` - `W_1980-2001`,
    trend_direction = ifelse(delta_W >= 0, "Positive", "Negative")
  ) %>%
  dplyr::filter(!is.na(delta_W))

# Long for plotting (two periods per city)
plot_data_sig <- data_wide_sig %>%
  tidyr::pivot_longer(
    cols = matches("^(PET\\.P|AET\\.P)_"),
    names_to = c("variable", "time_period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::mutate(
    time_period = gsub("\u2013|\u2014", "-", time_period),
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023")),
    trend_direction = ifelse(city %in% data_wide_sig$city[data_wide_sig$delta_W >= 0], "Positive", "Negative"),
    trend_direction = factor(trend_direction, levels = c("Positive", "Negative"))
  )

# For arrows: start -> end
plot_wide_sig <- plot_data_sig %>%
  dplyr::select(city, PET.P, AET.P, time_period, trend_direction) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  {
    pad <- 0.02
    dplyr::mutate(.,
                  dx   = `PET.P_2002-2023` - `PET.P_1980-2001`,
                  dy   = `AET.P_2002-2023` - `AET.P_1980-2001`,
                  norm = sqrt(dx^2 + dy^2),
                  ux   = ifelse(norm > 0, dx / norm, 0),
                  uy   = ifelse(norm > 0, dy / norm, 0),
                  xend_adj = `PET.P_2002-2023` - ux * pad,
                  yend_adj = `AET.P_2002-2023` - uy * pad
    )
  }

# Make the identical plot but with *_sig data 
BUDYKO_SIG <- ggplot() +
  # Theory curves (reuse df_budyko, df_energy, df_waterR, df_diag from your script)
  geom_line(data = df_budyko, aes(x = x, y = y), linetype = "dashed") +
  geom_line(data = df_energy, aes(x = y, y = x), color = "black") +
  geom_line(data = df_waterR, aes(x = x, y = y), color = "black") +
  geom_line(data = df_diag,   aes(x = x, y = y), linetype = "dotted", color = "black") +
  
  # Change vectors
  geom_segment(
    data = plot_wide_sig,
    aes(
      x = `PET.P_1980-2001`, y = `AET.P_1980-2001`,
      xend = xend_adj,        yend = yend_adj
    ),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    color     = "grey30",
    alpha     = 0.7,
    lineend   = "round",
    linewidth = 0.45
  ) +
  
  # Points (identical aesthetics & scales as original)
  geom_point(
    data = plot_data_sig,
    aes(x = PET.P, y = AET.P, fill = time_period, shape = trend_direction),
    size = 2.6, alpha = 0.85, color = "black", stroke = 0.25
  ) +
  scale_fill_manual(values = period_fills, name = "Time Period") +
  scale_shape_manual(values = c("Positive" = 24, "Negative" = 25),
                     breaks = c("Positive", "Negative"),
                     name   = "Trend Direction") +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5)) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)", title = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = element_blank() , 
    legend.title = element_text(size = 18),   # legend title size
    legend.text  = element_text(size = 14) , 
    axis.title.x = element_text(size = 18),   # "Aridity Index (PET/P)"
    axis.title.y = element_text(size = 18) , 
    axis.text.x = element_text(size = 16) ,
    axis.text.y = element_text(size = 16)
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  geom_segment(
    aes(x = 0, y = 0, xend = 0.3, yend = 0.3, linetype = "Trajectory"),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    color = "grey30",
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(values = c("Trajectory" = "solid"), name = NULL)

# Make sure ggrepel is installed
# install.packages("ggrepel")
library(ggrepel)

BUDYKO_SIG <- BUDYKO_SIG +
  geom_text_repel(
    data = plot_wide_sig,
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


ggsave(
  "F5_Era_Budyko_Two_Periods_SIG.png",
  plot  = BUDYKO_SIG,
  dpi   = 600,
  width = 16, height = 10,
  bg    = "white"
)


#########################################################################################
#for gleam and mswep (always check water trend)

# Budyko: two periods with vectors and styled legends, with period-label normalization 

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

#periods
climate_periods <- list(
  "1980-2001" = c(1980, 2001),
  "2002-2023" = c(2002, 2023)
)

#Helper: compute period means & ratios (TerraClimate only) 
unique(data1$data_source)

compute_budyko_period <- function(data, period_name, period_range) {
  data1 %>%
    filter(
      year >= period_range[1], year <= period_range[2],
      data_source == "mswep/gleam"
    ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    group_by(city, lat, lon, country, Continent) %>%
    summarise(
      e   = mean(e,   na.rm = TRUE),
      tp  = mean(tp,  na.rm = TRUE),
      pet = mean(pet, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      PET.P       = pet / tp,
      AET.P       = e   / tp,
      W   = tp - e,
      time_period = period_name
    )
}

#Build both periods 
data_budyko_combined <- bind_rows(lapply(names(climate_periods), function(p) {
  compute_budyko_period(data, p, climate_periods[[p]])
}))

#Wide for Δ(P−E) & direction 
data_wide <- data_budyko_combined %>%
  select(city, lat, lon, country, Continent, time_period, W, PET.P, AET.P) %>%
  pivot_wider(names_from = time_period, values_from = c(W, PET.P, AET.P)) %>%
  mutate(
    delta_W = `W_2002-2023` - `W_1980-2001`,
    trend_direction = ifelse(delta_W >= 0, "Positive", "Negative")
  ) %>%
  filter(!is.na(delta_W))

#Long for plotting (two periods per city)
plot_data <- data_wide %>%
  pivot_longer(
    cols = matches("^(PET\\.P|AET\\.P)_"),
    names_to = c("variable", "time_period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    #Normalize any en dash/em dash to hyphen to ensure scale labels match ---
    time_period = gsub("\u2013|\u2014", "-", time_period),
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023")),
    trend_direction = ifelse(city %in% data_wide$city[data_wide$delta_W >= 0], "Positive", "Negative"),
    trend_direction = factor(trend_direction, levels = c("Positive", "Negative"))
  )

#For arrows: wide (start -> end) 
plot_wide <- plot_data %>%
  select(city, PET.P, AET.P, time_period, trend_direction) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  { 
    pad <- 0.02  # arrow tip pull-back (closer than before)
    mutate(.,
           dx   = `PET.P_2002-2023` - `PET.P_1980-2001`,
           dy   = `AET.P_2002-2023` - `AET.P_1980-2001`,
           norm = sqrt(dx^2 + dy^2),
           ux   = ifelse(norm > 0, dx / norm, 0),
           uy   = ifelse(norm > 0, dy / norm, 0),
           xend_adj = `PET.P_2002-2023` - ux * pad,
           yend_adj = `AET.P_2002-2023` - uy * pad
    )
  }

#Budyko guide curves 
budyko_curve_x <- seq(0.01, 3, by = 0.05)
budyko_curve_y <- (budyko_curve_x * tanh(1 / budyko_curve_x) * (1 - exp(-budyko_curve_x)))^0.5

df_budyko <- data.frame(x = budyko_curve_x, y = budyko_curve_y)                   # Budyko curve
df_energy <- data.frame(x = seq(0, 1, by = 0.05), y = seq(0, 1, by = 0.05))       # Energy limit (AET/P = PET/P for x<=1)
df_waterR <- data.frame(x = seq(1, 3, by = 0.05), y = 1)                          # Water limit from RIGHT, ending at (1,1)
df_diag   <- data.frame(x = seq(0, 1, by = 0.05), y = seq(0, 1, by = 0.05))       # 1:1 within [0,1]

#Aesthetics 
period_fills <- c("1980-2001" = "skyblue", "2002-2023" = "pink3")

#Plot
BUDYKO <- ggplot() +
  # Theory curves
  geom_line(data = df_budyko, aes(x = x, y = y), linetype = "dashed") +
  geom_line(data = df_energy, aes(x = y, y = x), color = "black") +
  geom_line(data = df_waterR, aes(x = x, y = y), color = "black") +      # right side only; meets energy at (1,1)
  geom_line(data = df_diag,   aes(x = x, y = y), linetype = "dotted", color = "black") +
  # Change vectors (start 1980–2001 -> end 2002–2023), darker grey, tip pulled back
  geom_segment(
    data = plot_wide,
    aes(
      x = `PET.P_1980-2001`, y = `AET.P_1980-2001`,
      xend = xend_adj,        yend = yend_adj
    ),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    color     = "grey30",
    alpha     = 0.7,
    lineend   = "round",
    linewidth = 0.45
  ) +
  
  # Points (fill = time period, shape = trend direction). Triangles 24/25 use fill.
  geom_point(
    data = plot_data,
    aes(x = PET.P, y = AET.P, fill = time_period, shape = trend_direction),
    size = 2.6, alpha = 0.85, color = "black", stroke = 0.25
  ) +
  
  # Scales: your period colors; Positive listed above Negative in legend
  scale_fill_manual(values = period_fills, name = "Time Period") +
  scale_shape_manual(
    values = c("Positive" = 24, "Negative" = 25),
    breaks = c("Positive", "Negative"),
    name   = "Trend Direction"
  ) +
  
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5)) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)", title = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = element_blank() ,
    legend.title = element_text(size = 18),   # legend title size
    legend.text  = element_text(size = 14) , 
    axis.title.x = element_text(size = 18),   # "Aridity Index (PET/P)"
    axis.title.y = element_text(size = 18) , 
    axis.text.x = element_text(size = 16) ,
    axis.text.y = element_text(size = 16)
  )

BUDYKO <- BUDYKO +
  guides(
    fill  = guide_legend(override.aes = list(shape = 21, color = "black"))
  )+
  geom_segment(
    aes(x = 0, y = 0, xend = 0.3, yend = 0.3, linetype = "Trajectory"),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    color = "grey30",
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(values = c("Trajectory" = "solid"), name = NULL)


ggsave(
  "F5_mswepGleam_Budyko_Two_Periods.png",
  plot  = BUDYKO,
  dpi   = 300,
  width = 16, height = 10,
  bg    = "white"
)

################################################################################################
# Select significant cities (overall) 
# If using Sen's slope results:
sig_cities <- water_trends %>%
  filter(p_value < 0.05) %>%
  pull(city) %>%
  unique()
summary(sig_cities)


# Rebuild Budyko data but ONLY for significant cities 
data_budyko_combined_sig <- bind_rows(lapply(names(climate_periods), function(p) {
  compute_budyko_period(data, p, climate_periods[[p]])
})) %>%
  dplyr::filter(city %in% sig_cities)

# Wide for Δ(P−E) & direction
data_wide_sig <- data_budyko_combined_sig %>%
  dplyr::select(city, lat, lon, country, Continent, time_period, W, PET.P, AET.P) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(W, PET.P, AET.P)) %>%
  dplyr::mutate(
    delta_W = `W_2002-2023` - `W_1980-2001`,
    trend_direction = ifelse(delta_W >= 0, "Positive", "Negative")
  ) %>%
  dplyr::filter(!is.na(delta_W))

# Long for plotting (two periods per city)
plot_data_sig <- data_wide_sig %>%
  tidyr::pivot_longer(
    cols = matches("^(PET\\.P|AET\\.P)_"),
    names_to = c("variable", "time_period"),
    names_sep = "_",
    values_to = "value"
  ) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::mutate(
    time_period = gsub("\u2013|\u2014", "-", time_period),
    time_period = factor(time_period, levels = c("1980-2001", "2002-2023")),
    trend_direction = ifelse(city %in% data_wide_sig$city[data_wide_sig$delta_W >= 0], "Positive", "Negative"),
    trend_direction = factor(trend_direction, levels = c("Positive", "Negative"))
  )

# For arrows: start -> end
plot_wide_sig <- plot_data_sig %>%
  dplyr::select(city, PET.P, AET.P, time_period, trend_direction) %>%
  tidyr::pivot_wider(names_from = time_period, values_from = c(PET.P, AET.P)) %>%
  {
    pad <- 0.02
    dplyr::mutate(.,
                  dx   = `PET.P_2002-2023` - `PET.P_1980-2001`,
                  dy   = `AET.P_2002-2023` - `AET.P_1980-2001`,
                  norm = sqrt(dx^2 + dy^2),
                  ux   = ifelse(norm > 0, dx / norm, 0),
                  uy   = ifelse(norm > 0, dy / norm, 0),
                  xend_adj = `PET.P_2002-2023` - ux * pad,
                  yend_adj = `AET.P_2002-2023` - uy * pad
    )
  }

# Make the identical plot but with *_sig data 
BUDYKO_SIG <- ggplot() +
  # Theory curves (reuse df_budyko, df_energy, df_waterR, df_diag from your script)
  geom_line(data = df_budyko, aes(x = x, y = y), linetype = "dashed") +
  geom_line(data = df_energy, aes(x = y, y = x), color = "black") +
  geom_line(data = df_waterR, aes(x = x, y = y), color = "black") +
  geom_line(data = df_diag,   aes(x = x, y = y), linetype = "dotted", color = "black") +
  
  # Change vectors
  geom_segment(
    data = plot_wide_sig,
    aes(
      x = `PET.P_1980-2001`, y = `AET.P_1980-2001`,
      xend = xend_adj,        yend = yend_adj
    ),
    arrow     = arrow(length = unit(0.18, "cm"), type = "closed"),
    color     = "grey30",
    alpha     = 0.7,
    lineend   = "round",
    linewidth = 0.45
  ) +
  
  # Points (identical aesthetics & scales as original)
  geom_point(
    data = plot_data_sig,
    aes(x = PET.P, y = AET.P, fill = time_period, shape = trend_direction),
    size = 2.6, alpha = 0.85, color = "black", stroke = 0.25
  ) +
  scale_fill_manual(values = period_fills, name = "Time Period") +
  scale_shape_manual(values = c("Positive" = 24, "Negative" = 25),
                     breaks = c("Positive", "Negative"),
                     name   = "Trend Direction") +
  coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 1.5)) +
  labs(x = "Aridity Index (PET/P)", y = "Evaporation Index (AET/P)", title = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted", linewidth = 0.5),
    panel.grid.minor = element_blank() , 
    legend.title = element_text(size = 18),   # legend title size
    legend.text  = element_text(size = 14) , 
    axis.title.x = element_text(size = 18),   # "Aridity Index (PET/P)"
    axis.title.y = element_text(size = 18) , 
    axis.text.x = element_text(size = 16) ,
    axis.text.y = element_text(size = 16)
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 21, color = "black"))) +
  geom_segment(
    aes(x = 0, y = 0, xend = 0.3, yend = 0.3, linetype = "Trajectory"),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    color = "grey30",
    inherit.aes = FALSE
  ) +
  scale_linetype_manual(values = c("Trajectory" = "solid"), name = NULL)

# Make sure ggrepel is installed
# install.packages("ggrepel")
library(ggrepel)

BUDYKO_SIG <- BUDYKO_SIG +
  geom_text_repel(
    data = plot_wide_sig,
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


ggsave(
  "F5_mswepGleam_Budyko_Two_Periods_SIG.png",
  plot  = BUDYKO_SIG,
  dpi   = 600,
  width = 16, height = 10,
  bg    = "white"
)
