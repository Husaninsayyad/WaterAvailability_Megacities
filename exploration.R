#Below is Boxplot 
###########################################################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)

# pick a plausible time column to align mswep (P) with GLEAM (AET)
TIME_COL <- names(data)[names(data) %in% c("year","Year","date","time")][1]
if (is.na(TIME_COL)) stop("Couldn't find a time column named year/Year/date/time.")

# function: W trend when P and AET are in the SAME source (TERRA, ERA5)
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
      select(city, country, lon, lat, year, AET = value),
    by = c("city","country","lon","lat","year")
  ) %>%
    mutate(W = P - AET) %>%
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

#tp & e trends as I already did 

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
terra_aet <- trend_terra %>% filter(variable == "e")  %>%
  transmute(city, country, `TERRA AET` = Trend)

era_tp   <- trend_era   %>% filter(variable == "tp") %>%
  transmute(city, country, `ERA5L P` = Trend)
era_aet   <- trend_era   %>% filter(variable == "e")  %>%
  transmute(city, country, `ERA5L AET` = Trend)

mswep_tp  <- trend_mswep  %>% filter(variable == "tp") %>%
  transmute(city, country, `mswep P` = Trend)

gleam_aet <- trend_gleam %>% filter(variable == "e") %>%
  transmute(city, country, `GLEAM AET` = Trend)

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
  inner_join(terra_aet,      by = c("city","country")) %>%
  inner_join(era_aet,        by = c("city","country")) %>%
  inner_join(gleam_aet,      by = c("city","country")) %>%
  inner_join(terra_w,       by = c("city","country")) %>%
  inner_join(era_w,         by = c("city","country")) %>%
  inner_join(mswep_gleam_w,  by = c("city","country"))

long_df <- combined_df %>%
  pivot_longer(
    cols = c(`TERRA P`,`ERA5L P`,`mswep P`,
             `TERRA AET`,`ERA5L AET`,`GLEAM AET`,
             `TERRA W`,`ERA5L W`,`mswep/GLEAM W`),
    names_to = "Dataset", values_to = "Trend"
  )

# desired order P (TERRA, ERA, mswep)  AET (TERRA, ERA, GLEAM)  W (TERRA, ERA, mswep/GLEAM)
long_df$Dataset <- factor(long_df$Dataset,
                          levels = c("TERRA P","ERA5L P","mswep P",
                                     "TERRA AET","ERA5L AET","GLEAM AET",
                                     "TERRA W","ERA5L W","mswep/GLEAM W")
)

# quantity (for colors): P / AET / W
long_df$Quantity <- case_when(
  grepl(" P$",  long_df$Dataset) ~ "P",
  grepl(" AET$", long_df$Dataset) ~ "AET",
  TRUE                            ~ "W"
)

tick_labs <- c(
  "TERRA P"        = "TERRACLIMATE",
  "ERA5L P"        = "ERA5-LAND",
  "mswep P"         = "MSWEP-v2-8",
  "TERRA AET"       = "TERRACLIMATE",
  "ERA5L AET"       = "ERA5-Land",
  "GLEAM AET"       = "GLEAM-v4-1a",
  "TERRA W"        = "TERRACLIMATE",
  "ERA5L W"        = "ERA5-LAND",
  "mswep/GLEAM W"   = "MSWEP+GLEAM"
)

qty_cols <- c(P = "#D55E00" , AET = "#0072B2", W = "#009E73")  # orange/red, blue, green

BOXPLOT <- ggplot(long_df, aes(x = Dataset, y = Trend, fill = Quantity)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.95 , linewidth = 0.3 , width= 0.5) +
  scale_fill_manual(values = qty_cols, name = NULL,breaks = c("P", "AET", "W"),      # desired legend order (matches box order)
                    labels = c("P", "AET", "W")) +
  scale_x_discrete(labels = tick_labs)+
  labs(x = NULL, y = "Trend (mm/year)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1 , size = 14),
        axis.text.y = element_text(size = 14) , 
        legend.position = "bottom"  )

ggsave(
  "F4_Boxplot.png", plot  = BOXPLOT, dpi   = 600 ,width = 16, height = 10, bg    = "white")


##############################################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)
library(patchwork)

plot_water_trends <- function(data, dataset_name, output_file) {
  
  # 1) Prepare Data
  data_w <- data %>%
    spread(key = variable, value = value) %>%
    mutate(W = tp - e)
  
  # 2) Calculate Sen's slope & p-value
  sen_trends <- data_w %>%
    group_by(city) %>%
    summarise(
      slope = sens.slope(W)$estimates,
      p_value = sens.slope(W)$p.value,
      .groups = "drop"
    )
  
  # ============= Fixed color breakpoints ===============
  breaks <- c(-20, -5, 0, 5, 20)
  colors <- c("blue", "cyan", "#f7f7f7", "pink", "red2")
  
  # 3) Top + bottom 10 (all cities)
  top_bottom <- sen_trends %>%
    arrange(desc(slope)) %>%
    slice(1:min(n(), 10)) %>%
    bind_rows(
      sen_trends %>% arrange(slope) %>% slice(1:min(n(), 10))
    ) %>%
    distinct(city, .keep_all = TRUE) 
  
  # 4) Significant only
  sig <- sen_trends %>% filter(p_value < 0.05)
  
  top_bottom_sig <- sig %>%
    arrange(desc(slope)) %>%
    slice(1:min(n(), 10)) %>%
    bind_rows(
      sig %>% arrange(slope) %>% slice(1:min(n(), 10))
    ) %>%
    distinct(city, .keep_all = TRUE)
  
  # =============== PLOT 1 (all cities) ==================
  plot1 <- ggplot(top_bottom,
                  aes(x = reorder(city, slope),
                      y = slope,
                      fill = slope)) +
    geom_col() +
    coord_flip() +  # cities on y-axis
    scale_fill_gradientn(colours = colors,
                         values = scales::rescale(breaks),
                         limits = c(-20,20)) +
    labs(x = NULL , y = "(a) Cities with Highest and Lowest Trend (mm/year)") +  # no axis titles
    ylim(-20, 20) +             # fixed slope range
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  # =============== PLOT 2 (significant only) ==================
  plot2 <- ggplot(top_bottom_sig,
                  aes(x = reorder(city, slope),
                      y = slope,
                      fill = slope)) +
    geom_col() +
    coord_flip() +  # cities on y-axis
    scale_fill_gradientn(colours = colors,
                         values = scales::rescale(breaks),
                         limits = c(-20,20)) +
    labs(x = NULL, y = "(b) Cities with Highest and Lowest Significant Trends (mm/year)") +
    ylim(-20, 20) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
  
  # Combine plots
  combined <- plot1 | plot2
  
  # Save figure
  ggsave(output_file, combined, width = 16, height = 10, dpi = 800, bg = "white")
  
  return(combined)
}


data_era5 <- data1 %>% filter(data_source == "era5-land")

plot_water_trends(
  data = data_era5,
  output_file = "F6_Highest_and_Lowest_Era5-land.png"
)


data_mswep <- data1 %>% filter(data_source == "mswep/gleam")

plot_water_trends(
  data = data_mswep,
  output_file = "F6_Highest_and_Lowest_mswepgleam.png"
)

data_terra <- data %>% filter(data_source == "terraclimate")

plot_water_trends(
  data = data_terra,
  output_file = "F6_Highest_and_Lowest_TerraClimate.png"
)
