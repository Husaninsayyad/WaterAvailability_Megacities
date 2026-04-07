# 02_exploration.R

library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)
library(patchwork)

# W trend when P and AET are from the same source (TerraClimate, ERA5-Land)
w_trend_same_source <- function(df_source, label) {
  df_source %>%
    select(city, country, lon, lat, year, variable, value) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(W = tp - e) %>%
    group_by(city, country, lon, lat) %>%
    summarise(Trend = sens.slope(W)$estimates, .groups = "drop") %>%
    mutate(Dataset = label)
}

# W trend when P and AET come from different sources (MSWEP and GLEAM)
w_trend_cross_source <- function(df_p, df_et, label) {
  left_join(
    df_p %>% filter(variable == "tp") %>% select(city, country, lon, lat, year, P = value),
    df_et %>% filter(variable == "e")  %>% select(city, country, lon, lat, year, AET = value),
    by = c("city", "country", "lon", "lat", "year")
  ) %>%
    mutate(W = P - AET) %>%
    group_by(city, country, lon, lat) %>%
    summarise(Trend = sens.slope(W)$estimates, .groups = "drop") %>%
    mutate(Dataset = label)
}

data_terra <- data1 %>% filter(data_source == "terraclimate")
data_era   <- data1 %>% filter(data_source == "era5-land")
data_mswep <- data1 %>% filter(data_source == "mswep/gleam" & variable == "tp")
data_gleam <- data1 %>% filter(data_source == "mswep/gleam" & variable == "e")

trend_terra <- data_terra %>% group_by(city, country, lon, lat, variable) %>% summarise(Trend = sens.slope(value)$estimates, .groups = "drop")
trend_era   <- data_era   %>% group_by(city, country, lon, lat, variable) %>% summarise(Trend = sens.slope(value)$estimates, .groups = "drop")
trend_mswep <- data_mswep %>% group_by(city, country, lon, lat, variable) %>% summarise(Trend = sens.slope(value)$estimates, .groups = "drop")
trend_gleam <- data_gleam %>% group_by(city, country, lon, lat, variable) %>% summarise(Trend = sens.slope(value)$estimates, .groups = "drop")

terra_tp  <- trend_terra %>% filter(variable == "tp") %>% transmute(city, country, `TERRA P`   = Trend)
terra_aet <- trend_terra %>% filter(variable == "e")  %>% transmute(city, country, `TERRA AET` = Trend)
era_tp    <- trend_era   %>% filter(variable == "tp") %>% transmute(city, country, `ERA5L P`   = Trend)
era_aet   <- trend_era   %>% filter(variable == "e")  %>% transmute(city, country, `ERA5L AET` = Trend)
mswep_tp  <- trend_mswep %>% filter(variable == "tp") %>% transmute(city, country, `mswep P`   = Trend)
gleam_aet <- trend_gleam %>% filter(variable == "e")  %>% transmute(city, country, `GLEAM AET` = Trend)

terra_w       <- w_trend_same_source(data_terra, "TERRA W")               %>% transmute(city, country, `TERRA W`       = Trend)
era_w         <- w_trend_same_source(data_era,   "ERA5L W")               %>% transmute(city, country, `ERA5L W`       = Trend)
mswep_gleam_w <- w_trend_cross_source(data_mswep, data_gleam, "mswep/GLEAM W") %>% transmute(city, country, `mswep/GLEAM W` = Trend)

combined_df <- terra_tp %>%
  inner_join(era_tp,        by = c("city", "country")) %>%
  inner_join(mswep_tp,      by = c("city", "country")) %>%
  inner_join(terra_aet,     by = c("city", "country")) %>%
  inner_join(era_aet,       by = c("city", "country")) %>%
  inner_join(gleam_aet,     by = c("city", "country")) %>%
  inner_join(terra_w,       by = c("city", "country")) %>%
  inner_join(era_w,         by = c("city", "country")) %>%
  inner_join(mswep_gleam_w, by = c("city", "country"))

long_df <- combined_df %>%
  pivot_longer(
    cols = c(`TERRA P`, `ERA5L P`, `mswep P`,
             `TERRA AET`, `ERA5L AET`, `GLEAM AET`,
             `TERRA W`, `ERA5L W`, `mswep/GLEAM W`),
    names_to = "Dataset", values_to = "Trend"
  )

long_df$Dataset <- factor(long_df$Dataset,
                          levels = c("TERRA P", "ERA5L P", "mswep P",
                                     "TERRA AET", "ERA5L AET", "GLEAM AET",
                                     "TERRA W", "ERA5L W", "mswep/GLEAM W"))

long_df$Quantity <- case_when(
  grepl(" P$",   long_df$Dataset) ~ "P",
  grepl(" AET$", long_df$Dataset) ~ "AET",
  TRUE                             ~ "W"
)

tick_labs <- c(
  "TERRA P" = "TERRACLIMATE", "ERA5L P" = "ERA5-LAND", "mswep P" = "MSWEP-v2-8",
  "TERRA AET" = "TERRACLIMATE", "ERA5L AET" = "ERA5-Land", "GLEAM AET" = "GLEAM-v4-1a",
  "TERRA W" = "TERRACLIMATE", "ERA5L W" = "ERA5-LAND", "mswep/GLEAM W" = "MSWEP+GLEAM"
)

qty_cols <- c(P = "#D55E00", AET = "#0072B2", W = "#009E73")

BOXPLOT <- ggplot(long_df, aes(x = Dataset, y = Trend, fill = Quantity)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.95, linewidth = 0.3, width = 0.5) +
  scale_fill_manual(values = qty_cols, name = NULL, breaks = c("P", "AET", "W"), labels = c("P", "AET", "W")) +
  scale_x_discrete(labels = tick_labs) +
  labs(x = NULL, y = "Trend (mm/year)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "bottom"
  )

ggsave("outputs/F4_Boxplot.png", plot = BOXPLOT, dpi = 600, width = 16, height = 10, bg = "white")


