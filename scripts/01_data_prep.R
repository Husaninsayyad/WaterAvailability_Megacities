# 01_data_prep.R

library(dplyr)
library(readxl)

data <- readRDS("data/p_e_pet_cities_data_final1.rds")
continent_data <- read_excel("data/continent_data_clean.xlsx")

# extract year from date and drop the date column
data <- data %>%
  mutate(year = format(as.Date(date), "%Y")) %>%
  select(-date)

data <- data %>%
  left_join(continent_data %>% select(city, Continent), by = "city")

# mswx-past is excluded from the analysis
data <- data %>%
  filter(data_source != "mswx-past")

# gleam and mswep are treated as one combined source
data1 <- data %>%
  mutate(data_source = recode(data_source,
                              "gleam-v4-1a" = "mswep/gleam",
                              "mswep-v2-8"  = "mswep/gleam"))

unique(data1$data_source)
data1 %>% distinct(data_source, variable)

