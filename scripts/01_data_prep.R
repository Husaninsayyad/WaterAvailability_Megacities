# 01_data_prep.R
library(dplyr)
library(readxl)

#Raw_data file contain 237 cities selected for research
raw_data <- readRDS("data/Raw_data.rds")  
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

#Converting to lower case and removing spaces
data <- data %>%
  mutate(across(c(city , country, Continent),
                ~ tolower(gsub(" " , "_" , .))))

#saving RDS to use in later scripts 
saveRDS(data, "data/data_final.rds")

# gleam and mswep are treated as one combined source
combined_source_data <- data %>%
  mutate(data_source = recode(data_source,
                              "gleam-v4-1a" = "mswep/gleam",
                              "mswep-v2-8"  = "mswep/gleam"))

#saving RDS to use in later scripts 
saveRDS(combined_source_data, "data/combined_source_data.rds")

unique(combined_source_data$data_source)
combined_source_data %>% distinct(data_source, variable)

