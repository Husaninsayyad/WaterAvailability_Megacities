getwd()
my_wd <- getwd()
setwd(my_wd)
setwd('C:/Users/Admin/OneDrive - CZU v Praze/Plocha/Thesis/Paper1/MSWEP')
library(dplyr)
library(readxl)
# reading the data sent by Vishal 
data <- readRDS('C:/Users/Admin/OneDrive - CZU v Praze/Plocha/Thesis/Paper1/p_e_pet_cities_data_final1.rds')
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


