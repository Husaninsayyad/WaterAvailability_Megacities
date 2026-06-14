library(readxl)
library(dplyr)
library(readr)

#the following data was requested from WorldPopulationReview.com

population_data <- read_csv("C:/Users/Admin/OneDrive - CZU v Praze/Plocha/Thesis/draft/thesis/world-city-listing-table (1).csv")
length(unique(population_data$city))
length(population_data$city)
head(population_data)

#removing extra colomn 
population_data <- population_data %>%
  select( -population , -growthRate , -type, -rank)
