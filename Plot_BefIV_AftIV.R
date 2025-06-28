#Plots for Dissertaion

library(ggplot2)
library(dplyr)

#I. Before IV Modeling

#1. Historgram

#1.1. Airbnb_Listings

ggplot(final_geo_2020, aes(x = airbnb_listings)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() + 
  labs(title = "Distribution of Airbnb Listings per IRIS", x = "Listings", y = "Count")

#1.2. Vacancy Rates_IRIS (Dependent variable)

ggplot(final_geo_2020, aes(x = vacancy_rate)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  theme_minimal() + 
  labs(title = "Distribution of Vacancy Rate across IRIS", x = "Vacancy Rate", y = "Count")

#2. Scatter Plots

#2.1. Airbnb Listings vs Vacancy Rate
ggplot(final_geo_2020, aes(x = airbnb_listings, y = vacancy_rate)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Airbnb Listings vs. Vacancy Rate")

#3. Boxplots

#3.1. Airbnb Listings by IRIS

ggplot(final_geo_2020, aes(x = INSEE_COM, y = airbnb_listings)) +
  geom_boxplot() +
  labs(title = "Airbnb Listings Across Arrondissements", x = "Arrondissement Code", y = "Airbnb Listings")


