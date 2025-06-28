#Merging Datasets_IRIS

library(dplyr)
library(readr)
library(ggplot2)
library(cowplot)
library(gridExtra)

# Ensure 'iris' is character in all datasets
housing_clean <- housing_clean %>% mutate(iris = as.character(iris))
income_clean <- income_clean %>% mutate(iris = as.character(iris))
population_clean <- population_clean %>% mutate(iris = as.character(iris))
activity_clean <- activity_clean %>% mutate(iris = as.character(iris))


# Merge housing + income
merged_df <- housing_clean %>%
  left_join(income_clean, by = "iris") %>%
  left_join(population_clean, by = "iris") %>%
  left_join(activity_clean, by = "iris")

# Optional: Filter to keep only valid rows for Paris (start with 75)
merged_df <- merged_df %>%
  filter(startsWith(iris, "75"))

# Preview merged data
glimpse(merged_df)

# Optional: Save merged file
write.csv(merged_df, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/merged_paris_iris_2020.csv", row.names = FALSE)


#Merging with Spatial geometry

#Converting merged_df to characters
merged_df$iris <- as.character(merged_df$iris)

library(sf)

# Load the geojson file in R
geo_df <- st_read("paris_iris_2023.geojson")

# Check structure
glimpse(geo_df)

#Turning CODE_IRIS to character
geo_df$CODE_IRIS <- as.character(geo_df$CODE_IRIS)
class(geo_df$CODE_IRIS)

# Merge spatial + attribute data
merged_geo <- geo_df %>%
  left_join(merged_df, by = c("CODE_IRIS" = "iris"))

#Saving this as rds file
saveRDS(merged_geo, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/merged_paris_iris_2020.rds")

#To read
# merged_geo <- readRDS("C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/merged_paris_iris_2020.rds")


#Merging Listings_clean with merged_geo

# 1. Load the cleaned Airbnb listings
listings_new <- read_csv("C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/Listings_Airbnb.csv")

# 2. Convert listings to an sf object
listings_sf <- st_as_sf(
  listings_new,
  coords = c("longitude", "latitude"),
  crs = 4326  # WGS 84 (matches standard GeoJSON projection)
)

# 3. Load your spatial IRIS data (merged dataset with geometry)
merged_geo_new <- readRDS("C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/merged_paris_iris_2020.rds")  # Make sure this is in sf format

# 4. Reproject both to the same CRS (optional but good practice)
merged_geo_new <- st_transform(merged_geo_new, crs = 2154)       # Use French Lambert 93 (EPSG:2154)
listings_sf <- st_transform(listings_sf, crs = 2154)

# 5. Perform spatial join: assign each listing to an IRIS unit
listings_joined <- st_join(listings_sf, merged_geo_new["CODE_IRIS"], join = st_within)

# 6. Aggregate Airbnb metrics by IRIS
airbnb_by_iris <- listings_joined %>%
  st_drop_geometry() %>%
  group_by(CODE_IRIS) %>%
  summarise(
    airbnb_listings = n(),
    avg_airbnb_price = mean(price, na.rm = TRUE),
    pct_entire_home = mean(is_entire_home, na.rm = TRUE),
    pct_multi_listing = mean(is_multi_listing, na.rm = TRUE)
  )

# 7. Join these Airbnb metrics back to the merged IRIS spatial data
final_geo <- merged_geo_new %>%
  left_join(airbnb_by_iris, by = c("CODE_IRIS"))

# 8. Save result for analysis or mapping
saveRDS(final_geo,"C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/airbnb_enriched_iris_2020.rds")

# Quick map of Airbnb listings count
library(tmap)
tmap_mode("view")
tm_shape(final_geo) +
  tm_polygons("airbnb_listings", palette = "YlOrRd", title = "Airbnb Listings")

#Merging final_geo with Google Trends filtering for 2020
trends_2020 <- google_trends_yearly %>% filter(year == 2020)

final_geo_2020 <- final_geo %>%
  mutate(year = 2020) %>%
  left_join(trends_2020, by = "year")

#Corrections done to add vacancy_rates (Dependent variable)
final_geo_2020 <- final_geo_2020 %>%
  mutate(vacancy_rate = vacant_units / housing_units)

#Avg_trends_index
#Google Trends as instrument

google_trends_2020 <- google_trends %>%
  filter(year == 2020) %>%
  summarise(avg_trends_index = mean(trends_index, na.rm = TRUE))

# Explicitly assign the computed index to every row of your final_geo
final_geo_2020 <- final_geo_2020 %>%
  mutate(avg_trends_index = google_trends_2020$avg_trends_index[1])


#Optional: Comparing plots_Airbnb Listings and Google Trends(2020)

# 1. Set tmap mode
tmap_mode("plot")

#-----Airbnb Listings Map-----
tm_airbnb <- tm_shape(final_geo) +
  tm_polygons("airbnb_listings",
              palette = "YlOrRd",
              title = "Airbnb Listings (2020)") +
  tm_layout(main.title = "Airbnb Listings by IRIS", main.title.size = 1.1)

# Convert tmap to ggplot-compatible object
grob_airbnb <- tmap_grob(tm_airbnb)

# ----- Google Trends Map -----

#Google Trends Line Chart for 2020
google_trends_2020 <- google_trends %>%
  filter(year == 2020)

gg_trends <- ggplot(google_trends_2020, aes(x = date, y = trends_index)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  theme_minimal() +
  labs(title = "Google Search Interest for 'Airbnb Paris' (2020)",
       x = "Month",
       y = "Search Interest (0–100)") +
  theme(plot.title = element_text(size = 14, face = "bold"))

# 5. Combine both using cowplot
cowplot::plot_grid(grob_airbnb, gg_trends, labels = c("A", "B"), ncol = 2)



