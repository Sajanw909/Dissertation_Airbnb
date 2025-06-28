#IV Regression modeling_Abandoned_Restored(Synthetic Proxy)
#===========================================================================================
library(dplyr)
library(AER)
#I. Verifying Dataset

#summary(final_geo_2020$year)

final_geo_2020 %>%
  select(housing_units, airbnb_listings,avg_trends_index,
         median_income, total_population, total_households,
         working_age_pop, active_population,
         commuters_by_public_transport, INSEE_COM,vacancy_rate) %>%
  summary()
summary(final_geo_2020$avg_trends_index)

#Handling missing values
final_geo_2020 <- final_geo_2020 %>%
  filter(!is.na(airbnb_listings), !is.na(median_income))

colnames(final_geo_2020)

#II. IV Regression Model
#------------------------------------------------------------------------------------------------
# First-stage regression: Predicting airbnb_listings
IV_first_stage <- lm(airbnb_listings ~ avg_trends_index + median_income +
                    total_population + avg_household_size + 
                    pct_entire_home + pct_multi_listing + avg_airbnb_price,
                  data = final_geo_2020)

summary(IV_first_stage)

#This doesn't work because the avg_trends_index is a constant so the model wont work
#Adjusting the trends_index based on IRIS codes and the year 2020. 

# Step 1: Filter trends data to 2020
google_trends_2020 <- subset(google_trends, year == 2020)

colnames(google_trends_2020)

# Step 2: Merge Google Trends into your main dataset by CODE_IRIS
google_trends_2020$IRIS <- substr(final_geo_2020$CODE_IRIS,
                                  nchar(final_geo_2020$CODE_IRIS) - 3 + 1,
                                  nchar(final_geo_2020$CODE_IRIS))

# Step 3: Ensure IRIS column in google_trends_2020 is character type
google_trends_2020$IRIS <- as.character(google_trends_2020$IRIS)


final_geo_2020 <- merge(final_geo_2020,
                        google_trends_2020[, c("IRIS_key", "trends_index")],
                        by.x = "IRIS_key", by.y = "IRIS",
                        all.x = TRUE)

# Step 3: (Optional) Rename the column for clarity
names(final_geo_2020)[names(final_geo_2020) == "trends_index"] <- "trends_index_spatial"

#ABANDONING IV => INSUFFICIENT ENDOGENOUES VARIABLE
#============================================================================================


