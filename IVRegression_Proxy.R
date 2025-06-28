#IV Regression(Synthetic proxy)

colnames(final_geo_2020)

#1. Urban Accessibility

final_geo_2020$urban_accessibility_iv <- scale(final_geo_2020$total_population / final_geo_2020$housing_units) *
  scale(final_geo_2020$commuters_by_public_transport)



# First-stage regression: Check if the instrument predicts Airbnb presence
first_stage_urbaniv <- lm(airbnb_listings ~ urban_accessibility_iv + median_income +
                    total_population + avg_household_size + 
                    pct_entire_home + pct_multi_listing + avg_airbnb_price,
                  data = final_geo_2020)
summary(first_stage)

#p-value for urban_accessibility_iv is insignificant, indicating a weak relationship with airbnb_listings

#2. Personal transport (bike, walking)

final_geo_2020$bike_foot_iv <- scale(final_geo_2020$commuters_by_bike) +
  scale(final_geo_2020$commuters_on_foot)

# First-stage regression: Check if the instrument predicts Airbnb presence
first_stage_bike_foot <- lm(airbnb_listings ~ bike_foot_iv + median_income +
                              total_population + avg_household_size + 
                              pct_entire_home + pct_multi_listing + avg_airbnb_price,
                            data = final_geo_2020)
summary(first_stage_bike_foot)

#p-value for bike_foot_iv is significant, indicating a strong relationship with airbnb_listings

# Second-stage regression: Estimate the effect of Airbnb presence on vacancy rates

iv_model <- ivreg(vacancy_rate ~ airbnb_listings + median_income +
                    total_population + avg_household_size + 
                    pct_entire_home + pct_multi_listing + avg_airbnb_price |
                    bike_foot_iv + median_income + total_population + 
                    avg_household_size + pct_entire_home + 
                    pct_multi_listing + avg_airbnb_price,
                  data = final_geo_2020)

summary(iv_model)
summary(iv_model, diagnostics = TRUE) #Hausman Test
