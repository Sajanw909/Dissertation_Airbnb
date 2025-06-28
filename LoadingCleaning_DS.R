#Loading and cleaning datasets
#INSEE #Airbnb 

#I. INSEE
#1. Housing data_Paris_2020
# Load required packages
library(readxl)
library(dplyr)
library(readr)
library(janitor)
library(lubridate)

# Load data
housing_raw <- read_excel("Paris_IRIS_Housing_2020.xlsx") %>%
  clean_names()

# Select relevant columns
housing_clean <- housing_raw %>%
  select(
    iris,
    p20_log,            # total housing units
    p20_rp,             # main residences
    p20_logvac,         # vacant dwellings
    p20_appart,         # apartments
    p20_maison,         # houses
    c20_rp_hstu1p,      # 1-person households
    c20_rp_hstu1p_surocc # overcrowded 1-person households
  ) %>%
  rename(
    housing_units = p20_log,
    main_residences = p20_rp,
    vacant_units = p20_logvac,
    apartments = p20_appart,
    houses = p20_maison,
    one_person_hh = c20_rp_hstu1p,
    overcrowded_one_person_hh = c20_rp_hstu1p_surocc
  ) %>%
  filter(!is.na(iris), !is.na(housing_units))

# Save cleaned file (optional)
write.csv(housing_clean, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/housing_iris_2020.csv", row.names = FALSE)

#2. IRIS Income_Filosofi_2020

# Load the income file
income_raw <- read_excel("Paris_IRIS_Income_Filosofi_2020.xlsx") %>%
  clean_names()

# Clean and filter useful income indicators
income_clean <- income_raw %>%
  filter(dec_med20 != "ns", dec_med20 != "nd") %>%
  select(
    iris,
    dec_med20,
    dec_eq20,
    dec_s80s2020
  ) %>%
  rename(
    median_income = dec_med20,
    equivalized_income = dec_eq20,
    income_inequality_ratio = dec_s80s2020
  ) %>%
  mutate(
    across(
      .cols = c(median_income, equivalized_income, income_inequality_ratio),
      .fns = ~ as.numeric(.)
    )
  )

# Optional: Save cleaned income file
write.csv(income_clean, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/income_iris_2020.csv", row.names = FALSE)

#3. IRIS Population 2020

# Load the population dataset
population_raw <- read_excel("Paris_IRIS_Population_2020.xlsx") %>%
  clean_names()

# Select and rename relevant columns
population_clean <- population_raw %>%
  select(
    iris,
    p20_pop,        # total population
    p20_pmen,       # number of households
    p20_phormen     # average household size
  ) %>%
  rename(
    total_population = p20_pop,
    total_households = p20_pmen,
    avg_household_size = p20_phormen
  ) %>%
  filter(!is.na(iris), !is.na(total_population))

# Optional: Save cleaned dataset
write.csv(population_clean, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/population_iris_2020.csv", row.names = FALSE)

#4. IRIS Resident Activity 2020

# Load the dataset
activity_raw <- read_excel("Paris_IRIS_ResidentActivity_2020.xlsx") %>%
  clean_names()

# Select and rename relevant columns
activity_clean <- activity_raw %>%
  select(
    iris,
    p20_pop1564,               # working-age population
    p20_actocc15p,             # active population 15+
    c20_actocc15p_pas,         # workers on foot
    c20_actocc15p_velo,        # workers using bicycle
    c20_actocc15p_voit,        # workers using cars
    c20_actocc15p_tcom         # workers using public transport
  ) %>%
  rename(
    working_age_pop = p20_pop1564,
    active_population = p20_actocc15p,
    commuters_on_foot = c20_actocc15p_pas,
    commuters_by_bike = c20_actocc15p_velo,
    commuters_by_car = c20_actocc15p_voit,
    commuters_by_public_transport = c20_actocc15p_tcom
  ) %>%
  filter(!is.na(iris), !is.na(working_age_pop))

# Optional: Save cleaned dataset
write.csv(activity_clean, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/resident_activity_iris_2020.csv", row.names = FALSE)


# II. Airbnb Data

# Load listings
listings <- read_csv("listings_2.csv") %>%
  clean_names()

# Clean Airbnb listings data
listings_clean <- listings %>%
  select(
    id,
    host_id,
    latitude,
    longitude,
    room_type,
    price,
    calculated_host_listings_count
  ) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(
    price = as.numeric(gsub("[$,]", "", price)),  # Remove currency formatting
    is_entire_home = ifelse(room_type == "Entire home/apt", 1, 0),
    is_multi_listing = ifelse(calculated_host_listings_count > 1, 1, 0)
  )

# Preview
glimpse(listings_clean)

write.csv(listings_clean, "C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/Cleaned_Data/Lisitings_Airbnb.csv", row.names = FALSE)

#III. Google Trends

# Load and clean Google Trends data
google_trends <- read_csv("C:/Users/sajan/OneDrive/Documents/1_Sajan/Documents/NEOMA Business School/MSc Business Analytics/MSc_Dissertation/Raw_Data_Paris/google_trends_airbnb_paris.csv") %>%
  clean_names() %>%
  mutate(
    date = dmy(date),  # convert date format to proper Date type
    year = year(date),
    month = month(date)
  ) %>%
  rename(
    trends_index = airbnb_paris
  )

# Aggregate to annual average if merging by year
google_trends_yearly <- google_trends %>%
  group_by(year) %>%
  summarise(
    avg_trends_index = mean(trends_index, na.rm = TRUE)
  )

# Preview
head(google_trends_yearly)