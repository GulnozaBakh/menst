# Preamble ----
# Clean up 
rm(list = ls())

# Settings
set.seed(42)
options(scipen=2, digits=4)
options(max.print = 9999)

# Libraries
libraries <- c("dplyr", "haven", "labelled", "readxl", "survey", "readr", "psych", 
               "tidyr", "broom", "tableone", "webshot2", "stringr", "purrr", "foreign",
               "sf", "geosphere")
lapply(libraries, library, character.only=T)

# Set Working Directory
setwd("~/Documents/Agripath RA/Nepal Menstruation/Census")

# Load data
rainfall_data <- read_dta("rainfall_data.dta")

### Calculate the distance in km ----
rainfall_data <- rainfall_data %>%
  mutate(tole_health_distance_km = distHaversine(
    matrix(c(lon_tole, lat_tole), ncol = 2),
    matrix(c(lon_health, lat_health), ncol = 2)
  ) / 1000) %>%  # Convert meters to km
  relocate(tole_health_distance_km, .after = lat_health)
rainfall_data <- rainfall_data %>%
  mutate(tole_school_distance_km = distHaversine(
    matrix(c(lon_tole, lat_tole), ncol = 2),
    matrix(c(lon_school, lat_school), ncol = 2)
  )/ 1000) %>%
  relocate(tole_school_distance_km, .after = lat_school)
rainfall_data <- rainfall_data %>%
  mutate(tole_town_distance_km = distHaversine(
    matrix(c(lon_tole, lat_tole), ncol = 2),
    matrix(c(lon_town, lat_town), ncol = 2)
  )/ 1000) %>%
  relocate(tole_town_distance_km, .after = lat_town)


### Rainfall Analysis----
rainfall_cols <- paste0("rainfall_", 2013:2023)

# Calculate Mean, SD, and CV for each location
final_df <- rainfall_data %>%
  rowwise() %>%  
  mutate(
    mean_rainfall = mean(c_across(all_of(rainfall_cols)), na.rm = TRUE),
    sd_rainfall = sd(c_across(all_of(rainfall_cols)), na.rm = TRUE),
    cv_rainfall = (sd_rainfall / mean_rainfall) * 100
  ) %>%
  ungroup()



# Save the data 
write_dta(final_df, "final_df.dta")

