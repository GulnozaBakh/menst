# Preamble ----
# Clean up 
rm(list = ls())

# Settings
set.seed(42)
options(scipen=2, digits=4)
options(max.print = 9999)

# Libraries
libraries <- c("dplyr", "haven",  "stringr", "purrr", "sf", "geosphere", "terra")
lapply(libraries, library, character.only=T)

# Set Working Directory
setwd("~/Documents/Agripath RA/Nepal Menstruation/Census")

# Load the GeoTIFF
rainfall_2013 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2013.tif")
rainfall_2014 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2014.tif")
rainfall_2015 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2015.tif")
rainfall_2016 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2016.tif")
rainfall_2017 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2017.tif")
rainfall_2018 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2018.tif")
rainfall_2019 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2019.tif")
rainfall_2020 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2020.tif")
rainfall_2021 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2021.tif")
rainfall_2022 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2022.tif")
rainfall_2023 <- rast("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y2023.tif")

# Load ward_mun dataset 
ward_mun <- read_dta("ward_municipality.dta")

# Convert lon_tole and lat_tole into numeric
ward_mun$lon_tole <- as.numeric(ward_mun$lon_tole)
ward_mun$lat_tole <- as.numeric(ward_mun$lat_tole)

# Convert ward_mun into a SpatVector (spatial points)
points_vect <- vect(ward_mun, geom = c("lon_tole", "lat_tole"), crs = "EPSG:4326")

# Define years
years <- 2013:2023

# Create a rainfall dataframe to store rainfall data
rainfall_data <- ward_mun 

# Loop through each year, load raster, extract values, and store them
for (year in years) {
  raster_path <- paste0("04_Rainfall/CCS_Nepal_2025-02-28044126am/CCS_1y", year, ".tif")
  raster_data <- rast(raster_path)
  if (crs(raster_data) != crs(points_vect)) {   # check if CRS of raster and points match
    points_vect <- project(points_vect, crs(raster_data))  
  }
  rainfall_values <- extract(raster_data, points_vect) # extract rainfall values
  rainfall_data[[paste0("rainfall_", year)]] <- rainfall_values[, 2] # add rainfall values as new columns
}

# Relocate all rainfall columns next to 'lat_school'
rainfall_data <- rainfall_data %>%
  relocate(starts_with("rainfall_"), .after = lat_school)

# Save the data 
write_dta(rainfall_data, "rainfall_data.dta")

### Checking the tole coordinates fall within the rainfall boundaries ----
#1. Plot the rainfall raster (for 2013)
plot(rainfall_2013, main = "Rainfall 2013 with Ward Locations")
plot(points_vect, add = TRUE, col = "red", pch = 16) # Overlay the points in red

#2. Get the extent of the 2013 raster
raster_extent <- ext(rainfall_2013)
print(raster_extent)
# Get the coordinates of Mainatar (Mainatar is an example = first row in the data)
mainatar_coords <- ward_mun %>% filter(village_name == "Mainatar") %>% select(lon_tole, lat_tole)
print(mainatar_coords)

#3. Check CRS for the raster and points
crs_raster <- crs(rainfall_2013)
crs_points <- crs(points_vect)
print(crs_raster)
print(crs_points)

### THE MATCHING IS CORRECT ###
