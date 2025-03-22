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
               "sf")
lapply(libraries, library, character.only=T)

# Set Working Directory
setwd("~/Documents/Agripath RA/Nepal Menstruation/Census")

### Data ----
# Age
dfs_age_ward <- list()
for(i in 1:7) { 
  path = paste0("Secondary/Age/Indv02-FiveYearsAgeGroupsAndSex", as.character(i), ".xlsx")
  age_ward <- readxl::read_excel(path, range="A5:H31238",
                                 col_names = c("province", "district", "municipality", "ward", "age_group",	"total", "male", "female"))
  dfs_age_ward[[i]] <- age_ward  
}
age_ward <- do.call(rbind, dfs_age_ward); rm(dfs_age_ward)

# Migration
migration_bagmati <- readxl::read_excel("Secondary/Migration/Hhld_UsualAbsenteesAbroadInternal_Ward_Bagmati.xlsx", range="A5:Y1259",
                                        col_names=c("province", "district", "municipality", "area", "ward", "reside_household", 
                                                    "reside_total_pop", "reside_male_pop", "reside_fem_pop", "abs_within_cntry_hh", 
                                                    "abs_within_cntry_ttl_pop", "abs_within_cntry_male", "abs_within_cntry_fem", "abs_abroad_hh", 
                                                    "abs_abroad_ttl_pop", "absent_abroad_male", "absent_abroad_fem", "abs_within_cntry_pct_hh",
                                                    "abs_within_cntry_pct_ttl_pop", "abs_within_cntry_pct_male", "abs_within_cntry_pct_fem",
                                                    "abs_abroad_pct_hh", "abs_abroad_pct_total_pop", "abs_abroad_pct_male", "abs_abroad_pct_fem"))
migration_gandaki <- readxl::read_excel("Secondary/Migration/Hhld_UsualAbsenteesAbroadInternal_Ward_Gandaki.xlsx", range="A5:Y1259",
                                        col_names=c("province", "district", "municipality", "area", "ward", "reside_household", 
                                                    "reside_total_pop", "reside_male_pop", "reside_fem_pop", "abs_within_cntry_hh", 
                                                    "abs_within_cntry_ttl_pop", "abs_within_cntry_male", "abs_within_cntry_fem", "abs_abroad_hh", 
                                                    "abs_abroad_ttl_pop", "absent_abroad_male", "absent_abroad_fem", "abs_within_cntry_pct_hh",
                                                    "abs_within_cntry_pct_ttl_pop", "abs_within_cntry_pct_male", "abs_within_cntry_pct_fem",
                                                    "abs_abroad_pct_hh", "abs_abroad_pct_total_pop", "abs_abroad_pct_male", "abs_abroad_pct_fem"))
# Density 
density_bagmati <- readxl::read_excel("Secondary/Density/Ward level density_Bagmati.xlsx", range="A4:L1124",
                                      col_names = c("prov", "dis", "gapa", "province", "district", "municipality", "ward", "pop_2011",
                                                    "total_pop_2021", "pop_male_2021", "pop_fem_2021", "density"))%>%
  select(-prov, -dis, -gapa)
density_gandaki <- readxl::read_excel("Secondary/Density/Ward level density_Gandaki.xlsx", range="A4:L1124",
                                      col_names = c("prov", "dis", "gapa", "province", "district", "municipality", "ward", "pop_2011",
                                                    "total_pop_2021", "pop_male_2021", "pop_fem_2021", "density"))%>%
  select(-prov, -dis, -gapa)

# Population
dfs_population_ward <- list()
for(i in 1:7) { 
  path = paste0("Secondary/Population/Ward/Indv01-HouseholdAndPopulation", as.character(i), ".xlsx")
  population_ward <- readxl::read_excel(path, range="A6:J1700",
                                        col_names = c("province", "district", "municipality", "ward", "pop_households",	"pop_total", "pop_male", "pop_female",
                                                      "average_household_size", "sex_ratio"))
  dfs_population_ward[[i]] <- population_ward  
}
population_ward <- do.call(rbind, dfs_population_ward); rm(dfs_population_ward)

# Amenities
dfs_amenities_ward <- list()
for(i in 1:7) { 
  path = paste0("Secondary/Amenities/Ward/Hhld10_HouseholdAmenities", as.character(i), ".xlsx")
  amenities_ward <- readxl::read_excel(path, range="A6:U1700",
                                       col_names = c("province", "district", "municipality", "ward", "households",	"without_any_amenities_assets", "at_least_one_amenity_asset",
                                                     "radio",	"television",	"land_line_telephone", "mobile_phone_ordinary",	"mobile_phone_smart",	
                                                     "computer_laptop",	"internet", "car_jeep_van",	"motor_cycle_scooter",	"bicycle",	
                                                     "electric_fan",	"refrigerator",	"washing_machine", "air_conditioner"))
  dfs_amenities_ward[[i]] <- amenities_ward
}
amenities_ward <- do.call(rbind, dfs_amenities_ward); rm(dfs_amenities_ward)

# Agricultural Employment for Men and Women
agri_empl_bagmati <- readxl::read_excel("Secondary/AgricultureEmployment/Indv_EmpAgriAndNonAgriculture_Ward_Bagmati.xlsx", range= "A4:T1258",
                                        col_names = c("province", "district", "municipality", "area", "ward", "total_empl_pop", "male_employed", "fem_employed", "total_agri_empl",
                                                      "male_agri_empl", "fem_agri_empl", "total_non_agri_empl", "male_non_agri_empl", "fem_non_agri_empl",
                                                      "ttl_pct_agri_empl", "male_pct_agri_empl", "fem_pct_agri_empl", "ttl_pct_non_agri_empl", "male_pct_non_agri_empl",
                                                      "fem_pct_non_agri_empl"))
agri_empl_gandaki <- readxl::read_excel("Secondary/AgricultureEmployment/Indv_EmpAgriAndNonAgriculture_Ward_Gandaki.xlsx", range= "A4:T1258",
                                        col_names = c("province", "district", "municipality", "area", "ward", "total_empl_pop", "male_employed", "fem_employed", "total_agri_empl",
                                                      "male_agri_empl", "fem_agri_empl", "total_non_agri_empl", "male_non_agri_empl", "fem_non_agri_empl",
                                                      "ttl_pct_agri_empl", "male_pct_agri_empl", "fem_pct_agri_empl", "ttl_pct_non_agri_empl", "male_pct_non_agri_empl",
                                                      "fem_pct_non_agri_empl"))

# Wealth Quintile
wealth_quintile_bagmati <- readxl::read_excel("Secondary/Wealth/Rev_Hhld_WealthQuintileHousehold_Ward_formated_Bagmati.xlsx", range="A4:P1258",
                                              col_names = c("province", "district", "municipality", "area", "ward", "total_wealth_quintile", "poorest", "poorer", "middle", "richer",
                                                            "richest", "pct_poorest", "pct_poorer", "pct_middle", "pct_richer", "pct_richest"))
wealth_quintile_gandaki <- readxl::read_excel("Secondary/Wealth/Rev_Hhld_WealthQuintileHousehold_Ward_formated_Gandaki.xlsx", range="A4:P860",
                                              col_names = c("province", "district", "municipality", "area", "ward", "total_wealth_quintile", "poorest", "poorer", "middle", "richer",
                                                            "richest", "pct_poorest", "pct_poorer", "pct_middle", "pct_richer", "pct_richest"))

# Caste/ Ethnicity 
caste_by_age_mun <- readxl::read_excel("Secondary/Ethnicity/Population_Caste_Ethnicity_NPHC_2021.xlsx", 
                                       range = "A5:P24582",
                                       col_names = c("country","province", "district", "municipality", "sex", "caste_ethnicity", "total", 
                                                     "yrs_0_4", "yrs_5_14", "yrs_15_24", "yrs_25_34", "yrs_35_44", "yrs_45_54", "yrs_55_64", "yrs_65_74", "yrs_75p")) 

caste_pop_mun <- readxl::read_excel("Secondary/Ethnicity/Cast_Ethinicity_NPHC_2021.xlsx", 
                                    sheet = "Prov_District_local level",
                                    range="A154:H31793",
                                    col_names = c("province","country", "district", "municipality", "caste_ethnicity", "total", 
                                                  "male", "female")) %>% 
  select(-country)

# Religion 
religion <- readxl::read_excel("Secondary/Religion/Religion_NPHC_2021.xlsx", 
                               sheet = "Prov_District_local level", 
                               range = "A6:Q4580",
                               col_names = c("country","province", "district", "municipality", "sex", "empty", "total_religion", 
                                             "hindu", "bouddha", "islam", "kirat", "christian", "prakriti", "bon", "jain", "bahai", "sikha")) %>%
  select(-empty)

# Opinion (Province Level Data)
#opinion <- read_dta("Secondary/Additional_Data/Opinion/02_Data_STATA_SNP_100050.dta")
#opinion <- labelled::to_character(opinion)

# Agro-Ecological Zones (District Level Data)
#agro_eco_zones <- read_csv("Secondary/Agro-Ecological Zones/Cross_sections_of_Nepal_s_physiographic_regions.csv")

# Tole data
toles <- readxl::read_excel("Secondary/Administrative Boundary/tole/Table 01_Number of tole reported by Ward.xlsx", range="A5:J1003",
                            col_names = c("country", "province", "district", "municipality", "ttl_toles", "toles_1_5", "toles_6_9", 
                                          "toles_10_19", "toles_above_20", "total_tole"))


# Secondary School
#school <- readxl::read_excel("Secondary/Additional_Data/Services/Table 12_SecondaryLevelSchool.xlsx", range="A5:J1003",
                          #   col_names = c("country", "province", "district", "municipality", "ttl_wards", "no_school", 
                                 #          "1_2_schools", "3_4_schools", "5_9_schools", "above_10_schools")) %>%
  #select(-country)

# Towns
towns_correct <- st_read("05_Services/03_Towns/towns_correct.shp") %>%
  select(town = Name, municipality = GaPa_NaP_1, province = Province, district =  DISTRICT, ward = new_ward_n, geometry)

# Load shapefiles 
villages_chitwan <- st_read("Secondary/Administrative Boundary/tole/villages_towns_of_chitwan/updated_villages_chitwan/villages_chitwan.shp") %>%
  rename(municipality = GaPa_NaP_1, district =  DISTRICT, ward = new_ward_n, village_name = VIL_NAME)
villages_nawalparasi <- st_read("Secondary/Administrative Boundary/tole/villages_towns_of_Nawalprasi_E/updated_villages_nawalparasi/villages_nawalparasi.shp") %>%
  rename(municipality = GaPa_NaP_1, district =  DISTRICT, ward = new_ward_n, village_name = VIL_NAME)
health <- st_read("05_Services/01_Health/updated_health_posts/health.shp") %>%
  rename(health_amenity = amenity, municipality = GaPa_NaP_1, district =  DISTRICT, ward = new_ward_n) %>%
  select(-building, -healthca_1, -operator_t, -capacity_p, -addr_full, -addr_city,
         -source, -name_ne, -osm_id, -osm_type, -type_en)
school <- st_read("05_Services/02_Education/updated_school/school.shp")%>%
  rename(school_name = name, school_amenity = amenity, municipality = GaPa_NaP_1, district =  DISTRICT, ward = new_ward_n) %>%
  select(-name_en, -building, -operator_t, -capacity_p, -addr_full, -addr_city,
         -source, -name_ne, -osm_id, -osm_type)


# Mappers for correct feature names
source("study_data_spatial_mappers.R")

### Cleaning Dataset ----
clean_hierarchical_data <- function(df) {
  df %>%
    mutate(level = case_when(
        !is.na(province) & province != "0" ~ 1,
        !is.na(district) & district != "0" ~ 2,
        (!is.na(municipality) & municipality != "0") | ward %in% c("All", "All Wards", "All wards") ~ 3,
        !is.na(ward) ~ 4,
        TRUE ~ NA_real_)) %>%
    fill(province, district, municipality, ward, .direction = "down") %>%
    mutate(ward = ifelse(ward %in% c("All", "All Wards", "All wards"), NA, ward),
      ward = as.numeric(ward)) %>%
    fill(level, .direction = "down")
}
age_ward <- clean_hierarchical_data(age_ward)
population_ward <- clean_hierarchical_data(population_ward)
amenities_ward <- clean_hierarchical_data(amenities_ward)

clean_separate_data <- function(df, province_code, province_name) {
  df %>%
    mutate(province = ifelse(province == province_code, province_name, province),
      district = ifelse(municipality == 0, area, district),
      district = ifelse(grepl("^[0-9]+$", district), NA, district)) %>%
    fill(district, .direction = "down") %>%
    select(-municipality) %>%
    rename(municipality = area)
}
migration_bagmati <- clean_separate_data(migration_bagmati, 3, "Bagmati")
migration_gandaki <- clean_separate_data(migration_gandaki, 4, "Gandaki")
agri_empl_bagmati <- clean_separate_data(agri_empl_bagmati, 3, "Bagmati")
agri_empl_gandaki <- clean_separate_data(agri_empl_gandaki, 4, "Gandaki")
wealth_quintile_bagmati <- clean_separate_data(wealth_quintile_bagmati, 3, "Bagmati")
wealth_quintile_gandaki <- clean_separate_data(wealth_quintile_gandaki, 4, "Gandaki")

caste_by_age_mun <- caste_by_age_mun %>%
  fill(country, .direction = "down") %>%
  fill(province, .direction = "down") %>%
  group_by(province) %>%
  fill(district, .direction = "down") %>%
  ungroup() %>%
  group_by(province, district) %>%
  fill(municipality, .direction = "down") %>%
  ungroup() %>%
  group_by(province, district, municipality) %>%  
  fill(sex, .direction = "down") %>%
  ungroup() %>%
  mutate(
    level = case_when(
      !is.na(country) & is.na(province) ~ 0,   
      !is.na(province) & is.na(district) ~ 1,  
      !is.na(district) & is.na(municipality) ~ 2, 
      !is.na(municipality) ~ 3                 
    )
  ) %>%
  relocate(level, .after = municipality)

caste_pop_mun <- caste_pop_mun %>%
  fill(province, .direction = "down") %>%
  group_by(province) %>%
  fill(district, .direction = "down") %>%
  ungroup() %>%
  group_by(province, district) %>%
  fill(municipality, .direction = "down") %>%
  ungroup() %>%
  group_by(province, district, municipality) %>%  
  ungroup() %>%
  mutate(level = case_when(
      !is.na(province) & is.na(district) ~ 1,  
      !is.na(district) & is.na(municipality) ~ 2, 
      !is.na(municipality) ~ 3                 
    )) %>%
  relocate(level, .after = municipality)

religion <- religion %>%
  fill(country, .direction = "down") %>%
  fill(province, .direction = "down") %>%
  group_by(province) %>%
  fill(district, .direction = "down") %>%
  ungroup() %>%
  group_by(province, district) %>%
  fill(municipality, .direction = "down") %>%
  ungroup() %>%
  mutate(
    level = case_when(
      !is.na(country) & is.na(province) ~ 0,   
      !is.na(province) & is.na(district) ~ 1,   
      !is.na(district) & is.na(municipality) ~ 2, 
      !is.na(municipality) ~ 3                 
    )
  ) %>%
  relocate(level, .after = municipality)
toles <- toles %>%
  mutate(
    level = ifelse(!is.na(country) & country != "0", 0,
                   ifelse(!is.na(province) & province != "0", 1,
                          ifelse(!is.na(district) & district != "0", 2,
                                 ifelse((!is.na(municipality) & municipality != "0"), 3, NA))))) %>%
  fill(country, .direction = "down") %>%
  fill(province, .direction = "down") %>%
  fill(district, .direction = "down") %>%
  fill(municipality, .direction = "down") %>%
  fill(level, .direction = "down")

# Move non-NA values from `name` to `name_en` in health data
health <- health %>%
  mutate(name_en = ifelse(!is.na(name), name, name_en)) %>%
  select(-name) %>%
  rename(health_name = name_en)

# Source formatting corrections 
formatting <- function(data, drop_column, total_column) {
  data %>%
    drop_na({{ drop_column }}) %>%
    mutate(
      across(
        any_of("municipality"), 
        ~ trimws(gsub("Municipality|Gaunpalika|Sub-Metropolitian City|Metropolitian City|Rural Municipality|Urban Municipality|Metropolitan", "", .))
      ),
      across(
        any_of("province"), 
        ~ trimws(gsub("Province", "", .))
      )
    )
}

age_ward <- formatting(age_ward, total, total)
migration_bagmati <- formatting(migration_bagmati, ward, ward)
migration_gandaki <- formatting(migration_gandaki, ward, ward)
density_bagmati <- formatting(density_bagmati, ward, ward)
density_gandaki <- formatting(density_gandaki, ward, ward)
population_ward <- formatting(population_ward, pop_households, total)
amenities_ward <- formatting(amenities_ward, households, total)
agri_empl_bagmati <- formatting(agri_empl_bagmati, ward, ward)
agri_empl_gandaki <- formatting(agri_empl_gandaki, ward, ward)
wealth_quintile_bagmati <- formatting(wealth_quintile_bagmati, ward, ward)
wealth_quintile_gandaki <- formatting(wealth_quintile_gandaki, ward, ward)
caste_by_age_mun <- formatting(caste_by_age_mun, total, total)
caste_pop_mun <- formatting(caste_pop_mun, total, total)
religion <- formatting(religion, total_religion, total_religion)
toles <- formatting(toles, ttl_toles, ttl_toles)

# Remove "INSTITUTIONAL" from population counts
remove_institutional <- function(data) {
  data %>%
    filter(is.na(municipality) | municipality != "INSTITUTIONAL")
}
dataset_names <- c( "caste_by_age_mun", "caste_pop_mun", "religion")
for (name in dataset_names) {
  assign(name, remove_institutional(get(name)))
}

# Pivot data from long to wide format
clean_column_names <- function(names) {
  names %>%
    tolower() %>%                 
    gsub("[ &/,]", "_", .) %>%    
    gsub("__+", "_", .)            
}
age_ward <- age_ward %>%
  pivot_wider(
    names_from = c(age_group),
    values_from = c(total, male, female),
    names_sep = "_"
  )%>%
  rename_with(clean_column_names) 
caste_by_age_mun <- caste_by_age_mun %>%
  pivot_wider(names_from = c(sex, `caste_ethnicity`), 
    values_from = c(total, `yrs_0_4`, `yrs_5_14`, `yrs_15_24`, `yrs_25_34`, `yrs_35_44`, `yrs_45_54`, `yrs_55_64`, `yrs_65_74`, `yrs_75p`), 
    names_sep = "_",
    names_glue = "{.value}_{sex}_{`caste_ethnicity`}") %>%
  rename_with(clean_column_names) 
caste_pop_mun <- caste_pop_mun %>%
  pivot_wider(
    names_from = c(`caste_ethnicity`), 
    values_from = c(total, male, female), 
    names_sep = "_",
    names_glue = "{.value}_{`caste_ethnicity`}"
  ) %>%
  rename_with(clean_column_names) 
religion <- religion %>%
  pivot_wider(
    names_from = c(sex), 
    values_from = c(total_religion, hindu, bouddha, islam, kirat, christian, prakriti, bon, jain, bahai, sikha), 
    names_sep = "_",
    names_glue = "{.value}{ifelse(sex == 'total', '', paste0('_', sex))}"
  ) %>%
  rename_with(clean_column_names)

# Rename spatial features using mapper
recode_names <- function(data, recode_province, recode_district, recode_municipality) {
  data %>%
    mutate(
      across(any_of("province"), str_to_title),
      across(any_of("district"), str_to_title),
      across(any_of("province"), ~ recode(., !!!recode_province)),
      across(any_of("district"), ~ recode(., !!!recode_district)),
      across(any_of("municipality"), ~ recode(., !!!recode_municipality))
    )
}

dataset_names <- c( "age_ward", "migration_bagmati", "migration_gandaki", "density_bagmati", "density_gandaki",
                    "population_ward", "amenities_ward", "agri_empl_bagmati", "agri_empl_gandaki",
                    "wealth_quintile_bagmati", "wealth_quintile_gandaki", "caste_by_age_mun", 
                    "caste_pop_mun", "religion", "toles", "villages_chitwan", "villages_nawalparasi", "health",
                    "school", "towns_correct")
                    
for (name in dataset_names) {
  assign(name, recode_names(get(name), recode_province, recode_district, recode_municipality))
}

# Subset to ward or municipality levels
age_ward <- age_ward %>% filter(level == 4) %>% select(-level)
migration_bagmati <- migration_bagmati %>% filter(ward != "Total")
migration_gandaki <- migration_gandaki %>% filter(ward != "Total")
population_ward <- population_ward %>% filter(level == 4) %>% select(-level)
amenities_ward <- amenities_ward %>% filter(level == 4) %>% select(-level)
agri_empl_bagmati <- agri_empl_bagmati %>% filter(ward != "Total")
agri_empl_gandaki <- agri_empl_gandaki %>% filter(ward != "Total")
wealth_quintile_bagmati <- wealth_quintile_bagmati %>% filter(ward != "Total")
wealth_quintile_gandaki <- wealth_quintile_gandaki %>% filter(ward != "Total")
caste_by_age_mun <- caste_by_age_mun %>% filter(level == 3) %>% select(-level)
caste_pop_mun <- caste_pop_mun %>% filter(level == 3) %>% select(-level)
religion_mun <- religion %>% filter(level == 3) %>% select(-level)
toles <- toles %>% filter(level == 3) %>% select(-level)

# Remove columns with all-NA values
remove_na_cols <- function(data) {
  data %>%
    select(where(~!all(is.na(.))))
}
caste_by_age_mun <- remove_na_cols(caste_by_age_mun)
caste_pop_mun <- remove_na_cols(caste_pop_mun)

# Subset to two districts
datasets_province_district <- c("age_ward", "population_ward", "amenities_ward", 
                                "caste_by_age_mun", "caste_pop_mun", "religion_mun", "toles",
                                "towns_correct")

for (dataset in datasets_province_district) {
  assign(paste0(dataset, "_sub"),
         get(dataset) %>% filter(
           (province == "Bagmati" & district == "Chitwan") |
             (province == "Gandaki" & district == "Nawalparasi East")
         ))
}
datasets_district <- c("migration_bagmati", "migration_gandaki", "density_bagmati", "density_gandaki",
                       "agri_empl_bagmati", "agri_empl_gandaki", "wealth_quintile_bagmati", "wealth_quintile_gandaki")
districts <- c("Chitwan", "Nawalparasi East", "Chitwan", "Nawalparasi East",
               "Chitwan", "Nawalparasi East", "Chitwan", "Nawalparasi East")
for (i in seq_along(datasets_district)) {
  assign(datasets_district[i], get(datasets_district[i]) %>% filter(district == districts[i]))
}
health <- health %>%
  filter(district %in% c("Chitwan", "Nawalparasi East"))
school <- school %>%
  filter(district %in% c("Chitwan", "Nawalparasi East"))
  
# Remove unnecessary data 
rm(age_ward); rm(religion); rm(population_ward); rm(caste_by_age_mun); rm(caste_pop_mun); 
rm(religion_mun); rm(amenities_ward); rm(towns_correct)

### Merging section ----
# Combine Bagmati and Gandaki district datasets into one 
migration_sub <- bind_rows(migration_bagmati, migration_gandaki); rm(migration_bagmati); rm(migration_gandaki)
density_sub <- bind_rows(density_bagmati, density_gandaki); rm(density_bagmati); rm(density_gandaki)
agri_empl_sub <- bind_rows(agri_empl_bagmati, agri_empl_gandaki); rm(agri_empl_bagmati); rm(agri_empl_gandaki)
wealth_quintile_sub <- bind_rows(wealth_quintile_bagmati, wealth_quintile_gandaki); rm(wealth_quintile_bagmati); rm(wealth_quintile_gandaki)
villages <- bind_rows(villages_chitwan, villages_nawalparasi); rm(villages_chitwan); rm(villages_nawalparasi)

# Separate geometry into lon and lat
villages <- villages %>%
  mutate(lon_tole = st_coordinates(.)[, 1],  # X (Longitude)
         lat_tole = st_coordinates(.)[, 2])   # Y (Latitude)
health <- health %>%
  mutate(lon_health = st_coordinates(.)[, 1],  # X (Longitude)
         lat_health = st_coordinates(.)[, 2])   # Y (Latitude)
school <- school %>%
  mutate(lon_school = st_coordinates(.)[, 1],  # X (Longitude)
         lat_school = st_coordinates(.)[, 2])   # Y (Latitude)
towns_correct_sub <- towns_correct_sub %>%
  mutate(lon_town = st_coordinates(.)[, 1],  # X (Longitude)
         lat_town = st_coordinates(.)[, 2])   # Y (Latitude)

# Convert ward column from character into numeric to make it possible to merge datasets
migration_sub <- migration_sub %>% mutate(ward = as.numeric(ward))
agri_empl_sub <- agri_empl_sub %>% mutate(ward = as.numeric(ward))
wealth_quintile_sub <- wealth_quintile_sub %>% mutate(ward = as.numeric(ward))

# Merge ward level datasets
merged <- wealth_quintile_sub %>% left_join(agri_empl_sub, by = c("province", "district", "municipality", "ward"))
merged <- merged %>% left_join(age_ward_sub, by = c("province", "district", "municipality", "ward"))                                          
merged <- merged %>% left_join(migration_sub, by = c("province", "district", "municipality", "ward"))  
merged <- merged %>% left_join(density_sub, by = c("province", "district", "municipality", "ward"))  
merged <- merged %>% left_join(population_ward_sub, by = c("province", "district", "municipality", "ward"))%>%
  select(-pop_total, -pop_male, -pop_female)
merged <- merged %>% left_join(amenities_ward_sub, by = c("province", "district", "municipality", "ward"))  
merged <- merged %>% left_join(villages, by = c("district", "municipality", "ward")) %>%
  relocate(village_name, lon_tole, lat_tole, .after = ward)
merged <- merged %>% left_join(health, by = c("district", "municipality", "ward"), relationship = "many-to-many") %>%
  relocate(health_amenity, lon_health, lat_health, .after = lat_tole)
merged <- merged %>% left_join(school, by = c("district", "municipality", "ward"), relationship = "many-to-many") %>%
  relocate(school_name, school_amenity, lon_school, lat_school, .after = lat_health)
merged <- merged %>% left_join(towns_correct_sub, by = c("province", "district", "municipality", "ward"), relationship = "many-to-many") %>%
  relocate(town, school_amenity, lon_town, lat_town, .after = lat_tole)


# Merge municipality level datasets
mun_merged <- caste_pop_mun_sub %>% left_join(caste_by_age_mun_sub, by = c("province", "district", "municipality")) %>%
  select(-country)
mun_merged <- mun_merged %>% left_join(religion_mun_sub, by = c("province", "district", "municipality"))%>%
  select(-country)
mun_merged <- mun_merged %>% left_join(toles, by = c("province", "district", "municipality"))%>%
  select(-country)

# Merge ward level data with municipality
ward_mun <- merged %>% left_join(mun_merged, by = c("province", "district", "municipality"))



# Rename the long column names to be able to save in "dta" format
names(ward_mun) <- names(ward_mun) %>%
  gsub("_both_sexes_", "_ttl_", ., fixed = TRUE) %>%     
  gsub("_female_", "_fem_", ., fixed = TRUE) %>%       
  gsub("_linguistic_", "", ., fixed = TRUE) %>%    
  gsub("_not_stated", "", ., fixed = TRUE) %>%  
  gsub("_mountain_", "", ., fixed = TRUE) %>%
  gsub("yrs_", "yrs", ., fixed = TRUE) %>%
  gsub("_madhesh_tarai_dalit", "_madheshtaraidalit", ., fixed = TRUE)%>%
  gsub("_madhesh_tarai_caste", "_madheshtaraicaste", ., fixed = TRUE)%>%
  gsub("yrs.", "yrs", ., fixed = TRUE)%>%
  gsub("_newa:(newar)", "_newar", ., fixed = TRUE)
names(ward_mun) <- gsub("-", "_", names(ward_mun))
names(ward_mun) <- gsub("\\+", "p", names(ward_mun)) 

# Remove geometry column to be able to save in "dta"
ward_mun <- ward_mun %>%
  select(-geometry.x, -geometry.y, -geometry.y.y, -geometry.x.x)

# Save the data 
write_dta(ward_mun, "ward_municipality.dta")



