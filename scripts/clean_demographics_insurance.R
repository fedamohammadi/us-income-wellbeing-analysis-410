# =========================================================
# Pull and merge ACS 5-year profile data
# =========================================================

library(tidycensus)
library(dplyr)
library(readr)
library(stringr)
library(purrr)

# -------------------------
# 1. Census API key
# -------------------------
census_api_key("my API key", 
               install = FALSE, 
               overwrite = TRUE)

# -------------------------
# 2. File paths (ADJUST THESE TO MATCH YOUR FOLDERS)
# -------------------------
base_file   <- "Data/processed/final_county_panel_with_income_labor_acs.csv"
output_file <- "Data/processed/final_county_panel_full.csv"

# --- DEBUGGING: Check if file exists before trying to read it ---
if (!file.exists(base_file)) {
  stop(paste0("ERROR: The file '", base_file, "' was not found.\n",
              "Your current working directory is: ", getwd(), "\n",
              "Please check if the file is in a folder named 'Data/processed'."))
}

# -------------------------
# 3. Read current panel
# -------------------------
panel_df <- read_csv(base_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year)
  )

# -------------------------
# 4. Define variable mapping function (Handles Census code shifts)
# -------------------------
get_dp_vars <- function(yr) {
  # Variables that stay the same
  vars <- c(
    population = "DP05_0001",
    median_age = "DP05_0018"
  )
  
  # Health Insurance (Only available 2012 onwards in 5-year profile)
  if (yr >= 2012) {
    vars <- c(vars, uninsured_rate = "DP03_0099P")
  }
  
  # Race/Ethnicity (Codes shifted in 2015 and 2019)
  if (yr < 2015) {
    vars <- c(vars, 
              hispanic_share = "DP05_0070P",
              white_share    = "DP05_0072P",
              black_share    = "DP05_0073P")
  } else { 
    # Works for 2015-2023
    vars <- c(vars, 
              hispanic_share = "DP05_0071P",
              white_share    = "DP05_0077P",
              black_share    = "DP05_0078P")
  }
  return(vars)
}

# -------------------------
# 5. Function to pull one ACS year
# -------------------------
get_profile_year <- function(yr) {
  message("Pulling ACS 5-year profile data for year ", yr, " ...")
  
  vars_to_pull <- get_dp_vars(yr)
  
  df <- tryCatch({
    get_acs(
      geography = "county",
      variables = vars_to_pull,
      year      = yr,
      survey    = "acs5",
      output    = "wide",
      geometry  = FALSE
    )
  }, error = function(e) {
    message("Data for year ", yr, " not available or API error.")
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  # Standardize column names (tidycensus adds 'E' for estimates)
  res <- df %>%
    transmute(
      county_fips = GEOID,
      year = as.integer(yr),
      population = populationE,
      median_age = median_ageE
    )
  
  if ("uninsured_rateE" %in% names(df)) res$uninsured_rate <- df$uninsured_rateE
  if ("hispanic_shareE" %in% names(df)) res$hispanic_share <- df$hispanic_shareE
  if ("white_shareE" %in% names(df))    res$white_share    <- df$white_shareE
  if ("black_shareE" %in% names(df))    res$black_share    <- df$black_shareE
  
  return(res)
}

# -------------------------
# 6. Pull and combine ACS years (2011-2023)
# -------------------------
years <- 2011:2023
profile_long <- map_df(years, get_profile_year)

# -------------------------
# 7. Add population density (Gazetteer Data)
# -------------------------
message("Downloading land area data for density calculation...")
gaz_url <- "https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2020_Gazetteer/2020_Gaz_counties_national.zip"
temp_zip <- tempfile(fileext = ".zip")
download.file(gaz_url, temp_zip, mode = "wb")
# Unzip and read
land_area_df <- read_tsv(unzip(temp_zip, "2020_Gaz_counties_national.txt")) %>%
  transmute(
    county_fips = str_pad(as.character(GEOID), 5, "left", "0"),
    land_area_sqmi = ALAND_SQMI
  )

profile_long <- profile_long %>%
  left_join(land_area_df, by = "county_fips") %>%
  mutate(population_density = population / land_area_sqmi)

# -------------------------
# 8. Final Merge and Save
# -------------------------
final_panel_full <- panel_df %>%
  left_join(profile_long, by = c("county_fips", "year")) %>%
  arrange(state, county, year)

write_csv(final_panel_full, output_file)
message("SUCCESS: Final panel saved to: ", output_file)



