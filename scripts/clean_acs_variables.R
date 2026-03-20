# =========================================================
# Pull and merge ACS 5-year county data for ALL counties
# Variables: poverty_rate, gini, bachelors_share
# =========================================================

library(tidycensus)
library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

# -------------------------
# 1. Census API key
# -------------------------
census_api_key("my API key", 
               install = FALSE, 
               overwrite = TRUE)


base_file   <- "Data/processed/final_county_panel_with_income_labor.csv"
output_file <- "Data/processed/final_county_panel_with_income_labor_acs.csv"

# -------------------------
# 3. Read current panel
# -------------------------
# Wrapped in try() in case file doesn't exist yet for testing
panel_df <- read_csv(base_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year)
  )

# -------------------------
# 4. Detailed-table variables (Cleaned)
# -------------------------
# We define them without the "E" suffix here
acs_vars <- c(
  gini            = "B19083_001",  # Gini index
  pov_total       = "C17002_001",  # Poverty total (Universe)
  pov_under_50    = "C17002_002",  # Under 0.50
  pov_50_99       = "C17002_003",  # 0.50 to 0.99
  edu_total       = "B15003_001",  # Education total 25+
  edu_bachelors   = "B15003_022",  # Bachelor's
  edu_masters     = "B15003_023",  # Master's
  edu_professional= "B15003_024",  # Professional
  edu_doctorate   = "B15003_025"   # Doctorate
)

# -------------------------
# 5. Function to pull one year
# -------------------------
get_acs_year <- function(yr) {
  message("Pulling ACS 5-year data for year ", yr, " ...")
  
  # Fetch data
  df <- tryCatch({
    get_acs(
      geography = "county",
      variables = acs_vars,
      year = yr,
      survey = "acs5",
      output = "wide", # This appends 'E' to our names automatically
      geometry = FALSE,
      cache_table = TRUE
    )
  }, error = function(e) {
    message("Data for year ", yr, " might not be available yet or API error occurred.")
    return(NULL)
  })
  
  if (is.null(df)) return(NULL)
  
  # Calculate rates using the 'E' suffix added by output="wide"
  df %>%
    transmute(
      county_fips = GEOID,
      year = as.integer(yr),
      
      gini = giniE,
      
      poverty_rate = if_else(
        pov_totalE > 0,
        ((pov_under_50E + pov_50_99E) / pov_totalE) * 100,
        NA_real_
      ),
      
      bachelors_share = if_else(
        edu_totalE > 0,
        ((edu_bachelorsE + edu_mastersE + edu_professionalE + edu_doctorateE) / edu_totalE) * 100,
        NA_real_
      )
    )
}

# -------------------------
# 6. Pull all years (2011-2023)
# -------------------------
years <- 2011:2023
acs_list <- list()

for (yr in years) {
  dat <- get_acs_year(yr)
  if (!is.null(dat)) {
    acs_list[[as.character(yr)]] <- dat
  }
  Sys.sleep(1) # Be nice to the API
}

acs_long <- bind_rows(acs_list) %>%
  arrange(county_fips, year)

# -------------------------
# 7. Merge and Save
# -------------------------
final_panel_acs <- panel_df %>%
  left_join(acs_long, by = c("county_fips", "year")) %>%
  arrange(county_fips, year)

write_csv(final_panel_acs, output_file)
message("Process complete. File saved to: ", output_file)






