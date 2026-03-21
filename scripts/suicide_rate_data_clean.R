# =========================================================
# Merge Suicide Rate with Imputation (Rolling Average)
# =========================================================

# Install readxl if you don't have it: install.packages("readxl")
library(readxl) 
library(readr)
library(dplyr)
library(stringr)
library(zoo) 
library(janitor)

# -------------------------
# 1. File Paths
# -------------------------
panel_file   <- "Data/Processed/final_county_panel_COMPLETE1.csv"
suicide_file <- "Data/Raw/suicide_rates.xlsx" 
output_file  <- "Data/Processed/final_county_panel_ALL.csv"

# -------------------------
# 2. Read and Clean Suicide Data
# -------------------------
if (!file.exists(suicide_file)) {
  stop(paste("File not found at:", suicide_file))
}

# Use read_xlsx instead of read_csv
suicide_raw <- read_xlsx(suicide_file) %>%
  clean_names() %>%
  rename(
    county_fips = county_code, 
    year = year
  ) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year),
    # Convert "Unreliable" or other text to NA
    suicide_rate = as.numeric(as.character(crude_rate))
  ) %>%
  select(county_fips, year, suicide_rate)

# -------------------------
# 3. Read Current Panel
# -------------------------
if (!file.exists(panel_file)) stop("Base panel file not found.")

panel_df <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year)
  )

# -------------------------
# 4. Merge and Impute Missing Values
# -------------------------
final_panel_all <- panel_df %>%
  left_join(suicide_raw, by = c("county_fips", "year")) %>%
  group_by(county_fips) %>%
  arrange(year) %>%
  # Imputation: If NA, take the average of the two most recent years
  mutate(
    # rollapplyr with width=2 and na.rm=TRUE calculates the mean of available 
    # data in the current and previous slot.
    suicide_rate = rollapplyr(suicide_rate, width = 2, FUN = mean, na.rm = TRUE, fill = NA),
    
    # Fill any remaining gaps (like the first year of a series) 
    # using the closest available observation
    suicide_rate = na.locf(suicide_rate, na.rm = FALSE),
    suicide_rate = na.locf(suicide_rate, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

# -------------------------
# 5. Save Final Result
# -------------------------
write_csv(final_panel_all, output_file)

cat("Success: Dataset merged and saved as final_county_panel_ALL.csv\n")


