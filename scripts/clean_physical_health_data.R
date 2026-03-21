# =========================================================
# Clean and merge Poor Physical Health Days (2011-2023)
# Source: County Health Rankings (CHRR)
# =========================================================

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)

# -------------------------
# 1. File Paths
# -------------------------
panel_file   <- "Data/Processed/final_county_panel_COMPLETE.csv"
health_folder <- "Data/Raw/Health Data"
# Output file name
output_file  <- "Data/Processed/final_county_panel_COMPLETE1.csv" 

# -------------------------
# 2. Read Current Panel
# -------------------------
if (!file.exists(panel_file)) stop("Base panel file not found.")

panel_df <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year)
  )

# -------------------------
# 3. Find all health files
# -------------------------
mh_files <- list.files(
  path = health_folder,
  pattern = "analytic_data.*\\.csv$",
  full.names = TRUE
)

if (length(mh_files) == 0) {
  stop(paste("No analytic_data files found in:", health_folder))
}

# -------------------------
# 4. Robust Function to read CHRR Physical Health column
# -------------------------
read_phys_health_file <- function(file_path) {
  
  # Extract year from filename
  year_val <- str_extract(basename(file_path), "20[0-9]{2}") %>% as.integer()
  message("Processing year: ", year_val)
  
  # Read the CSV (CHRR files have variables in row 1, descriptions in row 2)
  raw_df <- read_csv(file_path, show_col_types = FALSE, col_names = TRUE)
  
  # Skip the description row if it contains text instead of a FIPS code
  if (is.character(raw_df[[1]][1]) && str_detect(raw_df[[1]][1], "[A-Za-z]")) {
    raw_df <- raw_df[-1, ]
  }
  
  # Clean names to handle capitalization/spaces
  df <- raw_df %>% clean_names()
  
  # Identify the FIPS column (varies by year)
  fips_col <- intersect(names(df), c("x5_digit_fips_code", "fips_code", "fips"))[1]
  
  # Identify the Physical Health column
  # Common names: poor_physical_health_days_raw_value or v036_raw_value
  phys_col <- intersect(names(df), c("poor_physical_health_days_raw_value", "v036_raw_value"))[1]
  
  if (is.na(fips_col)) {
    message("Warning: FIPS column not found in ", year_val, ". Skipping.")
    return(NULL)
  }
  
  if (is.na(phys_col)) {
    message("Warning: Physical Health column not found in ", year_val, ". Skipping.")
    return(NULL)
  }
  
  # Clean and select
  df_clean <- df %>%
    mutate(
      county_fips = str_pad(as.character(get(fips_col)), width = 5, side = "left", pad = "0"),
      year = year_val,
      poor_physical_health_days = as.numeric(get(phys_col))
    ) %>%
    filter(!is.na(county_fips)) %>%
    select(county_fips, year, poor_physical_health_days) %>%
    distinct(county_fips, year, .keep_all = TRUE)
  
  return(df_clean)
}

# -------------------------
# 5. Read, Stack, and Merge
# -------------------------
phys_health_long <- map_dfr(mh_files, read_phys_health_file)

# Merge into the existing panel
final_panel_updated <- panel_df %>%
  left_join(phys_health_long, by = c("county_fips", "year")) %>%
  arrange(state, county, year)

# -------------------------
# 6. Save and Report
# -------------------------
write_csv(final_panel_updated, output_file)

cat("\nSummary of Merge:\n")
cat("Rows in final panel:", nrow(final_panel_updated), "\n")
cat("Missing Physical Health values:", sum(is.na(final_panel_updated$poor_physical_health_days)), "\n")
cat("Final dataset saved to:", output_file, "\n")
















