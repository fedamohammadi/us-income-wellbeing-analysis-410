# =========================================================
# Clean and merge Poor Mental Health Days (2011-2023)
# County Health Rankings Analytic Data
# =========================================================

library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)

# -------------------------
# 1. File paths (Using Capitalized Names for consistency)
# -------------------------
panel_file  <- "Data/Processed/final_county_panel_full_1.csv"
raw_folder  <- "Data/Raw"
output_file <- "Data/Processed/final_county_panel_COMPLETE.csv"

# -------------------------
# 2. Read current panel
# -------------------------
if (!file.exists(panel_file)) stop("Panel file not found.")

panel_df <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year)
  )

# -------------------------
# 3. Find all mental health files
# -------------------------
# Looks for files starting with 'analytic_data' and ending in '.csv'
mh_files <- list.files(
  path = raw_folder,
  pattern = "analytic_data.*\\.csv$",
  full.names = TRUE
)

if (length(mh_files) == 0) {
  stop("No analytic_data files found in Data/Raw. Please check file names.")
}

# -------------------------
# 4. Robust Function to read CHRR files
# -------------------------
read_mh_file <- function(file_path) {
  
  # Extract year from filename
  year_val <- str_extract(basename(file_path), "20[0-9]{2}") %>% as.integer()
  message("Processing year: ", year_val)
  
  # Read the CSV
  # CHRR files often have the variable names in row 1 and labels in row 2.
  raw_df <- read_csv(file_path, show_col_types = FALSE, col_names = TRUE)
  
  # If the first row is a label row (contains letters like "FIPS"), remove it
  if (is.character(raw_df[[1]][1]) && str_detect(raw_df[[1]][1], "[A-Za-z]")) {
    raw_df <- raw_df[-1, ]
  }
  
  # Clean names to handle capitalization/spaces
  df <- raw_df %>% clean_names()
  
  # Robustly identify the FIPS column (it might be 'fips', 'fips_code', or 'x5_digit_fips_code')
  fips_col <- intersect(names(df), c("fips", "fips_code", "x5_digit_fips_code"))[1]
  
  # Robustly identify the Mental Health column
  # It is usually 'poor_mental_health_days_raw_value' or 'v002_raw_value'
  mh_col <- intersect(names(df), c("poor_mental_health_days_raw_value", "v002_raw_value"))[1]
  
  if (is.na(fips_col)) stop(paste("Could not find FIPS column in", file_path))
  if (is.na(mh_col)) {
    message("Warning: MH column not found in ", year_val, ". Skipping.")
    return(NULL)
  }
  
  # Clean and select
  df_clean <- df %>%
    mutate(
      county_fips = str_pad(as.character(get(fips_col)), width = 5, side = "left", pad = "0"),
      year = year_val,
      poor_mental_health_days = as.numeric(get(mh_col))
    ) %>%
    filter(!is.na(county_fips)) %>%
    select(county_fips, year, poor_mental_health_days) %>%
    distinct(county_fips, year, .keep_all = TRUE)
  
  return(df_clean)
}

# -------------------------
# 5. Read, stack, and merge
# -------------------------
mh_long <- map_dfr(mh_files, read_mh_file)

final_panel_complete <- panel_df %>%
  left_join(mh_long, by = c("county_fips", "year")) %>%
  arrange(state, county, year)

# -------------------------
# 6. Save and Report
# -------------------------
write_csv(final_panel_complete, output_file)

cat("\nSummary Statistics:\n")
cat("Rows in final panel:", nrow(final_panel_complete), "\n")
cat("Years covered:", paste(sort(unique(mh_long$year)), collapse = ", "), "\n")
cat("Missing Mental Health values:", sum(is.na(final_panel_complete$poor_mental_health_days)), "\n")
cat("Final dataset saved to:", output_file, "\n")

