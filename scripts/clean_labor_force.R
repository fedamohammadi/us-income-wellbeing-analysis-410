# =========================================================
# Clean and merge county labor force data (2011-2023)
# =========================================================

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(janitor)

# -------------------------
# 2. File paths
# -------------------------
base_file   <- "Data/processed/final_county_panel_with_income.csv"
raw_folder  <- "Data/raw"
output_file <- "Data/processed/final_county_panel_with_income_labor.csv"

# -------------------------
# 3. Read current panel file
# -------------------------
panel_df <- read_csv(base_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year)
  )

# -------------------------
# 4. Find all labor force Excel files
# -------------------------
labor_files <- list.files(
  path = raw_folder,
  pattern = "^laborfrc(2011|2012|2013|2014|2015|2016|2017|2018|2019|2020|2021|2022|2023)\\.xlsx$",
  full.names = TRUE
)

if (length(labor_files) == 0) {
  stop("No labor force Excel files were found in Data/raw.")
}

# -------------------------
# 5. Function to read one labor force file
# -------------------------
read_labor_file <- function(file_path) {
  
  # Read the first sheet, skipping the title row
  df <- read_excel(file_path, skip = 1) %>%
    clean_names()
  
  # Standardize and keep only needed columns
  df_clean <- df %>%
    transmute(
      laus_code = as.character(laus_code),
      state_fips = str_pad(as.character(state_fips_code), width = 2, side = "left", pad = "0"),
      county_fips_part = str_pad(as.character(county_fips_code), width = 3, side = "left", pad = "0"),
      county_name_state = as.character(county_name_state_abbreviation),
      year = as.integer(year),
      labor_force = as.numeric(labor_force),
      employed = as.numeric(employed),
      unemployed = as.numeric(unemployed),
      unemployment_rate = as.numeric(unemployment_rate_percent)
    ) %>%
    mutate(
      county_fips = paste0(state_fips, county_fips_part)
    ) %>%
    select(
      county_fips,
      year,
      labor_force,
      employed,
      unemployed,
      unemployment_rate
    )
  
  return(df_clean)
}

# -------------------------
# 6. Read and stack all yearly labor files
# -------------------------
labor_long <- map_dfr(labor_files, read_labor_file) %>%
  distinct(county_fips, year, .keep_all = TRUE) %>%
  arrange(county_fips, year)

# -------------------------
# 7. Quick checks on labor data
# -------------------------
cat("Number of labor files found:", length(labor_files), "\n")
cat("Rows in stacked labor dataset:", nrow(labor_long), "\n")
cat("Years in labor dataset:\n")
print(sort(unique(labor_long$year)))

cat("Missing unemployment_rate values:", sum(is.na(labor_long$unemployment_rate)), "\n")

# -------------------------
# 8. Merge labor data into current panel
# -------------------------
final_panel_labor <- panel_df %>%
  left_join(labor_long, by = c("county_fips", "year")) %>%
  arrange(state, county, year)

# -------------------------
# 9. Merge checks
# -------------------------
cat("Rows in panel before merge:", nrow(panel_df), "\n")
cat("Rows in panel after merge:", nrow(final_panel_labor), "\n")

cat("Missing labor_force values after merge:",
    sum(is.na(final_panel_labor$labor_force)), "\n")

cat("Missing employed values after merge:",
    sum(is.na(final_panel_labor$employed)), "\n")

cat("Missing unemployed values after merge:",
    sum(is.na(final_panel_labor$unemployed)), "\n")

cat("Missing unemployment_rate values after merge:",
    sum(is.na(final_panel_labor$unemployment_rate)), "\n")

# Counties in labor data not found in panel
unmatched_labor <- labor_long %>%
  distinct(county_fips) %>%
  anti_join(panel_df %>% distinct(county_fips), by = "county_fips")

cat("County FIPS in labor data not found in panel:",
    nrow(unmatched_labor), "\n")

if (nrow(unmatched_labor) > 0) {
  print(unmatched_labor)
}

# Counties-years in panel with no labor match
missing_labor_matches <- panel_df %>%
  distinct(county_fips, year) %>%
  anti_join(labor_long %>% distinct(county_fips, year), by = c("county_fips", "year"))

cat("County-year rows in panel with no labor match:",
    nrow(missing_labor_matches), "\n")

if (nrow(missing_labor_matches) > 0) {
  print(head(missing_labor_matches, 20))
}

# -------------------------
# 10. Save final merged file
# -------------------------
write_csv(final_panel_labor, output_file)

cat("Final file saved to:\n", output_file, "\n")


# to view the data

View(final_panel_labor)
summary(final_panel_labor$unemployment_rate)
table(final_panel_labor$year)






