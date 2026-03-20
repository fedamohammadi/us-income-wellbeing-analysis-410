# =========================================================
# Clean and merge county personal income data
# =========================================================

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# -------------------------
# 2. File paths
# -------------------------
base_file   <- "Data/processed/final_county_appalachian_dataset.csv"
income_file <- "Data/raw/County personal income.csv"

output_file <- "Data/processed/final_county_panel_with_income.csv"

# -------------------------
# 3. Read base county file
# -------------------------
base_counties <- read_csv(base_file, show_col_types = FALSE) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    state = str_to_upper(state)
  )

# -------------------------
# 4. Create county-year skeleton
# -------------------------
years <- 2011:2023

county_year_panel <- base_counties %>%
  tidyr::crossing(year = years)

# -------------------------
# 5. Read personal income data
# -------------------------
income_raw <- read_csv(
  income_file,
  na = c("", "NA", "(NA)"),
  show_col_types = FALSE
)

# -------------------------
# 6. Clean personal income data
# -------------------------
# Keep only the needed columns
# Remove accidental unnamed column if it exists
# Convert GeoFIPS to padded 5-digit county_fips
# Reshape from wide to long

income_long <- income_raw %>%
  select(-any_of("Unnamed: 15")) %>%
  mutate(
    county_fips = str_pad(as.character(GeoFIPS), width = 5, side = "left", pad = "0")
  ) %>%
  pivot_longer(
    cols = matches("^20(11|12|13|14|15|16|17|18|19|20|21|22|23)$"),
    names_to = "year",
    values_to = "personal_income"
  ) %>%
  mutate(
    year = as.integer(year),
    personal_income = parse_number(as.character(personal_income))
  ) %>%
  select(county_fips, year, personal_income)

# -------------------------
# 7. Merge into county-year panel
# -------------------------
final_panel_income <- county_year_panel %>%
  left_join(income_long, by = c("county_fips", "year")) %>%
  arrange(state, county, year)

# -------------------------
# 8. Quick checks
# -------------------------
cat("Rows in base counties file:", nrow(base_counties), "\n")
cat("Rows in county-year panel:", nrow(county_year_panel), "\n")
cat("Rows in income_long:", nrow(income_long), "\n")
cat("Rows in final merged panel:", nrow(final_panel_income), "\n")

cat("Number of missing personal income values:",
    sum(is.na(final_panel_income$personal_income)), "\n")

# Counties in income file that do NOT match your base county dataset
unmatched_income_fips <- income_long %>%
  distinct(county_fips) %>%
  anti_join(base_counties %>% distinct(county_fips), by = "county_fips")

cat("Number of county FIPS in income file not matched to base county file:",
    nrow(unmatched_income_fips), "\n")

if (nrow(unmatched_income_fips) > 0) {
  print(unmatched_income_fips)
}

# Counties in your base dataset missing from income file
missing_income_fips <- base_counties %>%
  distinct(county_fips, state, county) %>%
  anti_join(income_long %>% distinct(county_fips), by = "county_fips")

cat("Number of base counties with no income match:",
    nrow(missing_income_fips), "\n")

if (nrow(missing_income_fips) > 0) {
  print(missing_income_fips)
}

# -------------------------
# 9. Save cleaned merged file
# -------------------------
write_csv(final_panel_income, output_file)

cat("Final file saved to:\n", output_file, "\n")










