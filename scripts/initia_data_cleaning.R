# =========================================================
# Merge all U.S. counties with Appalachian county indicator
# =========================================================

library(readxl)
library(readr)
library(dplyr)
library(stringr)

# ---------------------------------------------------------
# 1. Set file paths
# ---------------------------------------------------------
appalachian_file <- "Data/processed/cleaned_dataset.xlsx"
all_counties_file <- "Data/raw/states_and_county_fips.csv"

output_file <- "Data/processed/final_county_appalachian_dataset.csv"

# my data folders/files
list.files("Data/raw")
list.files("Data/processed")

# ---------------------------------------------------------
# 2. Read the data
# ---------------------------------------------------------
appalachian_df <- read_excel(appalachian_file)
all_counties_df <- read_csv(all_counties_file, show_col_types = FALSE)

# ---------------------------------------------------------
# 3. Create a county-name cleaning function
# ---------------------------------------------------------
# This makes county names more consistent across files

clean_county_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_trim() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_remove("\\s+county$") %>%
    str_remove("\\s+parish$") %>%
    str_remove("\\s+borough$") %>%
    str_remove("\\s+census area$") %>%
    str_remove("\\s+municipality$") %>%
    str_remove("\\s+city and borough$") %>%
    str_remove("\\s+city$") %>%
    str_replace("^st\\s", "saint ") %>%
    str_trim()
}

# ---------------------------------------------------------
# 4. Clean the Appalachian dataset
# ---------------------------------------------------------
appalachian_clean <- appalachian_df %>%
  transmute(
    state = str_to_upper(State),
    county = County,
    Appalachian = as.integer(Appalachian),
    county_clean = clean_county_name(County)
  ) %>%
  distinct(state, county_clean, .keep_all = TRUE)

# ---------------------------------------------------------
# 5. Clean the full U.S. counties dataset
# ---------------------------------------------------------
# Remove rows that are state-level summary rows like "ALABAMA"
# because those do not have county abbreviations in 'state'

all_counties_clean <- all_counties_df %>%
  filter(!is.na(state)) %>%
  transmute(
    state = str_to_upper(state),
    county = name,
    county_fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
    county_clean = clean_county_name(name)
  )

# ---------------------------------------------------------
# 6. Merge: keep ALL U.S. counties, bring in Appalachian flag
# ---------------------------------------------------------
final_counties <- all_counties_clean %>%
  left_join(
    appalachian_clean %>% select(state, county_clean, Appalachian),
    by = c("state", "county_clean")
  ) %>%
  mutate(Appalachian = if_else(is.na(Appalachian), 0L, Appalachian)) %>%
  select(
    state,
    county,
    county_fips,
    Appalachian
  ) %>%
  arrange(state, county)

# ---------------------------------------------------------
# 7. Check merge results
# ---------------------------------------------------------
cat("Number of rows in final dataset:", nrow(final_counties), "\n")
cat("Number of Appalachian counties:", sum(final_counties$Appalachian, na.rm = TRUE), "\n")
cat("Number of non-Appalachian counties:", sum(final_counties$Appalachian == 0, na.rm = TRUE), "\n")

# Optional: check unmatched counties from your Appalachian file
unmatched_appalachian <- appalachian_clean %>%
  anti_join(all_counties_clean, by = c("state", "county_clean"))

cat("Number of Appalachian rows not matched to full county file:", nrow(unmatched_appalachian), "\n")

if (nrow(unmatched_appalachian) > 0) {
  print(unmatched_appalachian)
}

# ---------------------------------------------------------
# 8. Save final dataset
# ---------------------------------------------------------
write_csv(final_counties, output_file)

cat("Final merged dataset saved to:\n", output_file, "\n")
