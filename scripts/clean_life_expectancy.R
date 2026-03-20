# =========================================================
# Merge 2009 Life Expectancy as Base-Year Controls
# =========================================================

library(readr)
library(dplyr)
library(stringr)

panel_file   <- "Data/Processed/final_county_panel_full.csv"
lifeexp_file <- "Data/Raw/US_COUNTY_LIFE_EXPECTANCY.csv"
output_file  <- "Data/Processed/final_county_panel_full_1.csv"

# -------------------------
# 2. Read current final panel
# -------------------------
panel_df <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(
    # Standardize FIPS to 5-digit string for matching
    county_fips = str_pad(as.character(county_fips), width = 5, side = "left", pad = "0"),
    year = as.integer(year) 
  )

# -------------------------
# 3. Read and Clean life expectancy data (2009 only)
# -------------------------
# We extract only the values for 2009 to serve as a base-year control
lifeexp_2009 <- read_csv(lifeexp_file, show_col_types = FALSE) %>%
  filter(Year == 2009) %>%
  mutate(
    # Create matching key
    county_fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0")
  ) %>%
  transmute(
    county_fips,
    # Pulling only the requested expectancy values
    male_life_expectancy   = as.numeric(`Male life expectancy (years)`),
    female_life_expectancy = as.numeric(`Female life expectancy (years)`)
  ) %>%
  # Ensure one unique row per county
  distinct(county_fips, .keep_all = TRUE)

# -------------------------
# 4. Merge into the panel
# -------------------------
# Because we only join by 'county_fips', the 2009 values will be 
# automatically applied to every year (2011-2023) in your panel.
final_panel_full_1 <- panel_df %>%
  left_join(lifeexp_2009, by = "county_fips") %>%
  arrange(state, county, year)

# -------------------------
# 5. Quick check and Save
# -------------------------
cat("Rows in panel:", nrow(final_panel_full_1), "\n")
cat("Missing male life expectancy:", sum(is.na(final_panel_full_1$male_life_expectancy)), "\n")

write_csv(final_panel_full_1, output_file)

cat("Successfully saved to:\n", output_file, "\n")




