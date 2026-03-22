# =========================================================
# FINAL CLEANING + MODEL DATASETS + REGRESSIONS
# =========================================================

library(tidyverse)
library(fixest)
library(modelsummary)


df <- read_csv("Data/Processed/final_county_panel_COMPLETE1.csv", show_col_types = FALSE)

# -------------------------
# 2. Basic panel check
# -------------------------
# county_fips + year should uniquely identify observations
dup_check <- df %>%
  count(county_fips, year) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  stop("Duplicate county_fips-year observations found. Fix before continuing.")
} else {
  cat("Good: No duplicate county_fips-year observations found.\n")
}

# -------------------------
# 3. Drop corrupted variable
# -------------------------
# median_age was corrupted in my dataset, so I remove it for now
df <- df %>%
  select(-median_age)

# -------------------------
# 4. Create overall life expectancy
# -------------------------
# Average of male and female life expectancy
df <- df %>%
  mutate(
    life_expectancy = rowMeans(
      cbind(male_life_expectancy, female_life_expectancy),
      na.rm = TRUE
    )
  )

# -------------------------
# 5. Create income per capita
# -------------------------
# personal_income is in thousands of dollars at the county total level
# Divide by population to get per-capita income in thousands of dollars
df <- df %>%
  mutate(
    income_per_capita = personal_income / population
  )

# -------------------------
# 6. Winsorize income at the 99th percentile
# -------------------------
# This reduces the influence of extreme high-income counties
p99 <- quantile(df$income_per_capita, 0.99, na.rm = TRUE)

df <- df %>%
  mutate(
    income_pc_w = if_else(income_per_capita > p99, p99, income_per_capita),
    log_income  = log(income_pc_w)
  )

# -------------------------
# 7. Construct well-being index
# -------------------------
# Higher index = better well-being
# life_expectancy: higher is better
# poor_mental_health_days: higher is worse, so multiply by -1
# poor_physical_health_days: higher is worse, so multiply by -1

df <- df %>%
  mutate(
    mental_good   = -poor_mental_health_days,
    physical_good = -poor_physical_health_days,
    z_life        = as.numeric(scale(life_expectancy)),
    z_mental      = as.numeric(scale(mental_good)),
    z_physical    = as.numeric(scale(physical_good)),
    wellbeing_index = rowMeans(
      cbind(z_life, z_mental, z_physical),
      na.rm = FALSE
    )
  )

# -------------------------
# 8. Quick sanity checks
# -------------------------
cat("\n===== QUICK SUMMARIES =====\n")
summary(df$income_per_capita)
summary(df$log_income)
summary(df$life_expectancy)
summary(df$poor_mental_health_days)
summary(df$poor_physical_health_days)
summary(df$wellbeing_index)

cat("\n===== MISSING VALUES =====\n")
print(
  colSums(is.na(df)) %>%
    sort(decreasing = TRUE)
)

# -------------------------
# 9. Define control variables
# -------------------------
# We are not including race variables for now
# because they are incomplete shares and not central to your design

controls <- c(
  "unemployment_rate",
  "poverty_rate",
  "bachelors_share",
  "uninsured_rate",
  "population_density",
  "gini"
)

# Keep only controls that actually exist in the dataset
controls <- intersect(controls, names(df))

cat("\nControls used in models:\n")
print(controls)

# -------------------------
# 10. Keep final analysis columns
# -------------------------
analysis_vars <- c(
  "state",
  "county",
  "county_fips",
  "year",
  "Appalachian",
  "log_income",
  "income_pc_w",
  "life_expectancy",
  "poor_mental_health_days",
  "poor_physical_health_days",
  "wellbeing_index",
  controls
)

analysis_df <- df %>%
  select(all_of(analysis_vars))

# -------------------------
# 11. Create model-specific datasets
# -------------------------
# Note:
# Each model uses complete cases only for:
# - dependent variable
# - log income
# - Appalachian
# - all controls used

model1_df <- analysis_df %>%
  filter(
    !is.na(poor_mental_health_days),
    !is.na(log_income),
    !is.na(Appalachian)
  ) %>%
  drop_na(all_of(controls))

model2_df <- analysis_df %>%
  filter(
    !is.na(life_expectancy),
    !is.na(log_income),
    !is.na(Appalachian)
  ) %>%
  drop_na(all_of(controls))

model3_df <- analysis_df %>%
  filter(
    !is.na(wellbeing_index),
    !is.na(log_income),
    !is.na(Appalachian)
  ) %>%
  drop_na(all_of(controls))

# -------------------------
# 12. Inspect final model dataset sizes
# -------------------------
cat("\n===== MODEL DATASET DIMENSIONS =====\n")
cat("Model 1:", dim(model1_df), "\n")
cat("Model 2:", dim(model2_df), "\n")
cat("Model 3:", dim(model3_df), "\n")

# -------------------------
# 13. Save model datasets
# -------------------------
write_csv(model1_df, "model1_mental_health_dataset.csv")
write_csv(model2_df, "model2_life_expectancy_dataset.csv")
write_csv(model3_df, "model3_wellbeing_index_dataset.csv")

# save the combined analysis dataset too
write_csv(analysis_df, "final_analysis_dataset.csv")

# -------------------------
# 14. Build regression formulas
# -------------------------
# Main variable: log_income
# Key interaction: log_income * Appalachian
# Fixed effects: county_fips + year
# Clustered SEs: county_fips

rhs <- paste(
  c("log_income * Appalachian", controls),
  collapse = " + "
)

fml1 <- as.formula(paste("poor_mental_health_days ~", rhs, "| county_fips + year"))
fml2 <- as.formula(paste("life_expectancy ~", rhs, "| county_fips + year"))
fml3 <- as.formula(paste("wellbeing_index ~", rhs, "| county_fips + year"))

# -------------------------
# 15. Run regressions
# -------------------------
# County fixed effects + year fixed effects
# Cluster standard errors by county_fips

model1 <- feols(
  fml1,
  data = model1_df,
  cluster = ~county_fips
)

fml2 <- as.formula(paste("life_expectancy ~", rhs, "| year"))

model2 <- feols(
  fml2,
  data = model2_df,
  cluster = ~county_fips
)



model3 <- feols(
  fml3,
  data = model3_df,
  cluster = ~county_fips
)


# -------------------------
# 16. Show regression summaries
# -------------------------
cat("\n===== MODEL 1: POOR MENTAL HEALTH DAYS =====\n")
print(summary(model1))

cat("\n===== MODEL 2: LIFE EXPECTANCY =====\n")
print(summary(model2))

cat("\n===== MODEL 3: WELL-BEING INDEX =====\n")
print(summary(model3))

# -------------------------
# 17. Export regression table
# -------------------------
# This creates a clean table you can inspect later
modelsummary(
  list(
    "Mental Health Days" = model1,
    "Life Expectancy"    = model2,
    "Well-Being Index"   = model3
  ),
  output = "regression_results.html",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|Within|Pseudo"
)

# Optional Word file if needed later
modelsummary(
  list(
    "Mental Health Days" = model1,
    "Life Expectancy"    = model2,
    "Well-Being Index"   = model3
  ),
  output = "regression_results.docx",
  stars = TRUE,
  gof_omit = "IC|Log|Adj|Within|Pseudo"
)

# -------------------------
# 18. Save models as R objects
# -------------------------
saveRDS(model1, "model1_mental_health.rds")
saveRDS(model2, "model2_life_expectancy.rds")
saveRDS(model3, "model3_wellbeing_index.rds")


# ====================================================
# Robustness check : Heteroskedasticity
# ====================================================

install.packages("lmtest")
library(lmtest)

# Test model 1

ols1 <- lm(
  poor_mental_health_days ~ log_income * Appalachian +
    unemployment_rate + poverty_rate + bachelors_share +
    uninsured_rate + population_density + gini,
  data = model1_df
)

bptest(ols1)


# Test model 2

ols2 <- lm(
  life_expectancy ~ log_income * Appalachian +
    unemployment_rate + poverty_rate + bachelors_share +
    uninsured_rate + population_density + gini,
  data = model2_df
)

bptest(ols2)


# Test model 3

ols3 <- lm(
  wellbeing_index ~ log_income * Appalachian +
    unemployment_rate + poverty_rate + bachelors_share +
    uninsured_rate + population_density + gini,
  data = model3_df
)

bptest(ols3)



# fixing for Heteroskedasticity

model1_plain <- feols(
  fml1,
  data = model1_df
)


model1_robust <- feols(
  fml1,
  data = model1_df,
  vcov = "hetero"
)



model1_cluster <- model1




etable(
  model1_plain,
  model1_robust,
  model1_cluster,
  headers = c("Plain", "Robust", "Clustered")
)
















