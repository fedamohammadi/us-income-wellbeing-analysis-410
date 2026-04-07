library(tidyverse)
library(fixest)
library(modelsummary)
library(patchwork)
library(ggthemes)
library(ggcorrplot)
library(usmap)
library(usmap)
library(dplyr)
library(ggplot2)

# ---------------------------------------------------------
# 1. READ AND CLEAN DATA
# ---------------------------------------------------------

df <- read_csv("Data/Processed/final_county_panel_COMPLETE1.csv", show_col_types = FALSE)

df <- df %>% 
  select(-any_of("median_age")) %>%
  mutate(
    life_expectancy = rowMeans(cbind(male_life_expectancy, female_life_expectancy), na.rm = TRUE),
    income_per_capita = personal_income / population
  )

p99 <- quantile(df$income_per_capita, 0.99, na.rm = TRUE)

df <- df %>%
  mutate(
    income_pc_w = if_else(income_per_capita > p99, p99, income_per_capita),
    log_income  = log(income_pc_w),
    mental_good = -poor_mental_health_days,
    physical_good = -poor_physical_health_days,
    z_life = as.numeric(scale(life_expectancy)),
    z_mental = as.numeric(scale(mental_good)),
    z_physical = as.numeric(scale(physical_good)),
    wellbeing_index = rowMeans(cbind(z_life, z_mental, z_physical), na.rm = FALSE),
    Appalachia_Label = factor(ifelse(Appalachian == 1, "Appalachian", "Non-Appalachian"))
  )

# ---------------------------------------------------------
# 2. RUN MODELS
# ---------------------------------------------------------

controls <- c(
  "unemployment_rate",
  "poverty_rate",
  "bachelors_share",
  "uninsured_rate",
  "population_density",
  "gini"
)

fml1 <- as.formula(paste(
  "poor_mental_health_days ~ log_income * Appalachian +",
  paste(controls, collapse = " + "),
  "| county_fips + year"
))

fml2 <- as.formula(paste(
  "life_expectancy ~ log_income * Appalachian +",
  paste(controls, collapse = " + "),
  "| year"
))

fml3 <- as.formula(paste(
  "wellbeing_index ~ log_income * Appalachian +",
  paste(controls, collapse = " + "),
  "| county_fips + year"
))

model1 <- feols(fml1, data = df, cluster = ~county_fips)
model2 <- feols(fml2, data = df, cluster = ~county_fips)
model3 <- feols(fml3, data = df, cluster = ~county_fips)

# ---------------------------------------------------------
# FIGURE 1: Distribution of Log Income
# ---------------------------------------------------------

p1_hist <- ggplot(df, aes(x = log_income)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white",
    linewidth = 0.2
  ) +
  labs(
    title = "Distribution of Log Income Per Capita",
    x = "Log Income Per Capita",
    y = "Frequency"
  ) +
  
  geom_vline(
    xintercept = mean(df$log_income, na.rm = TRUE),
    color = "red",
    linetype = "dashed",
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Log Income Per Capita",
    x = "Log Income Per Capita",
    y = "Frequency"
  ) +
  theme_minimal()

  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )

  
print(p1_hist)




# Raw income
p_raw <- ggplot(df, aes(x = income_per_capita)) +
  geom_histogram(
    bins = 40,
    fill = "darkorange",
    color = "white"
  ) +
  labs(
    title = "Raw Income Distribution",
    x = "Income Per Capita",
    y = "Frequency"
  ) +
  theme_minimal()

# Log income
p_log <- ggplot(df, aes(x = log_income)) +
  geom_histogram(
    bins = 40,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Log Income Distribution",
    x = "Log Income Per Capita",
    y = "Frequency"
  ) +
  theme_minimal()

# Side by side
library(patchwork)

p_compare <- p_raw | p_log

print(p_compare)

ggsave(
  "Output/figures/income_raw_vs_log.png",
  p_compare,
  width = 12,
  height = 5,
  dpi = 300,
  bg = "white"
)


# Save to laptop with white background
ggsave(
  filename = "Output/figures/hist_log_income.png",
  plot = p1_hist,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ---------------------------------------------------------
# FIGURE 2: Appalachia vs Non-Appalachia (Well-Being)
# ---------------------------------------------------------

p2_box <- ggplot(
  df %>% filter(!is.na(wellbeing_index), !is.na(Appalachia_Label)),
  aes(x = Appalachia_Label, y = wellbeing_index, fill = Appalachia_Label)
) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = 0.7
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3,
    color = "black"
  ) +
  coord_cartesian(ylim = c(-3, 3)) +
  scale_fill_manual(values = c("steelblue", "firebrick")) +
  labs(
    title = "Well-Being in Appalachian vs Non-Appalachian Counties",
    x = "",
    y = "Well-Being Index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p2_box)

# Save
ggsave(
  "Output/figures/boxplot_appalachia_wellbeing.png",
  p2_box,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)



# ---------------------------------------------------------
# FIGURE 3: Interaction Plot (Model 3)
# ---------------------------------------------------------

# Extract coefficients
b <- coef(model3)

# Compute means of controls
mean_unemp <- mean(df$unemployment_rate, na.rm = TRUE)
mean_pov   <- mean(df$poverty_rate, na.rm = TRUE)
mean_bach  <- mean(df$bachelors_share, na.rm = TRUE)
mean_unins <- mean(df$uninsured_rate, na.rm = TRUE)
mean_dens  <- mean(df$population_density, na.rm = TRUE)
mean_gini  <- mean(df$gini, na.rm = TRUE)

# Create income range
income_seq <- seq(
  min(df$log_income, na.rm = TRUE),
  max(df$log_income, na.rm = TRUE),
  length.out = 100
)

# Build prediction dataset
pred_data <- expand.grid(
  log_income = income_seq,
  Appalachian = c(0, 1)
)

# Compute predicted values first
pred_data <- pred_data %>%
  mutate(
    unemployment_rate = mean_unemp,
    poverty_rate = mean_pov,
    bachelors_share = mean_bach,
    uninsured_rate = mean_unins,
    population_density = mean_dens,
    gini = mean_gini,
    
    wellbeing_pred =
      b["log_income"] * log_income +
      b["unemployment_rate"] * unemployment_rate +
      b["poverty_rate"] * poverty_rate +
      b["bachelors_share"] * bachelors_share +
      b["uninsured_rate"] * uninsured_rate +
      b["population_density"] * population_density +
      b["gini"] * gini +
      b["log_income:Appalachian"] * (log_income * Appalachian),
    
    Region = factor(ifelse(Appalachian == 1, "Appalachia", "Non-Appalachia"))
  )

# Then center within each region
pred_data <- pred_data %>%
  group_by(Region) %>%
  mutate(
    wellbeing_centered = wellbeing_pred - min(wellbeing_pred)
  ) %>%
  ungroup()


# Plot
p3_interaction <- ggplot(pred_data, aes(x = log_income, y = wellbeing_centered, color = Region)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(values = c("blue", "red")) +
  labs(
    title = "Effect of Income on Well-Being by Region",
    subtitle = "Lines normalized to compare slopes",
    x = "Log Income",
    y = "Change in Well-Being Index",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )


print(p3_interaction)

# Save
ggsave(
  filename = "Output/figures/interaction_model3.png",
  plot = p3_interaction,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)




# ---------------------------------------------------------
# FIGURE: Marginal effect of income on well-being by region
# Model 3
# ---------------------------------------------------------

# Coefficients
b <- coef(model3)
V <- vcov(model3)

# Effect in Non-Appalachia
beta_nonapp <- b["log_income"]

# Effect in Appalachia
beta_app <- b["log_income"] + b["log_income:Appalachian"]

# Standard error for Non-Appalachia
se_nonapp <- sqrt(V["log_income", "log_income"])

# Standard error for Appalachia
se_app <- sqrt(
  V["log_income", "log_income"] +
    V["log_income:Appalachian", "log_income:Appalachian"] +
    2 * V["log_income", "log_income:Appalachian"]
)

# 95% confidence intervals
ci_nonapp_low  <- beta_nonapp - 1.96 * se_nonapp
ci_nonapp_high <- beta_nonapp + 1.96 * se_nonapp

ci_app_low  <- beta_app - 1.96 * se_app
ci_app_high <- beta_app + 1.96 * se_app

# Data frame for plotting
effect_df <- data.frame(
  Region = factor(
    c("Appalachia", "Non-Appalachia"),
    levels = c("Appalachia", "Non-Appalachia")
  ),
  Effect = c(beta_app, beta_nonapp),
  CI_low = c(ci_app_low, ci_nonapp_low),
  CI_high = c(ci_app_high, ci_nonapp_high)
)

# Plot
p_effect_bar <- ggplot(effect_df, aes(x = Region, y = Effect, fill = Region)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.14, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.3f", Effect)), vjust = -0.55, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Appalachia" = "steelblue",
                               "Non-Appalachia" = "firebrick")) +
  labs(
    title = "The Effect of Income on Well-Being in Appalachia vs Non-Appalchia",
    x = "",
    y = "Estimated Effect of Income on Well-Being",
    caption = "Bars show Model 3 marginal effects of log income on the well-being index (and it's clearly stronger in Appalachia).\nError bars represent 95% confidence intervals."
  ) +
  coord_cartesian(ylim = c(min(effect_df$CI_low) - 0.03, max(effect_df$CI_high) + 0.05)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(size = 10, color = "gray30", hjust = 0),
    plot.caption.position = "plot"
  )

print(p_effect_bar)

ggsave(
  "Output/figures/income_effect_wellbeing_by_region.png",
  p_effect_bar,
  width = 8,
  height = 5.5,
  dpi = 300,
  bg = "white"
)





# ---------------------------------------------------------
# FIGURE 4: Coefficient Plot 
# ---------------------------------------------------------

library(modelsummary)

p4_coef <- modelplot(
  list(
    "Mental Health" = model1,
    "Life Expectancy" = model2,
    "Well-Being Index" = model3
  ),
  coef_map = c(
    "log_income" = "Log Income",
    "log_income:Appalachian" = "Income × Appalachia"
  )
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Effect of Income on Well-Being Across Models",
    subtitle = "Points show estimates, lines show confidence intervals",
    x = "Coefficient Value",
    y = ""
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Show
print(p4_coef)

# Save
ggsave(
  "Output/figures/coefficient_plot.png",
  p4_coef,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)
 # ==========> not helpful



# ---------------------------------------------------------
# FIGURE: US Map (Well-Being Index)
# ---------------------------------------------------------
library(usmap)
library(dplyr)
library(ggplot2)
library(sf)

# County shapes
counties_map <- us_map(regions = "counties")

# Your latest-year data
map_df <- df %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  mutate(fips = sprintf("%05d", county_fips))

# Merge map + data
map_merged <- counties_map %>%
  left_join(map_df, by = "fips")

# Plot
p_map <- ggplot(map_merged) +
  # Base counties filled by well-being
  geom_sf(aes(fill = wellbeing_index), color = "white", linewidth = 0.05) +
  
  # Outline Appalachian counties
  geom_sf(
    data = map_merged %>% filter(Appalachian == 1),
    fill = NA,
    color = "black",
    linewidth = 0.25
  ) +
  
  scale_fill_viridis_c(
    option = "magma",
    name = "Well-Being",
    na.value = "grey90"
  ) +
  labs(
    title = paste0("Well-Being Across U.S. Counties (", max(df$year), ")")
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "right"
  )

print(p_map)

ggsave(
  "Output/figures/map_wellbeing_appalachia_outline.png",
  p_map,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)


# ---------------------------------------------------------
# FIGURE: Scatter Plot (Income vs Well-Being)
# ---------------------------------------------------------

p_scatter_facet <- ggplot(df, aes(x = log_income, y = wellbeing_index, color = Appalachia_Label)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", linewidth = 1) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  facet_wrap(~Appalachia_Label) +
  labs(
    title = "Income and Well-Being by Region",
    x = "Log Income",
    y = "Well-Being Index",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_scatter_facet)



# 1. Calculate the slopes dynamically for each group
# We use a simple linear model for each subset of the data
slope_app <- lm(wellbeing_index ~ log_income, 
                data = df %>% filter(Appalachia_Label == "Appalachian"))$coefficients["log_income"]

slope_nonapp <- lm(wellbeing_index ~ log_income, 
                   data = df %>% filter(Appalachia_Label == "Non-Appalachian"))$coefficients["log_income"]

# 2. Create the label data frame
# We place the text in the top-left corner of each facet (adjust x and y as needed)
label_df <- data.frame(
  Appalachia_Label = c("Appalachian", "Non-Appalachian"),
  x = c(min(df$log_income, na.rm = TRUE), min(df$log_income, na.rm = TRUE)),
  y = c(max(df$wellbeing_index, na.rm = TRUE), max(df$wellbeing_index, na.rm = TRUE)),
  label = c(
    paste0("Slope = ", round(slope_app, 3)),
    paste0("Slope = ", round(slope_nonapp, 3))
  )
)

# 3. Create the finalized plot
p_scatter_final <- ggplot(df, aes(x = log_income, y = wellbeing_index, color = Appalachia_Label)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", linewidth = 1, se = TRUE) +
  # Adding the slopes
  geom_text(
    data = label_df,
    aes(x = x, y = y, label = label),
    color = "black",       
    hjust = -0.1,          
    vjust = 1.5,           # Shift slightly down from the top
    size = 4.5,            
    fontface = "bold",     # Make it pop
    inherit.aes = FALSE    
  ) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  facet_wrap(~Appalachia_Label) +
  labs(
    title = "Income and Well-Being by Region",
    x = "Log Income",
    y = "Well-Being Index",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold", size = 14), 
    legend.position = "none"
  )

print(p_scatter_final)



# Save
ggsave(
  filename = "Output/figures/scatter_income_wellbeing_slopes.png",
  plot = p_scatter_final,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)



# ==================================================
# average poor mental health days over time
# average poor physical health days over time
# ==================================================

trend_df <- df %>%
  group_by(year, Appalachia_Label) %>%
  summarize(
    mental_health = mean(poor_mental_health_days, na.rm = TRUE),
    physical_health = mean(poor_physical_health_days, na.rm = TRUE),
    .groups = "drop"
  )

# mental health over time

p_trend_mental <- ggplot(trend_df, aes(x = year, y = mental_health, color = Appalachia_Label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  scale_x_continuous(
    breaks = 2011:2023   # force all years
  ) +
  labs(
    title = "Poor Mental Health Days Over Time",
    x = "Year",
    y = "Average Poor Mental Health Days",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_trend_mental)



# Physical health over time

p_trend_physical <- ggplot(trend_df, aes(x = year, y = physical_health, color = Appalachia_Label)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  scale_x_continuous(
    breaks = 2011:2023   # show all years
  ) +
  labs(
    title = "Poor Physical Health Days Over Time",
    x = "Year",
    y = "Average Poor Physical Health Days",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_trend_physical)



ggsave("Output/figures/trend_mental_health.png", p_trend_mental,
       width = 10, height = 5, dpi = 300, bg = "white")

ggsave("Output/figures/trend_physical_health.png", p_trend_physical,
       width = 10, height = 5, dpi = 300, bg = "white")



# Poverty rates vs well-being ==============================

p_poverty_facet <- ggplot(df, aes(x = poverty_rate, y = wellbeing_index, color = Appalachia_Label)) +
  geom_point(alpha = 0.03) +
  geom_smooth(method = "lm", linewidth = 1) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  facet_wrap(~Appalachia_Label) +
  labs(
    title = "Poverty and Well-Being by Region",
    x = "Poverty Rate",
    y = "Well-Being Index"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_poverty_facet)


# Save
ggsave("Output/figures/poverty_wellbeing_facet.png",
       width = 10, height = 5, dpi = 300, bg = "white")




# ==================================================
# RAW INCOME PER CAPITA OVER TIME
# Appalachian vs Non-Appalachian
# ==================================================

income_trend_df <- df %>%
  group_by(year, Appalachia_Label) %>%
  summarize(
    mean_income = mean(income_per_capita, na.rm = TRUE),
    median_income = median(income_per_capita, na.rm = TRUE),
    sd_income = sd(income_per_capita, na.rm = TRUE),
    n = sum(!is.na(income_per_capita)),
    se_income = sd_income / sqrt(n),
    .groups = "drop"
  )

# Main graph using mean income
p_income_trend <- ggplot(
  income_trend_df,
  aes(x = year, y = mean_income, color = Appalachia_Label, group = Appalachia_Label)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  scale_x_continuous(breaks = 2011:2023) +
  labs(
    title = "Raw Income Per Capita Over Time by Region",
    subtitle = "Average county-level income per capita, 2011 to 2023",
    x = "Year",
    y = "Average Income Per Capita",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_income_trend)

ggsave(
  "Output/figures/raw_income_trend_by_region.png",
  p_income_trend,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)





# Median raw income trend
p_income_trend_median <- ggplot(
  income_trend_df,
  aes(x = year, y = median_income, color = Appalachia_Label, group = Appalachia_Label)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  scale_x_continuous(breaks = 2011:2023) +
  labs(
    title = "Median Raw Income Per Capita Over Time by Region",
    subtitle = "Median county-level income per capita, 2011 to 2023",
    x = "Year",
    y = "Median Income Per Capita",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_income_trend_median)

ggsave(
  "Output/figures/raw_income_median_trend_by_region.png",
  p_income_trend_median,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# Income gap: Non-Appalachian minus Appalachian
income_gap_df <- income_trend_df %>%
  select(year, Appalachia_Label, mean_income) %>%
  pivot_wider(names_from = Appalachia_Label, values_from = mean_income) %>%
  mutate(gap = `Non-Appalachian` - Appalachian)

p_income_gap <- ggplot(income_gap_df, aes(x = year, y = gap)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  scale_x_continuous(breaks = 2011:2023) +
  labs(
    title = "Income Gap Over Time",
    subtitle = "Difference in average income per capita: Non-Appalachian minus Appalachian",
    x = "Year",
    y = "Income Gap"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p_income_gap)

ggsave(
  "Output/figures/raw_income_gap_over_time.png",
  p_income_gap,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)



# ==================================================
# WELL-BEING INDEX OVER TIME
# Appalachian vs Non-Appalachian
# ==================================================

wb_trend_df <- df %>%
  group_by(year, Appalachia_Label) %>%
  summarize(
    mean_wb = mean(wellbeing_index, na.rm = TRUE),
    median_wb = median(wellbeing_index, na.rm = TRUE),
    sd_wb = sd(wellbeing_index, na.rm = TRUE),
    n = sum(!is.na(wellbeing_index)),
    se_wb = sd_wb / sqrt(n),
    .groups = "drop"
  )


p_wb_trend <- ggplot(
  wb_trend_df,
  aes(x = year, y = mean_wb, color = Appalachia_Label, group = Appalachia_Label)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("steelblue", "firebrick")) +
  scale_x_continuous(breaks = 2011:2023) +
  labs(
    title = "Well-Being Index Over Time by Region",
    subtitle = "Average county-level well-being index, 2011–2023",
    x = "Year",
    y = "Well-Being Index",
    color = "Region"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.background = element_rect(fill = "white", color = NA)
  )

print(p_wb_trend)

ggsave(
  "Output/figures/wellbeing_trend_by_region.png",
  p_wb_trend,
  width = 10,
  height = 5,
  dpi = 300,
  bg = "white"
)


wb_gap_df <- wb_trend_df %>%
  select(year, Appalachia_Label, mean_wb) %>%
  pivot_wider(names_from = Appalachia_Label, values_from = mean_wb) %>%
  mutate(gap = `Non-Appalachian` - Appalachian)

p_wb_gap <- ggplot(wb_gap_df, aes(x = year, y = gap)) +
  geom_line(linewidth = 1.2, color = "darkgreen") +
  geom_point(size = 2, color = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = 2011:2023) +
  labs(
    title = "Well-Being Gap Over Time",
    subtitle = "Difference in well-being index: Non-Appalachian minus Appalachian",
    x = "Year",
    y = "Well-Being Gap"
  ) +
  theme_minimal(base_size = 13)

print(p_wb_gap)

ggsave(
  "Output/figures/wellbeing_gap_over_time.png",
  p_wb_gap,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)










