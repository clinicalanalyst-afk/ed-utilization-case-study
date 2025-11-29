###############################################################
# Emergency Department Utilization Analysis (2016–2022)
# Author: Mutsumi Cornett
# Description: Full reproducible R workflow for importing,
# cleaning, analyzing, and visualizing national ED visit data.
###############################################################

# ============================
# 1. Load Packages
# ============================
library(tidyverse)
library(janitor)
library(scales)

# ============================
# 2. Import Dataset
# ============================
# Replace filename with your CSV name if different
df <- read_csv("cdc_ed_visits_2016_2022.csv") %>%
  clean_names()

# Inspect structure
names(df)
str(df)

# Check for missing values
colSums(is.na(df))

# Note: Zero values in ranking variables are meaningful (not NA)

# ============================
# 3. Diagnosis Category Analysis
# ============================

# Remove non-specific categories
df_specific <- df %>%
  filter(!measure %in% c("All diagnoses", "All reasons"))

# Aggregate ED visits by diagnosis category
df_diagnosis <- df_specific %>%
  group_by(measure) %>%
  summarise(total_visits = sum(estimate, na.rm = TRUE)) %>%
  ungroup()

# Convert to millions
df_diagnosis <- df_diagnosis %>%
  mutate(total_visits_millions = total_visits / 1e6)

# Diagnosis bar chart (horizontal)
ggplot(df_diagnosis,
       aes(x = reorder(measure, total_visits_millions),
           y = total_visits_millions)) +
  geom_col(fill = "#4A90E2") +
  coord_flip() +
  labs(
    title = "Most Common Emergency Department Diagnoses (2016–2022)",
    x = "Diagnosis Category",
    y = "ED Visits (Millions)"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)
  )

# Save chart
ggsave(
  filename = "ed_diagnoses_bar_chart.png",
  plot = last_plot(),
  width = 10,
  height = 7,
  dpi = 300
)

# ============================
# 4. Age Group Analysis
# ============================

# Filter for age-specific data
df_age <- df %>%
  filter(group == "By age")

# Summarize visits by age group
age_summary <- df_age %>%
  group_by(subgroup) %>%
  summarise(total_visits = sum(estimate, na.rm = TRUE))

# Age group bar chart
ggplot(age_summary, aes(x = subgroup, y = total_visits)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Total Emergency Department Visits by Age Group",
    x = "Age Group",
    y = "Total ED Visits (Millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(face = "bold")
  )

# Save chart
ggsave("ed_visits_by_age_group.png", width = 10, height = 6, dpi = 300)

# ============================
# 5. Yearly Trend Analysis
# ============================

# Aggregate by year
df_year <- df %>%
  group_by(year) %>%
  summarise(total_visits = sum(estimate, na.rm = TRUE))

# Convert to billions
df_year <- df_year %>%
  mutate(total_visits_billion = total_visits / 1e9)

# Line chart
ggplot(df_year, aes(x = year, y = total_visits_billion)) +
  geom_line(linewidth = 1.2, color = "#4A90E2") +
  geom_point(size = 2, color = "#4A90E2") +
  labs(
    title = "Yearly Emergency Department Visits (2016–2022)",
    x = "Year",
    y = "ED Visits (Billions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold")
  )

# Save yearly trend chart
ggsave("ed_visits_over_time.png", width = 10, height = 6, dpi = 300)

###############################################################
# End of Script
# This code produces all tables and visualizations included
# in the Emergency Department Utilization case study.
###############################################################
