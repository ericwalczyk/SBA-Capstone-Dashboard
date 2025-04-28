# acs_data_cleaning.R
# Purpose: Clean and merge ACS 2017-2023 data for dashboard analysis

# Load libraries
library(tidyverse)

## Step 1: Load  ACS files from 2017 to 2023
acs_years <- 2017:2023
acs_list <- list()

for (year in acs_years) {
  file_path <- paste0("data/raw/ac_econ/acs", year, ".csv")
  temp <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(year = year)  # Add year column
  acs_list[[as.character(year)]] <- temp
}

## Step 2: Stack all years into one big table
acs_full <- bind_rows(acs_list)

## Step 3: Basic cleaning
acs_clean <- acs_full %>%
  slice(-1) %>%  # Remove first row containing metadata labels
  rename_with(~ str_to_lower(gsub(" ", "_", .x)), everything()) %>%
  separate(name, into = c("county_name", "state"), sep = ", ", extra = "merge", fill = "right") %>%
  rename(
    geo_id = geo_id,
    total_population = dp03_0001e,
    median_household_income = dp03_0062e,
    poverty_rate = dp03_0119pe,
    civilian_labor_force = dp03_0005e,
    mean_household_income = dp03_0063e,
    per_capita_income = dp03_0066e,
    employment_rate = dp03_0033e,
    unemployment_rate = dp03_0034pe
  ) %>%
  mutate(
    geo_fips = str_sub(geo_id, -5, -1),
    across(c(total_population, median_household_income, poverty_rate,
             civilian_labor_force, mean_household_income,
             per_capita_income, employment_rate, unemployment_rate),
           ~ suppressWarnings(as.numeric(.x)))
  )

## Step 4: Select core fields for dashboard analysis
acs_summary <- acs_clean %>%
  select(
    geo_fips, state, county_name, year,
    total_population, median_household_income, mean_household_income, per_capita_income,
    poverty_rate, employment_rate, unemployment_rate
  )

## Step 5: Save processed data to RDS
write_rds(acs_summary, "data/acs_summary.rds")
