# --- 0) Load Libraries ---
library(tidyverse)
library(plm)
library(broom)
library(dotwhisker)

# --- 1) Summarize Small Contracts by County-Year ---

contract_summary <- fedcon %>%
  group_by(county_fips, fiscal_year) %>%
  summarise(
    total_obligation = sum(total_obligation, na.rm = TRUE),
    n_contracts = n(),
    .groups = "drop"
  )

# --- 2) Merge Contract Data into Economic Data ---

econ_panel <- econ_data %>%
  left_join(contract_summary, by = c("county_fips" = "county_fips", "year" = "fiscal_year")) %>%
  mutate(
    total_obligation = ifelse(is.na(total_obligation), 0, total_obligation),
    n_contracts = ifelse(is.na(n_contracts), 0, n_contracts)
  )

# --- 3) Collapse Duplicates and Calculate GDP Growth ---

econ_panel_collapsed <- econ_panel %>%
  group_by(county_fips, year) %>%
  summarise(
    total_population = first(total_population),
    median_household_income = first(median_household_income),
    mean_household_income = first(mean_household_income),
    per_capita_income = first(per_capita_income),
    poverty_rate = first(poverty_rate),
    employment_rate = first(employment_rate),
    unemployment_rate = first(unemployment_rate),
    total = first(total),
    total_obligation = sum(total_obligation, na.rm = TRUE),
    n_contracts = sum(n_contracts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(county_fips, year) %>%
  group_by(county_fips) %>%
  mutate(
    gdp_growth = (total / lag(total)) - 1
  ) %>%
  ungroup()

# --- 4) Create Lagged Contract Variables ---

econ_panel_collapsed <- econ_panel_collapsed %>%
  group_by(county_fips) %>%
  arrange(year) %>%
  mutate(
    lagged_obligation = lag(total_obligation, 1)
  ) %>%
  ungroup()

# --- 5) Identify "Active" Contracting Counties ---

counties_with_contracts <- econ_panel_collapsed %>%
  group_by(county_fips) %>%
  summarise(total_obligation_sum = sum(total_obligation, na.rm = TRUE)) %>%
  filter(total_obligation_sum > 0) %>%
  pull(county_fips)

econ_panel_active <- econ_panel_collapsed %>%
  filter(county_fips %in% counties_with_contracts)

# --- 6) Declare Panel Structure ---

panel_data_active <- pdata.frame(econ_panel_active, index = c("county_fips", "year"))

# --- 7) Estimate Fixed Effects Models (Contracts Only Counties) ---

model_current_active <- plm(
  gdp_growth ~ total_obligation,
  data = panel_data_active,
  model = "within"
)

model_lagged_active <- plm(
  gdp_growth ~ lagged_obligation,
  data = panel_data_active,
  model = "within"
)

# --- 8) Summarize Results ---

summary(model_current_active)
summary(model_lagged_active)

# --- 9) Quick Visual: Coefficient Plot ---

dwplot(list(model_current_active, model_lagged_active)) +
  theme_minimal() +
  labs(title = "Impact of SBA Small Contracts on GDP Growth (Active Counties Only)",
       x = "Coefficient Estimate (95% CI)",
       y = "")