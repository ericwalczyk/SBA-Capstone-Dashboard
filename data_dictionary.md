
# 📊 Federal Contracts Explorer — Data Dictionary

This document describes the structure and contents of the datasets used in the **Federal Contracts Explorer** Shiny dashboard. Each dataset is stored in `.rds` format and supports interactive visualizations and economic analysis of federal small business contracting activity at the state and county levels.

---

## 🧾 `fedcon.rds`
**Federal contract awards by county and year (filtered to small business awards)**

| Variable           | Description                                                        |
|--------------------|--------------------------------------------------------------------|
| `county_fips`      | 5-digit FIPS code identifying the county                           |
| `state`            | State abbreviation (e.g., "VA", "TX")                              |
| `total_obligation` | Total dollar amount of contract obligations                        |
| `parent_agency`    | Parent federal agency (e.g., "Department of Defense")              |
| `agency`           | Sub-agency responsible for the contract (e.g., "Army")             |
| `naics_code`       | 6-digit NAICS industry classification code                         |
| `naics_description`| Description of the NAICS code industry                             |
| `naics_group`      | High-level NAICS group (e.g., "Construction", "Healthcare")        |
| `sector`           | Economic sector classification                                     |
| `is_small_business`| Indicator for small business designation (TRUE/FALSE)              |
| `is_minority_owned`| Indicator if business is minority-owned                            |
| `is_woman_owned`   | Indicator if business is woman-owned                               |
| `is_veteran_owned` | Indicator if business is veteran-owned                             |
| `fiscal_year`      | Federal fiscal year of the contract                                |

---

## 🧵 `cpb.rds`
**County-level employment and establishment counts (summary version)**

| Variable       | Description                                 |
|----------------|---------------------------------------------|
| `county_fips`  | County FIPS code                            |
| `year`         | Year of data                                |
| `total_est`    | Total number of business establishments     |
| `total_emp`    | Total employment                            |
| `total_ap`     | Total annual payroll (likely in dollars)    |

---

## 🏢 `cpb_full.rds`
**Detailed County Business Patterns data by industry**

| Variable     | Description                                        |
|--------------|----------------------------------------------------|
| `fipstate`   | State FIPS code                                    |
| `fipscty`    | County FIPS code                                   |
| `naics`      | NAICS industry code                                |
| `empflag`    | Employment flag (confidentiality or estimate note) |
| `emp_nf`     | Nonflagged employment estimate                     |
| `emp`        | Total employment                                   |
| `qp1_nf`     | Nonflagged quarterly payroll                       |
| `qp1`        | Quarterly payroll (in dollars)                     |
| `ap_nf`      | Annual payroll nonflagged                          |
| `ap`         | Annual payroll                                     |
| `est`        | Number of establishments                           |
| `n<5` to `n1000_4` | Establishments by size bins (number of employees) |
| `censtate`   | Census state code                                  |
| `cencty`     | Census county code                                 |
| `county_fips`| County FIPS code                                   |
| `year`       | Year of record                                     |

---

## 💵 `state_gdp.rds`

| Variable         | Description                                    |
|------------------|------------------------------------------------|
| `geo_fips`       | State-level FIPS code                          |
| `geo_name`       | State name                                     |
| `description`    | Type of economic activity (e.g., total GDP)    |
| `year`           | Year of observation                            |
| `value`          | GDP value (in millions)                        |
| `state_abbr.x`   | State abbreviation (possibly redundant)        |
| `state_abbr.y`   | State abbreviation (possibly redundant)        |

---

## 💰 `county_gdp.rds`

| Variable        | Description                                         |
|-----------------|-----------------------------------------------------|
| `geo_fips`      | County-level FIPS code                              |
| `geo_name`      | County name                                         |
| `industry_code` | NAICS or BEA industry code                          |
| `description`   | Description of industry                             |
| `year`          | Year of GDP observation                             |
| `gdp_millions`  | GDP value (in millions of dollars)                  |
| `sector`        | Industry sector (e.g., manufacturing, services)     |
| `naics_group`   | High-level NAICS grouping                           |

---

## 🧮 `sbcs.rds`
**Survey data on small business conditions**

| Variable            | Description                                               |
|---------------------|-----------------------------------------------------------|
| `Responder.Type`    | Respondent category (e.g., sole prop., small corp.)       |
| `Year`              | Year of survey                                            |
| `Survey.Responder`  | Entity responding (possibly same as Responder.Type)       |
| `Survey.question`   | Survey question text                                      |
| `Response.option`   | Response option selected                                  |
| `Percent`           | Percent of respondents selecting the option               |
| `N`                 | Number of respondents                                     |

---

## 📊 `acs_summary.rds`
**American Community Survey summary indicators**

| Variable                   | Description                                     |
|----------------------------|-------------------------------------------------|
| `geo_fips`                 | County FIPS code                                |
| `state`                    | State abbreviation                              |
| `county_name`              | County name                                     |
| `year`                     | Year of data                                    |
| `total_population`         | Total population                                |
| `median_household_income`  | Median household income                         |
| `mean_household_income`    | Mean household income                           |
| `per_capita_income`        | Per capita income                               |
| `poverty_rate`             | Share of population below poverty line (%)      |
| `employment_rate`          | Employment-to-population ratio (%)              |
| `unemployment_rate`        | Unemployment rate (%)                           |

---

## 🗺️ `state_sf.rds`
**Spatial polygons for U.S. states (sf object)**

| Variable   | Description                                |
|------------|--------------------------------------------|
| `STATEFP`  | State FIPS code                            |
| `STATENS`  | ANSI code for state                        |
| `AFFGEOID` | Geographic identifier (Census)             |
| `GEOID`    | Combined state-level GEOID                 |
| `STUSPS`   | State abbreviation                         |
| `NAME`     | State name                                 |
| `LSAD`     | Legal/statistical area description         |
| `ALAND`    | Land area (m²)                             |
| `AWATER`   | Water area (m²)                            |
| `geometry` | sf geometry object                         |
| `state`    | Lowercase state name (added in preprocessing) |

---

## 🧭 `county_sf.rds`
**Spatial polygons for U.S. counties (sf object)**

| Variable     | Description                                |
|--------------|--------------------------------------------|
| `STATEFP`    | State FIPS code                            |
| `COUNTYFP`   | County FIPS code                           |
| `COUNTYNS`   | ANSI code for county                       |
| `AFFGEOID`   | Full geographic identifier (Census)        |
| `GEOID`      | Combined county-level GEOID                |
| `NAME`       | County name                                |
| `NAMELSAD`   | Legal/statistical area description         |
| `STUSPS`     | State abbreviation                         |
| `STATE_NAME` | Full state name                            |
| `LSAD`       | Legal/statistical area description         |
| `ALAND`      | Land area (m²)                             |
| `AWATER`     | Water area (m²)                            |
| `geometry`   | sf geometry object                         |

---

## 📉 `unemployment.rds`
**County-level unemployment data**

| Variable            | Description                      |
|---------------------|----------------------------------|
| `county_fips`       | 5-digit FIPS code                |
| `area_text`         | County or region name            |
| `year`              | Year of observation              |
| `unemployment_rate` | Annual average unemployment rate |
