# Federal Contracts Explorer

The **Federal Contracts Explorer** is an interactive R Shiny application designed to visualize and analyze the distribution and economic impact of federal small business contract obligations across U.S. states and counties. It integrates multiple public datasets, including USAspending.gov federal contracting data, ACS economic indicators, County Business Patterns, and more.

This app was developed as part of a data-driven capstone project to support policy analysis and decision-making related to small business federal contracting and local economic development.

---

## Features

- **Interactive Map Drilldown**  
  - Start with a national overview of federal contract obligations as a % of state GDP.  
  - Click on any state to zoom into county-level data.  
  - Reset to national view at any time.

- **Dynamic Value Boxes**  
  - Displays total obligations, GDP, and per capita contract obligations.  
  - Automatically updates with filters and state drilldowns.

- **Data Explorer Tabs**  
  - **Economic Impact Comparison:** Evaluate the relationship between receiving contracts and business outcomes (jobs, establishments, payroll) using descriptive and regression-based models.  
  - **Distribution Explorer:** View top-performing counties or states by agency or NAICS group.  
  - **National Trends:** Track GDP, income, and unemployment over time.  
  - **Obligations Over Time:** Visualize contracting trends across fiscal years.  
  - **Top Industries & Agencies:** See the top NAICS groups and agencies over time.  
  - **Small Business Community Survey (SBCS):** Explore business sentiment and responses by year, group, and question.  
  - **Business Applications:** Compare trends in new business applications and population by state.  
  - **Correlation Explorer:** Examine the relationship between contracting and GDP or business formation at the county level using raw values or percentiles.

---

## Directory Structure

```
data/
├── smallcon.rds              # Filtered federal contract data
├── state_gdp.rds             # State GDP by year
├── county_gdp.rds            # County GDP by year
├── acs_summary.rds           # ACS demographic and economic indicators
├── cbp_summary_clean.rds     # Summarized CBP data
├── cbp_2017_2022.rds         # Full CBP by NAICS data
├── business_apps.rds         # Business formation data from BFS
├── sbcs.rds                  # Small Business Community Survey data
├── state_sf.rds              # State shapefile (sf object)
├── county_sf.rds             # County shapefile (sf object)
```

---

## Dependencies

This app relies on the following R packages:

- `shiny`, `shinydashboard`, `shinyWidgets`, `shinycssloaders`
- `leaflet`, `sf`, `leaflet.extras`
- `plotly`, `ggplot2`, `scales`
- `dplyr`, `tidyverse`, `stringr`

To install all dependencies:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "shinycssloaders",
  "leaflet", "leaflet.extras", "sf", "plotly", "ggplot2", "scales",
  "dplyr", "tidyverse", "stringr"
))
```

---

## Running the App

1. Clone the repo or copy all required files into a project folder.
2. Ensure all `.rds` data files are stored in a `/data` subdirectory.
3. Launch the app from your R console:

```r
shiny::runApp("app.R")
```

---

## Data Sources

- **Federal Contract Data**: [USAspending.gov](https://www.usaspending.gov/)
- **GDP Data**: Bureau of Economic Analysis (BEA)
- **Business Formation**: U.S. Census Business Formation Statistics (BFS)
- **ACS Summary**: U.S. Census American Community Survey (ACS)
- **County Business Patterns**: U.S. Census CBP
- **SBCS**: Small Business Credit Survey (Federal Reserve)

---

## Notes

- The app is optimized for small business contracts (≤ $250k), but can be extended to include all contracts with minimal changes.
- Drilldown logic uses `STATEFP` to map click interactions to shapefiles.
- Reactive components are modular and can be expanded with minimal friction.
- Summary statistics and regression models are recalculated based on filtered views for precision and transparency.

---

## Authors & Credits

Developed by **Eric Walczyk**, **Emma Compbell**, **Maria Papera**,
**Shayna Vinlkoor**, and **Rugy Yusufu**
George Washington University  


With gratitude to all collaborators, professors, and classmates who provided data feedback and design suggestions.

---

##️ Future Improvements

- Add download buttons for filtered datasets
- Integrate additional contract metadata (e.g., purpose codes)
- Expand to Census Tract resolution with updated shapefiles
- Embed pre-computed correlation coefficients in tooltips


