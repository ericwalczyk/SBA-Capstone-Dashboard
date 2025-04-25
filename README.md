SBA Capstone Dashboard

An interactive R Shiny dashboard for the Small Business Administration (SBA) Capstone Project, exploring how federal contracting dollars are distributed geographically and how they relate to local economic indicators like GDP and business formation.

This dashboard allows users to:
	•	Explore the geographic distribution of federal contract obligations at the county and state level
	•	Compare contract spending with county GDP and new business applications
	•	Examine correlations across different contract sizes and industries
	•	Provide insights into local economic impacts of federal contracting

The dashboard is modular and scalable — new components from team members (e.g., industry drilldowns, demographic filters) will be integrated progressively.


###################################################################################################
####################################### FOLDER STRUCTURE ##########################################
###################################################################################################

SBA-Capstone-Dashboard/
├── app.R                  # Main launcher script (sources modules)
├── dashboard_MAIN.R       # (Optional) Working version of full dashboard
│
├── deploy/                # Preprocessed datasets for fast loading
│   ├── allfedcon.rds
│   ├── small_contracts.rds
│   ├── county_gdp.rds
│   ├── bfs_apps.rds
│   └── state_data.rds
│
├── modules/               # All modular Shiny app components
│   ├── map_module.R
│   ├── correlation_module.R
│   └── (future modules here)
│
├── ui/                    # UI components (sidebar, themes)
│   └── sidebar.R
│
├── www/                   # Static assets (CSS, logos, JS)
│   └── style.css
│
└── README.md              # (This file)



###################################################################################################
####################################### APP COMPONENTS ############################################
###################################################################################################

Map Module (map_module.r)

Correlation Explorer (correlation_module.R)

Sidebar Filters (sidebar.R)

Coming Soon - industry view, demographic overlays, and more


###################################################################################################
####################################### Data Sources ##############################################
###################################################################################################

NOTE: All of these are going to be reuploaded as .RDS files and saved under the /deploy folder. All code will need to be updated to reflect correct pathing. 


all_fedcon.csv: All federal contract obligations (county-level, FY2017-2023)
small_contracts.csv: Subset of contracts â‰¤ $250,000
county_gdp.csv: County-level GDP data from BEA
state_gdp.csv: State-level GDP data from BEA
bfs_county_apps_annual.csv: New business applications from the U.S. Census Bureau
laus_unemployment_by_county.csv: LAUS unemployment survey data
cbp_2017_2022.csv: County business pattern data from U.S. Census Bureau

