import requests  # Import the requests module
import pandas as pd  # Import pandas for DataFrame operations
import time  # Import time for sleep
import os  # For file path handling

# Initialize session and URL
session = requests.Session()
url = "https://api.usaspending.gov/api/v2/search/spending_by_award/"

page = 1  # Initialize the starting page
max_pages = 2  # Limit the test to 2 pages
all_results = []  # Initialize a list to store all results

while page <= max_pages:
    payload = {
        "filters": {
            "award_type_codes": ["A", "B", "C", "D"],  # Standard contract awards
            "time_period": [
                {
                    "start_date": "2017-01-01",  # Start of FY17
                    "end_date": "2025-12-31"    # End of FY25
                }
            ],
            "award_amounts": [
                {
                    "upper_bound": 1000000  # Awards under $1M
                }
            ],
            "set_aside_type_codes": ["SBA", "8A", "WOSB", "HUBZone", "SDVOSB", "VO"]  # Small business filters
        },
        "fields": [
            "award_id",
            "recipient_name",
            "recipient_uei",
            "recipient_duns",
            "recipient_state_code",
            "award_amount",
            "award_date",
            "period_of_performance_start_date",
            "period_of_performance_current_end_date",
            "place_of_performance_city",
            "place_of_performance_state_code",
            "place_of_performance_zip5",
            "place_of_performance_country_code",
            "awarding_agency_name",
            "funding_agency_name",
            "naics_code",
            "naics_description",
            "product_or_service_code",
            "product_or_service_description",
            "contract_award_type"
        ],
        "limit": 100,
        "page": page,
        "sort": "award_amount",  # Use the correct field name for sorting
        "order": "desc"
    }

    try:
        response = session.post(url, json=payload)
        response.raise_for_status()  # Raises an error for non-200 responses
    except requests.exceptions.RequestException as e:
        print(f"Error on page {page}: {e}")
        if e.response is not None:
            print(f"Response content: {e.response.text}")
        break

    data = response.json()
    results = data.get("results", [])
    if not results:
        print("❌ No more results returned.")
        break

    all_results.extend(results)
    print(f"✅ Pulled page {page} with {len(results)} records.")
    page += 1
    time.sleep(0.5)  # Be kind to the server

# Convert the results to a DataFrame and save as CSV
df = pd.DataFrame(all_results)
output_path = "data/raw/contract_awards_fy17_25_small_businesses.csv"
df.to_csv(output_path, index=False)

print(f"🎉 Done! Saved test data to: {output_path}")