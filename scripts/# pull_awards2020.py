from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry
import requests
import json
import csv
import time

# === USAspending API endpoint and headers ===
url_geography = "https://api.usaspending.gov/api/v2/search/spending_by_geography/"
headers = {
    "Content-Type": "application/json"
}

# === Create a session with retry logic ===
session = requests.Session()
retries = Retry(total=5, backoff_factor=1, status_forcelist=[500, 502, 503, 504])
adapter = HTTPAdapter(max_retries=retries)
session.mount("https://", adapter)
session.mount("http://", adapter)

# === Payload for Spending by Geography (county and state level) ===
payload_geography = {
    "filters": {
        "time_period": [
            {
                "start_date": "2017-01-01",
                "end_date": "2023-12-31"  # Adjust to the most recent census year
            }
        ]
    },
    "scope": "recipient_location",
    "geo_layer": "county",  # Change to "state" for state-level data
    "fields": [
        "Geographic Area",
        "Obligation Amount",
        "County Code",
        "State Code"
    ],
    "page": 1,
    "limit": 50,  # Fetch 50 results per page
    "sort": "Obligation Amount",
    "order": "desc"
}

# === Function to fetch data for multiple pages ===
def fetch_data(url, payload, num_pages):
    all_data = []
    for page in range(1, num_pages + 1):  # Fetch multiple pages
        payload["page"] = page
        try:
            response = session.post(url, headers=headers, data=json.dumps(payload))
            response.raise_for_status()  # Raise an HTTPError for bad responses (4xx and 5xx)
            data = response.json()
            
            # Get results
            results = data.get('results', [])
            print(f"Page {page}: {len(results)} results fetched.")  
            all_data.extend(results)
        except requests.exceptions.RequestException as e:
            print(f"❌ Request failed on page {page}: {e}")
            break
        time.sleep(1)  # Add a 1-second delay between requests
    return all_data

# === Fetch data for counties (2 pages for testing) ===
all_geography_data = fetch_data(url_geography, payload_geography, 2)

# === Check if data was fetched successfully ===
if not all_geography_data:
    print("❌ No data returned from the Spending by Geography API.")
    exit()

# === Save the data to CSV ===
output_path = "/Volumes/vorb/02_school/capstone/SBA-Capstone-Dashboard/data/raw/spending_by_county.csv"
with open(output_path, mode='w', newline='') as file:
    writer = csv.DictWriter(file, fieldnames=all_geography_data[0].keys())
    writer.writeheader()
    for item in all_geography_data:
        writer.writerow(item)

print(f"✅ Data saved to {output_path}")