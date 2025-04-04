def get_total_observations(url, payload):
    try:
        response = session.post(url, headers=headers, data=json.dumps(payload))
        response.raise_for_status()  # Raise an HTTPError for bad responses (4xx and 5xx)
        data = response.json()
        
        # Extract total records from metadata
        total_records = data.get("page_metadata", {}).get("total", 0)
        return total_records
    except requests.exceptions.RequestException as e:
        print(f"❌ Failed to fetch metadata: {e}")
        return 0