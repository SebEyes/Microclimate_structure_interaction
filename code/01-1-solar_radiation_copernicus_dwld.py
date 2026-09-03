## Ensure that you installed the required package to gather data from CAMS
## Note that cdsapi will not work immediately after installation. It requires an access key to connect to the Copernicus Climate Data Store.
## More information: https://cds.climate.copernicus.eu/how-to-api

import cdsapi
import pandas as pd
# Load site locations from CSV
sites_df = pd.read_csv(r'data/QGIS_layers/sites_locations.csv', sep=';')

dataset = "cams-solar-radiation-timeseries"

client = cdsapi.Client()

for idx, row in sites_df.iterrows():
    location_id = row['locationID']
    longitude = row['X']
    latitude = row['Y']
    request = {
        "sky_type": "observed_cloud",
        "location": {"longitude": longitude, "latitude": latitude},
        "altitude": ["-999"],
        "date": ["2023-01-02/2025-09-02"],
        "time_step": "1hour",
        "time_reference": "universal_time",
        "data_format": "csv"
    }
    target = f"{location_id}.csv"
    client.retrieve(dataset, request, target)