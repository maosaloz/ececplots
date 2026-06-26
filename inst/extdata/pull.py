import json
import pandas as pd
import requests
from io import StringIO
import os

# Load the JSON file
json_path = os.path.join(os.path.dirname(__file__), "data_explorer.JSON")
with open(json_path, "r") as f:
    data_explorer = json.load(f)

# Collect all datasets (flatten across categories)
datasets = {}
for category, indicators in data_explorer.items():
    for name, info in indicators.items():
        datasets[name] = info["API"]

# Pull each dataset and write to separate sheets in data.xlsx
output_path = os.path.join(os.path.dirname(__file__), "data.xlsx")

with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
    for name, url in datasets.items():
        print(f"Pulling: {name}")
        response = requests.get(url)
        response.raise_for_status()
        df = pd.read_csv(StringIO(response.text))

        # Keep only relevant columns
        df = df[["Reference area", "TIME_PERIOD", "OBS_VALUE"]]

        # Keep only the latest TIME_PERIOD per Reference area
        df = df.loc[df.groupby("Reference area")["TIME_PERIOD"].idxmax()]

        # Excel sheet names are limited to 31 characters
        sheet_name = name[:31]
        df.to_excel(writer, sheet_name=sheet_name, index=False)
        print(f"  -> Written to sheet: '{sheet_name}' ({len(df)} rows)")

print(f"\nDone. File saved to: {output_path}")
