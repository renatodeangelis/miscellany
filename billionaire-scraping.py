import requests
import json
import csv

url = "https://forbes-billionaires-api.p.rapidapi.com/list.php"

headers = {
	"x-rapidapi-key": "e069469160msh12fb48882a7e0c6p1635c0jsn2d490a4837ff",
	"x-rapidapi-host": "forbes-billionaires-api.p.rapidapi.com"
}

response = requests.get(url, headers=headers)

data = response.json()

# Save JSON data to a file
with open('billionaires.json', 'w') as json_file:
    json.dump(data, json_file, indent=4)

# Convert JSON to CSV
if isinstance(data, list) and data:
    with open('billionaires.csv', mode='w', newline='') as file:
        writer = csv.writer(file)
        
        # Write the header
        writer.writerow(data[0].keys())
        
        # Write the data rows
        for item in data:
            writer.writerow(item.values())
else:
    print("Unexpected data format or empty response:", data)