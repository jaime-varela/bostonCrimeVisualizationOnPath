

from re import X
import pandas as pd
import numpy as np

import os
import pandas as pd

# List to store DataFrames
dataframes = []

# Get all CSV files in the current directory
csv_files = [file for file in os.listdir('.') if file.endswith('.csv')]

# Read and append each CSV file to the list
for csv_file in csv_files:
    try:
        df = pd.read_csv(csv_file)
        dataframes.append(df)
        print(f"Successfully read {csv_file}")
    except Exception as e:
        print(f"Error reading {csv_file}: {e}")

# Concatenate all DataFrames
if dataframes:
    allData = pd.concat(dataframes, ignore_index=True)
    print("All CSV files have been concatenated into a single DataFrame.")
else:
    print("No CSV files found in the current directory or all failed to load.")
    exit(1)

errorCodesDf = pd.read_csv('rmsoffensecodes.csv',dtype={'CODE': 'str'})
errorCodesDic = {}
errorCodesDf = errorCodesDf.reset_index()  # make sure indexes pair with number of rows

for index, row in errorCodesDf.iterrows():
    code = row['CODE']
    name = row['NAME']
    errorCodesDic[code] = name

print("total crimes " + str(len(allData.index)))

allData['OFFENSE_CODE'] = allData['OFFENSE_CODE'].apply(lambda x: errorCodesDic[x] if x in errorCodesDic.keys() else "No code available")


allData.to_csv('crimesBoston.csv',index=False)

print(len(allData[allData['OFFENSE_CODE'] == "No code available"].index))