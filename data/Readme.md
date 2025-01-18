
## Data Merging Scripts

The file `datajoinscript.py` can be used to merge csv files. 

The data is [located here](https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system).  The year 2022 should be updated regularly.  For later years the script will have to be modified.

The python script creates `crimesBoston.csv` as its output from the csvs downloaded from the Boston crime data site.


## Violent Crime Extraction Scripts

The script `extract_violent_crimes.py` generates two csvs with violent crimes and homicides, a subset of violent crimes.


## Processing data for STPP analysis

Run the `process_boston_crime_data.py` after extracting the violent crimes. Use the notebook in this dir to generate a test/train/val set.