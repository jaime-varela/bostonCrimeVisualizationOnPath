
## Data Merging Scripts

The file `datajoinscript.py` can be used to merge csv files. 

The data is [located here](https://data.boston.gov/dataset/crime-incident-reports-august-2015-to-date-source-new-system).  The year 2022 should be updated regularly.  For later years the script will have to be modified.

The python script creates `crimesBoston.csv` as its output.

In order for this script to work one needs to download all the data from the above link and rename the files as `crime-incident-reports-20XX.csv`.  Also export the `rmsoffensecodes.xlsx` as `rmsoffensecodes.csv`.