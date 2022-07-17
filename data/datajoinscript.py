

from re import X
import pandas as pd
import numpy as np

crime2015 = pd.read_csv('crime-incident-reports-2015.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2016 = pd.read_csv('crime-incident-reports-2016.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2017 = pd.read_csv('crime-incident-reports-2017.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2018 = pd.read_csv('crime-incident-reports-2018.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2019 = pd.read_csv('crime-incident-reports-2019.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2020 = pd.read_csv('crime-incident-reports-2020.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2021 = pd.read_csv('crime-incident-reports-2021.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})
crime2022 = pd.read_csv('crime-incident-reports-2022.csv',dtype={'INCIDENT_NUMBER': 'str','OFFENSE_CODE': 'str'})

allDfs = [
    crime2015,
    crime2016,
    crime2017,
    crime2018,
    crime2019,
    crime2020,
    crime2021,
    crime2022
]

areEqual = all([len(allDfs[0].columns.intersection(df.columns)) 
      == allDfs[0].shape[1] for df in allDfs])

print(areEqual)

allData = pd.concat(allDfs)

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