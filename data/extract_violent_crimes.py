# extracts violent crimes from the database 
import pandas as pd
from datetime import datetime

def joinDataFrames(dataFrameArray):
    areEqual = all([len(dataFrameArray[0].columns.intersection(df.columns)) 
      == dataFrameArray[0].shape[1] for df in dataFrameArray])
    assert areEqual, "can not join data frames with different columns"
    result = pd.concat(dataFrameArray)
    result = result.drop_duplicates()
    return result

def selectDataFramesByKeyWord(dataFrame ,keyWord, columsToQuery=['Group','OFFENSE_DESCRIPTION','OFFENSE_CODE_NAME'],lowerStrings=True):
    # df = df[df['Credit-Rating'].str.contains('Fair')]
    dataFrameArray = []
    if lowerStrings:
        for columnName in columsToQuery:
            dataWithValues = dataFrame[dataFrame[columnName].notna()]
            print(keyWord,columnName)
            dataWithValues[columnName] = dataWithValues[columnName].apply(str.lower)
            queryWord = keyWord.lower()
            filteredDF = dataWithValues[dataWithValues[columnName].str.contains(queryWord)]
            dataFrameArray.append(filteredDF)
    else:
        for columnName in columsToQuery:
            dataWithValues = dataFrame[dataFrame[columnName].notna()]
            dataWithValues[columnName] = dataWithValues[columnName].apply(str.lower)
            queryWord = keyWord
            filteredDF = dataWithValues[dataWithValues[columnName].str.contains(queryWord)]
            dataFrameArray.append(filteredDF)
    return joinDataFrames(dataFrameArray)


if __name__ == "__main__":
    date_parser = lambda x: pd.to_datetime(x, format="%Y-%m-%d %H:%M:%S", errors="coerce")

    data = pd.read_csv('crimesBoston.csv', dtype={'INCIDENT_NUMBER': 'str'})


    data.drop(['INCIDENT_NUMBER', 'Location'], axis=1, inplace=True)


    rename = {'OFFENSE_CODE_GROUP': 'Group',
              'DISTRICT': 'District',
              'REPORTING_AREA': 'Area',
              'SHOOTING': 'If_shooting',
              'YEAR': 'Year',
              'MONTH': 'YMonth',
              'DAY_OF_WEEK': 'WDay',
              'HOUR': 'DHour',
              'UCR_PART': 'UCR_part',
              'STREET': 'Street'}

    data.rename(index=str, columns=rename, inplace=True)

    violentCrimeKeyWords = ['Aggravated assault','Homicide', 'HOME INVASION', 
    'Manslaughter', 'HUMAN TRAFFICKING', 'BODILY HARM', 'MURDER ', 'BATTERY', 
    'Kidnapping', 'ROBBERY ATTEMPT - OTHER WEAPON', 'ROBBERY - STREET', 'ROBBERY - HOME INVASION', 
    'ROBBERY - CAR JACKING', 'ROBBERY - KNIFE']

    homicideKeyWords = ['Homicide','Manslaughter','MURDER ']

    dataWithLocations = data[data['Lat'].notna() & data['Long'].notna()]
    violentCrimesDFs = [selectDataFramesByKeyWord(dataWithLocations,keyWordVal) for keyWordVal in violentCrimeKeyWords]
    homicideCrimesDFs =[selectDataFramesByKeyWord(dataWithLocations,keyWordVal) for keyWordVal in homicideKeyWords]
    violentCrimeDF = joinDataFrames(violentCrimesDFs)
    homicideCrimeDF = joinDataFrames(homicideCrimesDFs)
    print(len(violentCrimeDF.index) , len(homicideCrimeDF.index))

    violentCrimeDF.to_csv('violentCrimes.csv',index=False)
    homicideCrimeDF.to_csv('homicides.csv', index=False)

