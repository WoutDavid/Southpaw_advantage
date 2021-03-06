import requests
import pandas as pd

dataframeDict = {}
versusList = ["vr","vl"]
# battingList = ["l", "r"]
for versus in versusList:
    dataframes = []
    for i in range(1,7):
        url = 'https://www.mlb.com/stats/2019?split=' + versus + '&page=' + str(i)
        html = requests.get(url).content
        df_list = pd.read_html(html)
        df = df_list[-1]
        dataframes.append(df)
    dataframeDict['versus_{}'.format(versus)] = pd.concat(dataframes)
for k, v in dataframeDict.items():
    v.to_csv('{}.csv'.format(str(k)))




#importing necessary packages
import pandas as pd 
import os


##read in data
dfVersusLeft = pd.read_csv("versus_vl.csv")
dfVersusRight = pd.read_csv("versus_vr.csv")
dfVersusList = [dfVersusLeft, dfVersusRight]


##reading and shifting the columns a bit
#creating new columns for later
for df in dfVersusList:
    df.drop("Unnamed: 0", inplace=True, axis=1)
    df.rename(columns={"caret-upcaret-downOPS": "OPS"}, inplace=True)
    df["FIRSTNAME"] = df["PLAYER"]
    df["LASTNAME"] = df["PLAYER"]



##Parsing the name column since it is ineligable and I need last name identifiers to merge with another 
##database in R
def extractLastName(string):
    temp = string[:len(string) // 2]
    return temp

def extractFirstName(string):
    temp = string[:-1]
    return temp

for df in dfVersusList:
    for i,player in enumerate(df["PLAYER"]):
        if i <10:
            new_player = player[1:len(player)-6]
        elif i<100:
            new_player = player[2:len(player)-7]
        else:
            new_player = player[3:len(player)-8]
        splitName = new_player.split(" ")
        df["FIRSTNAME"].replace({player: extractFirstName(splitName[0])},inplace=True)
        df["LASTNAME"].replace({player: extractLastName(splitName[1])},inplace=True)
        df["PLAYER"].replace({player: new_player}, inplace=True)


dfVersusList[0].to_csv("versusLeft.csv")
dfVersusList[1].to_csv("versusRight.csv")

