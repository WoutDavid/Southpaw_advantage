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


