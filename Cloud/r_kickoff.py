import mysql.connector
import pandas as pd
import subprocess
#from rpy2.robjects import pandas2ri

#Connect to database                                                                                                                                           
db = mysql.connector.connect(
    host='34.138.188.252',
    user='root',
    passwd='uconnstamford',
    database='DATA'
    )

mycursor = db.cursor()

#Read all rows of MSCI_R table  
mycursor.execute('SELECT isinID, ticker, Month, Year, price, environmentalPillarScore, goverancePillarScore, socialPillarScore FROM MSCI_R')

#create dictionary of data for dataframe instantiation later on in the code                                                                                    
data = {'isinID': [],
        'ticker': [],
        'Month': [],
        'Year': [],
        'price': [],
        'environmentalPillarScore': [],
        'goverancePillarScore': [],
        'socialPillarScore': []
        }

#iterate through every row and unpack values                                                                                                                   
#add unpacked values to dictionary                                                                                                                             
for row in mycursor:
    isinID, ticker, Month, Year, price, environmentalPillarScore, governancePillarScore, socialPillarScore = row

    data['isinID'].append(isinID)
    data['ticker'].append(ticker)
    data['Month'].append(Month)
    data['Year'].append(Year)
    data['price'].append(price)
    data['environmentalPillarScore'].append(environmentalPillarScore)
    data['goverancePillarScore'].append(governancePillarScore)
    data['socialPillarScore'].append(socialPillarScore)


#Store database table in Pandas dataframe              
df = pd.DataFrame(data)

#Store pandas dataframe in csv so that R can access and process it                                             
df.to_csv('MSCI_Price_Data.csv', index=False)


##Get Price Data
mycursor.execute('SELECT DATE_DIM_ID, BB_TICKER_CD, ISIN_CD, PBD_PRICE_AMT FROM PRICE_R')

#create dictionary of data for dataframe instantiation later on in the code
                                                                                                                
data = {'DATE_DIM_ID': [],
        'BB_TICKER_CD': [],
        'ISIN_CD': [],
        'PBD_PRICE_AMT': []}

for row in mycursor:
    DATE_DIM_ID, BB_TICKER_CD, ISIN_CD, PBD_PRICE_AMT = row

    data['DATE_DIM_ID'].append(DATE_DIM_ID)
    data['BB_TICKER_CD'].append(BB_TICKER_CD)
    data['ISIN_CD'].append(ISIN_CD)
    data['PBD_PRICE_AMT'].append(PBD_PRICE_AMT)

    
df = pd.DataFrame(data)

#Store pandas dataframe in csv so that R can access and process it
df.to_csv('PriceData_2012_2022.csv', index=False)

#Call R correlation model
subprocess.call(["Rscript", "correlation_model.R"])
