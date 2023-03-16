import pandas as pd
import matplotlib.pyplot as plt
import mysql.connector
from sklearn import linear_model


#Connect to database
db = mysql.connector.connect(
    host='34.138.188.252',
    user='root',
    passwd='uconnstamford',
    database='DATA'
    )

mycursor = db.cursor()

#Read all rows of Stock table
mycursor.execute('SELECT ticker, Month, Year, price, industryAdjustedScore, weightedAvgScore, environmentalPillarScore, goverancePillarScore, socialPillarScore FROM Stock')

#create dictionary of data for dataframe instantiation later on in the code
data = {'ticker': [],
        'Month': [],
        'Year': [],
        'price': [],
        'industryAdjustedScore': [],
        'weightedAvgScore': [],
        'environmentalPillarScore': [],
        'goverancePillarScore': [],
        'socialPillarScore': []
        }

#iterate through every row and unpack values
#add unpacked values to dictionary
for row in mycursor:
    ticker, Month, Year, price, industryAdjustedScore, weightedAvgScore, environmentalPillarScore, governancePillarScore, socialPillarScore = row

    data['ticker'].append(ticker)
    data['Month'].append(Month)
    data['Year'].append(Year)
    data['price'].append(price)
    data['industryAdjustedScore'].append(industryAdjustedScore)
    data['weightedAvgScore'].append(weightedAvgScore)
    data['environmentalPillarScore'].append(environmentalPillarScore)
    data['goverancePillarScore'].append(governancePillarScore)
    data['socialPillarScore'].append(socialPillarScore)

    
#Store database table in Pandas dataframe
df = pd.DataFrame(data)

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)


#output correlation coefficients to text_file
output_ = r'correlation.txt'
output = open(output_, 'w')
output.write(str(df.groupby('ticker').corr()))
output.close()




########################################################

###correlation between each individual factor and price                                                                                                         
#factors = ['industryAdjustedScore', 'environmentalPillarScore', 'goverancePillarScore', 'socialPillarScore']
#model = linear_model.LinearRegression()

#factor = 0

##graph each relationship using matplotlib
#while factor < len(factors):
#    curr = ['Month', 'Year', 'price', 'ticker', 'weightedAvgScore'] + factors[factor + 1:] + factors[:factor]

#    X = df.drop(curr, axis=1)
#    Y = df['price']

#    model.fit(X, Y)


#    print('R squared: ', model.score(X, Y))

#    plt.scatter(X, Y, color = "red", s = 15)
  
#    plt.xlabel(xlabel="{}".format(factors[factor]))
#    plt.ylabel(ylabel="Monthly Average Price (in dollars)")
#    plt.title("Correlation between {} and Price".format(factors[factor]))
#    plt.show()

#    factor += 1


# works cited: https://realpython.com/linear-regression-in-python/#regression
# works cited: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html

