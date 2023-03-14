import pandas as pd
import matplotlib.pyplot as plt
#from sklearn.model_selection import train_test_split
from sklearn import linear_model

#replace with name of any csv file
#Note: must be a csv file, not excel (convert excel file to csv)
df = pd.read_csv(r'merged_data.csv')

pd.set_option('display.max_columns', None)
pd.set_option('display.max_rows', None)

###correlation between each individual factor and price

factors = ['industryAdjustedScore', 'environmentalPillarScore', 'goverancePillarScore', 'socialPillarScore']
model = linear_model.LinearRegression()

factor = 0

#output correlations coefficients to text_file
output_ = r'correlation.txt'
output = open(output_, 'w')
output.write(str(df.groupby('ticker').corr()))
output.close()


#graph each relationship using matplotlib
while factor < len(factors):
    curr = ['Month', 'Year', 'price', 'ticker', 'isinID', 'weightedAvgScore'] + factors[factor + 1:] + factors[:factor]

    X = df.drop(curr, axis=1)
    Y = df['price']

    model.fit(X, Y)


    print('R squared: ', model.score(X, Y))

    plt.scatter(X, Y, color = "red", s = 15)
  
    plt.xlabel(xlabel="{}".format(factors[factor]))
    plt.ylabel(ylabel="Monthly Average Price (in dollars)")
    plt.title("Correlation between {} and Price".format(factors[factor]))
    plt.show()

    factor += 1


# works cited: https://realpython.com/linear-regression-in-python/#regression
# works cited: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html

