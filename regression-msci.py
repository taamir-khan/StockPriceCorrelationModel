import pandas as pd
import matplotlib.pyplot as plt
#from sklearn.model_selection import train_test_split
from sklearn import linear_model

#replace with name of any csv file
#Note: must be a csv file, not excel (convert excel file to csv)
df = pd.read_csv('msci-test.csv')
df.dropna(axis = 0, how = 'any', thresh = None, subset = None, inplace = True)


###correlation between each individual factor and price
factors = ['MSCI Factor 1', 'MSCI Factor 2', 'MSCI Factor 3', 'MSCI Factor 4']
model = linear_model.LinearRegression()

factor = 0
while factor < len(factors):
    curr = ['Price', 'Name', 'Date'] + factors[factor + 1:] + factors[:factor]

    X = df.drop(curr, axis=1)
    Y = df['Price']

    model.fit(X, Y)


    print('Correlation coefficient: ', model.coef_)
    print('R squared: ', model.score(X, Y))

    plt.scatter(X, Y, color = "red", s = 15)
  
    plt.xlabel(xlabel="{}".format(factors[factor]))
    plt.ylabel(ylabel="Price")
    plt.title("Correlation between {} and Price".format(factors[factor]))
    plt.show()

    factor += 1


# works cited: https://realpython.com/linear-regression-in-python/#regression'''
# works cited: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html