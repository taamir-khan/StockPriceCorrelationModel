import pandas as pd
import matplotlib.pyplot as plt
#from sklearn.model_selection import train_test_split
from sklearn import linear_model

#replace with name of any csv file
#Note: must be a csv file, not excel (convert excel file to csv)
df = pd.read_csv(r'C:\Users\Agasti Mhatre\Desktop\MSCI Test\testLYB.csv')
df.dropna(axis = 0, how = 'any', thresh = None, subset = None, inplace = True)
pd.set_option('display.max_columns', None)

###correlation between each individual factor and price
factors = ['INDUSTRY_ADJUSTED_SCORE', 'ENVIRONMENTAL_PILLAR_SCORE', 'GOVERNANCE_PILLAR_SCORE', 'SOCIAL_PILLAR_SCORE']
model = linear_model.LinearRegression()

factor = 0

#ouput correlations coefficients to text_file
output_ = r'C:\Users\Agasti Mhatre\Desktop\MSCI Test\correlation.txt'
output = open(output_, 'w')
output.write(str(df.corr()))
output.close()


#graph each relationship using matplotlib
while factor < len(factors):
    curr = ['Month, Year', 'Average Price (in dollars)'] + factors[factor + 1:] + factors[:factor]

    X = df.drop(curr, axis=1)
    Y = df['Average Price (in dollars)']

    model.fit(X, Y)


    print('R squared: ', model.score(X, Y))

    plt.scatter(X, Y, color = "red", s = 15)
  
    plt.xlabel(xlabel="{}".format(factors[factor]))
    plt.ylabel(ylabel="Monthly Average Price (in dollars)")
    plt.title("Correlation between {} and Price".format(factors[factor]))
    plt.show()

    factor += 1


# works cited: https://realpython.com/linear-regression-in-python/#regression'''
# works cited: https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.LinearRegression.html