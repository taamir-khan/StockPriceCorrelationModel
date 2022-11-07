# loads the data
data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]

# prints the first 6 rows
head(my_data, 6)

# computes the correlation matrix
res <- cor(my_data)
round(res, 2)
