install.packages("dplyr")
install.packages("corrplot")
library("dplyr")
library("corrplot")

# Getting just numeric variables for correlation matrix

my_data <- union_participation[, c(4,5,6,7,8,9)]

# First 5 rows of new data

head(my_data,5)

# Correlation matrix

res <- cor(my_data)
round(res,2)

#Correlation plot

corrplot(res, method = "circle")
