## Develop a model to predict whether a given car gets high or low gas
## mileage based on the Auto data set of package ISLR

## Get data
install.packages("ISLR")
library(ISLR)
data(Auto)

## Exploring the Data
str(Auto)
summary(Auto)

## Preparing the Data:
# Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median (high gas mileage), and a 0 if mpg contains
# a value below its median (low gas mileage).

Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)

# Recode mpg01 as a factor and relabel
Auto$mpg01 <- factor(Auto$mpg01, levels = c(0,1),
                         labels = c("Low", "High"))

# remove the mpg variable
Auto <- Auto[-1]

table(Auto$mpg01)
# half and half, as expected from median

# Remove name & origin variables, they do not directly effect gas mileage
Auto <- Auto[,-c(7:8)]

# Study relationships between remaining variables
library(psych)
pairs.panels(Auto[,1:7])
# Gives the distribution of each, pairwise scatterplots & correlation coeff.s
# mpg01 looks prestty strongly influenced by each of the remaining variables,
# but a little less so by acceleration and year. I think acc is a function
# of other variables rather than directly influencing, so take them out.
Auto <- Auto[,-c(5:6)]
# removed acceleration and year
pairs.panels(Auto[,1:5])

## Split the data into a training set and a test set.
library(caret)
trainIndex <- createDataPartition(Auto$mpg01, p = 0.6, list = FALSE,
                                  times = 1)
Train <- Auto[trainIndex, ]
Test <- Auto[-trainIndex, ]

## Perform KNN on the training data, with several values of K, in order to
## predict mpg01. 

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10,
                           repeats = 3)

knnModel <- train(x=Train[1:4], y=Train[,5],
                  method = "knn",
                  tuneGrid = data.frame(.k = 1:20),
                  trControl = fitControl)
knnModel
plot(knnModel)
# The highest accuracy of 0.911 was attinined with k=18, but that is too high.
# There is a spike in accuracy of 0.904 at k=3, which is reasonably close, so
# the best k value to be used is 3.

