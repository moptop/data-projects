wbcd <- read.csv("http://nycdatascience.com/slides/XRIntermediate/part3/data/wisc_bc_data.csv", stringsAsFactors = FALSE)

# Exploring data

str(wbcd)
head(wbcd,2)

# drop the id feature (first variable)
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table of proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x)))
}

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# confirm that normalization worked
summary(wbcd_n$area_mean) # all have a min of 0 and max of 1

library(caret)
trainIndex <- createDataPartition(wbcd$diagnosis, p = 0.6, list = FALSE,  times = 1)

Train <- wbcd_n[trainIndex, ]
Test <- wbcd_n[-trainIndex, ]

# Training a model on the data
fitControl <- trainControl(method = "repeatedcv", 
                           number = 10,
                           repeats = 3)

knnModel <- train(y=wbcd$diagnosis[trainIndex],
                  x=Train,
                  method = "knn",
                  tuneGrid = data.frame(.k = 1:20),
                  trControl = fitControl)
knnModel
plot(knnModel)

# Evaluating model performance
confusionMatrix(predict(knnModel,Test),wbcd$diagnosis[-trainIndex])

# Reduce Dimension
install.packages("corrplot")
library(corrplot)
corrmatrix <- cor(wbcd[-1])
corrplot(corrmatrix, order = "hclust")

pcaProc <- preProcess(wbcd[-1],method='pca')
dataProced <- predict(pcaProc,wbcd[-1])
dim(dataProced) # number of variables reduced from 30 to 10
head(dataProced)

Train <- dataProced[trainIndex, ]
Test <- dataProced[-trainIndex, ]

knnModel2 <- train(y=wbcd$diagnosis[trainIndex],
                   x=Train,
                   method = "knn",
                   tuneGrid = data.frame(.k = 1:20),
                   trControl = fitControl)
confusionMatrix(predict(knnModel2,Test),wbcd$diagnosis[-trainIndex])
# Results just as good using 10 variables as it was using 30
plot(dataProced$PC1,dataProced$PC2)



