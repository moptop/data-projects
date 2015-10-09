## The purpose is to demonstrate your ability to collect, work with, and clean a data set.
## The goal is to prepare tidy data that can be used for later analysis.

## One of the most exciting areas in all of data science right now is wearable computing. The data
## linked to from the course website represent data collected from the accelerometers from the
## Samsung Galaxy S smartphone. A full description is available at the site where the data was
## obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Here are the data for the project: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## Read in the data and see what it looks like ###################################################
setwd("C:/Users/David/Classes/Coursera/JohnsHopkinsDataScienceSignatureTrack/Getting_Cleaning_Data/ProgAssign1")
Labels <- read.table("./UCI_HAR_Dataset/activity_labels.txt")
Features <- read.table("./UCI_HAR_Dataset/features.txt")

# Subset the Features for use in subsetting the xtest data later
library(sqldf)
FeatureSubset1 <- subset(Features, grepl(pattern="mean()",V2, fixed=TRUE))
FeatureSubset2 <- subset(Features, grepl(pattern="std()",V2, fixed=TRUE))

## From the "test" data file ##########################################################

# Read in the data
subjecttest <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")
ytest <- read.table("./UCI_HAR_Dataset/test/y_test.txt")
xtest <- read.table("./UCI_HAR_Dataset/test/x_test.txt")

# Change the column names
names(subjecttest) <- "Subject"
names(ytest) <- "Activity"
names(xtest) <- Features$V2

## Combine the test data into one dataframe (subject + y + x)
total_test <- cbind(subjecttest, ytest, xtest)

## From the "train" data file #########################################################

# Read in the data
subjecttrain <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")
ytrain <- read.table("./UCI_HAR_Dataset/train/y_train.txt")
xtrain <- read.table("./UCI_HAR_Dataset/train/x_train.txt")

# Change the column names
names(subjecttrain) <- "Subject"
names(ytrain) <- "Activity"
names(xtrain) <- Features$V2

## Combine the training data into one dataframe (subject + y + x)
total_train <- cbind(subjecttrain, ytrain, xtrain)

## Add test data to training ##########################################################
total_data <- rbind(total_test, total_train)

## Rename the Activities column from a number to the activity description #############
total_data[2][total_data[2] == 1] <- "Walking"
total_data[2][total_data[2] == 2] <- "Walking Upstairs"
total_data[2][total_data[2] == 3] <- "Walking Downstairs"
total_data[2][total_data[2] == 4] <- "Sitting"
total_data[2][total_data[2] == 5] <- "Standing"
total_data[2][total_data[2] == 6] <- "Laying"

## Subset only the columns of data that have "mean()" or "std()" in the column name ###
## The first two columns ("Subject" and "Activity") should also be kept ###############
subset_data <- total_data[,(names(total_data) %in% FeatureSubset1$V2) |
                           (names(total_data) %in% FeatureSubset2$V2) |
                           (names(total_data) == "Subject") |
                           (names(total_data) == "Activity")]

## Remove the "()" from the column names ##################################
names(subset_data) <- gsub(pattern = "()", replacement="", x=names(subset_data), fixed=TRUE)

## Create a second tidy data set with the average of each variable for each activity and each subject.

library(reshape2)

# Melt the first tidy set by Subject and Activity, measuring the rest of the variables 
subset_melt <- melt(subset_data, id=c("Subject","Activity"), measure.vars=names(subset_data)[3:68])

# Re-cast the melted data set, taking the mean for each Subject and Activity
averages <- dcast(subset_melt, Subject+Activity ~ variable, mean)

#Display the final tidy data set
head(averages)

#Write the data set to a file
write.csv(averages, file="TidyData.csv")

