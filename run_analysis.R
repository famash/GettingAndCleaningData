
###############################################################
# Library
###############################################################
## The libraries used in this operation are data.table and dplyr. 
## data.table as it is efficient in handling large data as tables. 
## dplyr is used to aggregate variables to create the tidy data.

library(data.table)
library(dplyr)

###############################################################
# Read Supporting Metadata
###############################################################
## The supporting metadata in this data are the name of the features and the name of the activities.
## These are loaded into variables featureNames and activityLabels. 

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

#####################################################################################
# Format training and test data sets
# Both training and test data sets are split up into subject, activity and features. 
# They are present in three different files.
#####################################################################################

## Read training data

subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## Read test data

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


###################################################################
# PART 1 - MERGE THE TRAINING & THE TEST SET TO CREATE ONE DATASET
###################################################################

## Combine the respective data in training and test data sets corresponding to subject, activity and features.
## The results are stored in subject, activity and features.

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

## Naming the columns
## The columns in the features data set can be named from the metadata in featureNames

colnames(features) <- t(featureNames[2])

## Merge the data
## The data in features,activity and subject are merged and the complete data is now stored in completeData.

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)


###################################################################
# PART 2 - EXTRACTS ONLY THE MEASUREMENT ON THE MEAN AND STANDARD 
#          DEVIATION FOR EACH MEASUREMENT
###################################################################

## Extract the column indices that have either mean or std in them.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

## Add activity and subject columns to the list and look at the dimension of completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

## create extractedData with the selected columns in requiredColumns. 
## Verfy the dimension of requiredColumns.

extractedData <- completeData[,requiredColumns]
dim(extractedData)

################################################################################
# PART 3 - USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITY IN THE DATA SET
################################################################################

## The activity field in extractedData is originally of numeric type. 
## Have to change its type to character so that it can accept activity names. 
## The activity names are taken from metadata activityLabels.

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

## Have to factor the activity variable, once the activity names are updated.

extractedData$Activity <- as.factor(extractedData$Activity)

################################################################################
# PART 4 - APPROPRIATELY LABELS THE DATASET WITH DESCRIPTIVE VARIABLE NAMES
################################################################################

## Below are the names of the variables in extractedData

names(extractedData)


## Replacing the following acronyms 
## Acc --> Accelerometer
## Gyro --> Gyroscope
## BodyBody --> Body
## Mag  --> Magnitude
## Character f --> Frequency
## Character t --> Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

## Print the names of the variables in extractedData after edited

names(extractedData)


##################################################################################
# PART 5 - FROM THE DATASET IN STEP 4, CREATE A SECOND , INDEPENDENT TIDY DATASET
#          WITH THE AVERAGE OF EACH VARIABLE FOR EACH VARIABLE FOR EACH ACTIVITY
#          AND EACH SUBJECT
##################################################################################

##  Use subject as a factor variable.

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

## Create tidyData as a data set with average for each activity and subject. 
## Then, order the enties in tidyData and write it into data file Tidy.txt that contains the processed data.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)