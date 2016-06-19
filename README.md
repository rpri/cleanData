#Peer Graded Assignment: Getting and Cleaning Data Course Project

This repo contains one R script called run_analysis.R that does the following.
1.	Merges the training and the test sets to create one data set.
2.	Extracts only the measurements on the mean and standard deviation for each measurement.
3.	Uses descriptive activity names to name the activities in the data set
4.	Appropriately labels the data set with descriptive variable names.
5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##DATA
Data linked to from the specified website http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Represent data collected from the accelerometers from the Samsung galaxy S smartphone. 

##ANALYSIS Performed
###Download data
filesPath <- "C:/Users/admin/Desktop/coursera_data science/getting and cleaning data/week4/assignment_getting and cleaning data/data/UCI HAR Dataset"
setwd(filesPath)

if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip")

#Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

###Packages used

library(dplyr)
library(data.table)
library(tidyr)

Read Files containing data

# Read subject files
subjectTrainingData <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
subjectTestingData  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
activityTrainingData <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
activityTestingData  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files.
trainingData <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
testingData  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))

Part 1- Merges the training and the test sets to create one data set
Subjectdata_total <- rbind(subjectTrainingData, subjectTestingData)
setnames(Subjectdata_total, "V1", "subject")
activitydata_total<- rbind(activityTrainingData, activityTestingData)
setnames(activitydata_total, "V1", "activityNum")

#combine the DATA training and test files
combinedDataTable <- rbind(trainingData, testingData)

# name variables 
featuresData <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(featuresData, names(featuresData), c("featureNum", "featureName"))
colnames(combinedDataTable) <- featuresData$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
totaldataSubjct<- cbind(Subjectdata_total, activitydata_total)
combinedDataTable <- cbind(totaldataSubjct, combinedDataTable)

Part 2- Extracts only the measurements on the mean and standard deviation for each measurement.
# Reading "features.txt" and extracting only the mean and standard deviation
featuresMeanStdData <- grep("mean\\(\\)|std\\(\\)",featuresData$featureName,value=TRUE)

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"
featuresMeanStdData <- union(c("subject","activityNum"), featuresMeanStdData)
combinedDataTable<- subset(combinedDataTable,select=featuresMeanStdData) 

Part 3- Uses descriptive activity names to name the activities in the data set
##enter name of activity into dataTable
combinedDataTable <- merge(activityLabels, combinedDataTable , by="activityNum", all.x=TRUE)
combinedDataTable$activityName <- as.character(combinedDataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
combinedDataTable$activityName <- as.character(combinedDataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = combinedDataTable, mean) 
combinedDataTable<- tbl_df(arrange(dataAggr,subject,activityName))

Part 4- Appropriately labels the data set with descriptive variable names.
#Earlier Names 
head(str(combinedDataTable),2)

names(combinedDataTable)<-gsub("std()", "SD", names(combinedDataTable))
names(combinedDataTable)<-gsub("mean()", "MEAN", names(combinedDataTable))
names(combinedDataTable)<-gsub("^t", "time", names(combinedDataTable))
names(combinedDataTable)<-gsub("^f", "frequency", names(combinedDataTable))
names(combinedDataTable)<-gsub("Acc", "Accelerometer", names(combinedDataTable))
names(combinedDataTable)<-gsub("Gyro", "Gyroscope", names(combinedDataTable))
names(combinedDataTable)<-gsub("Mag", "Magnitude", names(combinedDataTable))
names(combinedDataTable)<-gsub("BodyBody", "Body", names(combinedDataTable))

# Final Names 
head(str(combinedDataTable),6)


Part 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(combinedDataTable, "TidyData.txt", row.name=FALSE)


 




