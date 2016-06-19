filesPath <- "C:/Users/admin/Desktop/coursera_data science/getting and cleaning data/week4/assignment_getting and cleaning data/data/UCI HAR Dataset"
##setwd(filesPath)

##if(!file.exists("./data")){dir.create("./data")}
##fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download.file(fileUrl,destfile="./data/Dataset.zip")

###Unzip DataSet to /data directory
##unzip(zipfile="./data/Dataset.zip",exdir="./data")


###Load required packages
library(dplyr)
library(data.table)
library(tidyr)


# Read subject files
subjectTrainingData <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")))
subjectTestingData  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )))

# Read activity files
activityTrainingData <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")))
activityTestingData  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )))

#Read data files.
trainingData <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )))
testingData  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )))


##   PART 1
#  this will merge the training and the test sets by row binding & rename variables "subject" and "activityNum"
Subjectdata_total <- rbind(subjectTrainingData, subjectTestingData)
setnames(Subjectdata_total, "V1", "subject")
activitydata_total<- rbind(activityTrainingData, activityTestingData)
setnames(activitydata_total, "V1", "activityNum")

#combine the DATA training and test files
combinedDataTable <- rbind(trainingData, testingData)

# name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
featuresData <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(featuresData, names(featuresData), c("featureNum", "featureName"))
colnames(combinedDataTable) <- featuresData$featureName

#column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

# Merge columns
totaldataSubjct<- cbind(Subjectdata_total, activitydata_total)
combinedDataTable <- cbind(totaldataSubjct, combinedDataTable)

##  PART 2
# Reading "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",featuresData$featureName,value=TRUE)

# Taking only measurements for the mean and standard deviation and add "subject","activityNum"

dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
combinedDataTable<- subset(combinedDataTable,select=dataFeaturesMeanStd) 


##  PART 3

##enter name of activity into dataTable
combinedDataTable <- merge(activityLabels, combinedDataTable , by="activityNum", all.x=TRUE)
combinedDataTable$activityName <- as.character(combinedDataTable$activityName)

## create dataTable with variable means sorted by subject and Activity
combinedDataTable$activityName <- as.character(combinedDataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = combinedDataTable, mean) 
combinedDataTable<- tbl_df(arrange(dataAggr,subject,activityName))


## PART 4

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


## PART 5 (aggregate found earlier in part 3)

write.table(combinedDataTable, "TidyData.txt", row.name=FALSE)

