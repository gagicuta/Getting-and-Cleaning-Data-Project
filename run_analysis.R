## Get data from web, save it and unzip it

dataZipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataZipUrl,destfile="./data/UCI HAR Dataset.zip", mode= "wb")
 unzip("./data/UCI HAR Dataset.zip",exdir = "./data/UCI HAR Dataset")

## Read data by training and test sets
#### Training set

trainSubject <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
trainData <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
trainActivity <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

####Test set

testSubject <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
testData <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

## Read features file

features <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/features.txt", as.is = TRUE)

## Read activity labels

activities <- read.table("./data/UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt") 
colnames(activities) <- c("activityId", "activityLabel") 

## Step 1 - Merge the training and the test sets to create one data set

mergedActivityData <- rbind( 
   cbind(trainSubject, trainData, trainActivity), 
   cbind(testSubject, testData, testActivity) 
) 
 
 
# insert column names 
colnames(mergedActivityData) <- c("subject", features[, 2], "activity") 

## Step 2 - Extracts only the measurements on the mean and standard deviation 
#          for each measurement 
 
# keep columns of dataset based on column names as needed

columnsNeeded <- grepl("subject|activity|mean|std", colnames(mergedActivityData)) 
activityData <- mergedActivityData[, columnsNeeded] 

##Step 3 - Use descriptive activity names to name the activities in the data 
#          set 


# replace activity values with named factor levels 
activityData$activity <- factor(activityData$activity, levels = activities[, 1], 
                         labels = activities[, 2]) 

# Step 4 - Appropriately label the data set with descriptive variable names 
 
# get column names 
activityDataCols <- colnames(activityData) 
 
# remove special characters 
activityDataCols <- gsub("[\\(\\)-]", "", activityDataCols) 
 
# expand abbreviations and clean up names 
activityDataCols <- gsub("^f", "frequencyDomain", activityDataCols) 
activityDataCols <- gsub("^t", "timeDomain", activityDataCols) 
activityDataCols <- gsub("Acc", "Accelerometer", activityDataCols) 
activityDataCols <- gsub("Gyro", "Gyroscope", activityDataCols) 
activityDataCols <- gsub("Mag", "Magnitude", activityDataCols) 
activityDataCols <- gsub("Freq", "Frequency", activityDataCols) 
activityDataCols <- gsub("mean", "Mean", activityDataCols) 
activityDataCols <- gsub("std", "StandardDeviation", activityDataCols) 
activityDataCols <- gsub("BodyBody", "Body", activityDataCols) 
 
# use new labels as column names 
colnames(activityData) <- activityDataCols 

# Step 5 - Create a second, independent tidy set with the average of each 
#          variable for each activity and each subject 
 
# group by subject and activity and summarise using mean 
activityDataMeans <- activityData %>%  
group_by(subject, activity) %>% 
summarise_each(funs(mean)) 
 
# output to file "tidy_data.txt" 
write.table(activityDataMeans, "tidy_data.txt", row.names = FALSE,  
            quote = FALSE) 



