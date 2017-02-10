## run_analysis is a function that performs the following:
## 1- Merges the training and the test sets to create one data set.
## 2- Extracts only the measurements on the mean and standard deviation for each measurement.
## 3- Uses descriptive activity names to name the activities in the data set
## 4- Appropriately labels the data set with descriptive variable names.
## 5- From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

run_analysis <- function(){
  library(plyr)
  library(dplyr)

  ## Gather all the text files into seperate data frames in R
  actvity.labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
  features <- read.table("./UCI HAR Dataset/features.txt")

  ## Read in the test folder
  test.subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  test.x <- read.table("./UCI HAR Dataset/test/X_test.txt")
  names(test.x) <- as.character(features$V2) ## rename the columns in the dataframe
  
  test.y <- read.table("./UCI HAR Dataset/test/Y_test.txt")
  
  ## Merge and rename the columns of the resulting data frame
  test.merged <- data.frame(test.subject,test.y,test.x)
  names(test.merged)[1] <- "subject"
  names(test.merged)[2] <- "activity"
  
  ## Read in the train folder
  train.subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  train.x <- read.table("./UCI HAR Dataset/train/X_train.txt")
  names(train.x) <- as.character(features$V2) ## rename the columns in the dataframe
  train.y <- read.table("./UCI HAR Dataset/train/Y_train.txt")
  
  ## Merge and rename the columns of the resulting data frame
  train.merged <- data.frame(train.subject,train.y,train.x)
  names(train.merged)[1] <- "subject"
  names(train.merged)[2] <- "activity"
  
  merged.data <- bind_rows(test.merged,train.merged)
  
  ## Change the Actvity column to factor labels rather than numbers
  
  merged.data$activity <- cut(merged.data$activity, breaks = 6, labels = actvity.labels$V2)
  
  ## return the mean and standard deviation in a dataframe
  
  mean.sd.data <- cbind(select(merged.data,subject:activity),
                        select(merged.data,contains("mean")),
                        select(merged.data,contains("std")))
  
  ## Create the tidy datasets required
  
  ## 1 - Create the average of the actvities dataframe
  ave.activity <- aggregate(mean.sd.data[,3:88],list(mean.sd.data$activity), mean)
  ave.activity
  
  ## 2 - Create the average of the subjects dataframe
  ave.subject <- aggregate(mean.sd.data[,3:88],list(mean.sd.data$subject), mean)
  
  ## 3 - Return both data frames in a list
  
  write.csv(ave.activity,"ave.activity.csv")
  write.csv(ave.subject,"ave.subject.csv")
  
  ##return(list(ave.activity,ave.subject))
  
}