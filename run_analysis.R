###############################################################
# Assignment: Getting and Cleaning Data Course Project
#############################################################

# load library
library(dplyr)

#set work direction with the data files
setwd('C:/Users/sbom/Documents/Trainings/UCI HAR Dataset')

#############################
  ###Get the train data###
#############################
data_features <- read.table("./features.txt",header=FALSE)
data_labels <-read.table ("./activity_labels.txt", header = FALSE)
data_train_subject <- read.table("./train/subject_train.txt", header=FALSE)
data_xtrain <- read.table("./train/X_train.txt", header=FALSE)
data_ytrain <- read.table("./train/y_train.txt", header=FALSE)

#assign the name of the columns to the tables
colnames(data_labels)<-c("Activity_Id","Activity_Type")
colnames(data_train_subject) <- "Subject_Id"
colnames(data_xtrain) <- data_features[,2]
colnames(data_ytrain) <- "Activity_Id"

#Merging the train data
Data_train <- cbind(data_ytrain,data_train_subject,data_xtrain)

#############################
###Get the test data###
#############################
data_test_subject    <-read.table("./test/subject_test.txt", header=FALSE)
data_xtest         <- read.table("./test/X_test.txt", header=FALSE)
data_ytest         <- read.table("./test/y_test.txt", header=FALSE)

# Assign the name of the columns to the tables

colnames(data_test_subject) <- "Subject_Id"
colnames(data_xtest) <- data_features[,2]
colnames(data_ytest) <- "Activity_Id"

#Merging the test data
Data_test <- cbind(data_ytest,data_test_subject,data_xtest)

#Merge the train and the test data set to create one date set: 
Total_data <- rbind(Data_train,Data_test)

#Return only data columns with mean and standard deviation;
Total_data_mean_std <- Total_data[,grepl("mean()|std()|Activity_Id|Subject_Id",colnames(Total_data))]

#Use descriptive activity names to name the activities in the data set: 
Total_data_mean_std$Activity_Id <- data_labels[Total_data_mean_std$Activity_Id, 2]

# Appropriately labels the data set with descriptive variable names.
names(Total_data_mean_std)<-gsub("^t", "Time ", names(Total_data_mean_std))
names(Total_data_mean_std)<-gsub("^f", "Frequency ", names(Total_data_mean_std))
names(Total_data_mean_std)<-gsub("Acc", "Accelerometer ", names(Total_data_mean_std))
names(Total_data_mean_std)<-gsub("Gyro", "Gyroscope ", names(Total_data_mean_std))
names(Total_data_mean_std)<-gsub("Mag", "Magnitude ", names(Total_data_mean_std))
names(Total_data_mean_std)<-gsub("BodyBody", "Body ", names(Total_data_mean_std))

# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.
Tidy_data_set <- Total_data_mean_std %>%
  group_by(Subject_Id, Activity_Id) %>%
  summarise_all(funs(mean))
write.table(Tidy_data_set, "FinalData.txt", row.name=FALSE)
