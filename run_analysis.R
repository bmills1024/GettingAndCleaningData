# 1.  Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation 
#     for each measurement. 
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names. 
# 5.	From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.

library(dplyr) 

#Set folder and file names
folder <- "UCI HAR Dataset"
subject_test <- paste(folder, "/test/subject_test.txt", sep = "")
subject_train <- paste(folder, "/train/subject_train.txt", sep = "")
X_test <- paste(folder, "/test/X_test.txt", sep = "")
X_train <- paste(folder, "/train/X_train.txt", sep = "")
y_test <- paste(folder, "/test/y_test.txt", sep = "")
y_train <- paste(folder, "/train/y_train.txt", sep = "")
features <- paste(folder, "/features.txt", sep = "")
activity_labels <- paste(folder, "/activity_labels.txt", sep = "")

#Check for "UCI HAR Dataset" folder
if (!file.exists(folder)) {  
  stop(paste(folder,  " not found.  Please set download file and set working folder.", sep = ""))}

#Load features and activity data 
ifelse (file.exists(features),  
        dtfeatures <- read.table(features, comment.char = "", col.names=c("featureID", "featurename")), 
        stop(paste(features,  " not found", sep = "")))
ifelse (file.exists(activity_labels),  
        dtactivity_labels <- read.table(activity_labels, comment.char = "", col.names=c("activityID", "activity")), 
        stop(paste(activity_labels,  " not found", sep = "")))

#load test data
ifelse (file.exists(subject_test),
        dtsubject_test <- read.table(subject_test, comment.char = "", col.names="subjectID"), 
        stop(paste(subject_test,  " not found", sep = "")))
ifelse (file.exists(y_test), 
        dty_test <- read.table(y_test, comment.char = "", col.names="activityID"), 
        stop(paste(y_test,  " not found", sep = "")))
ifelse (file.exists(X_test), 
        dtX_test <- read.table(X_test, comment.char = ""), 
        stop(paste(X_test,  " not found", sep = "")))
#Assign meaningful names to dtX_test
names(dtX_test) <- dtfeatures$featurename
#Identify mean() and avg() columns to keep
keepers <- grep("mean\\(\\)|std\\(\\)", dtfeatures$featurename)
#Extract mean() and avg() columns
dtX_test <- dtX_test[ ,keepers]
#Combine test data tables
dtTest <- cbind(dtsubject_test, dty_test, dtX_test)

#Remove unneeded tables to save space in memory
rm(dtsubject_test, dty_test, dtX_test)

#Load train data
ifelse (file.exists(subject_train),  
        dtsubject_train <- read.table(subject_train, comment.char = "", col.names="subjectID"), 
        stop(paste(subject_train,  " not found", sep = "")))
ifelse (file.exists(y_train), 
        dty_train <- read.table(y_train, comment.char = "", col.names="activityID"), 
        stop(paste(y_train,  " not found", sep = "")))
ifelse (file.exists(X_train), 
#        dtX_train <- read.table(X_train, comment.char = "", col.names="dtfeatures$featurename"), 
        dtX_train <- read.table(X_train, comment.char = ""), 
        stop(paste(X_train,  " not found", sep = "")))
#Assign meaningful names to dtX_train
names(dtX_train) <- dtfeatures$featurename
#Extract mean() and avg() columns
dtX_train <- dtX_train[ ,keepers]
#Combine train data tables
dtTrain <- cbind(dtsubject_train, dty_train, dtX_train)
#Remove unneeded tables to save space in memory
rm(dtsubject_train, dty_train, dtX_train)

#Combine test and train tables
dtdata <- rbind(dtTrain, dtTest)
#Remove unneeded tables
rm(dtTrain, dtTest)

#Add activity description
dtdata$activityID <- factor(dtdata$activityID, 
                            labels=c("Walking", "Walking Upstairs", 
                                     "Walking Downstairs", "Sitting", 
                                     "Standing", "Laying")) 

# 5.  From the data set in step 4, creates a second, independent tidy data set 
#     with the average of each variable for each activity and each subject.
# Create averages data set
dtdata_agg <- dtdata %>% 
  group_by(subjectID, activityID) %>% 
  summarise_each(funs(mean)) 

# Write averages data set to file
write.table(dtdata_agg, "tidydata.txt", row.names = FALSE, sep = "\t") 
