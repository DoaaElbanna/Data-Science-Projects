# Getting and cleaning data project

# run_analysis.R script should have:

# 1-Merges the training and the test sets to create one data set.
# 2-Extracts only the measurements on the mean and standard deviation for each measurement.
# 3-Uses descriptive activity names to name the activities in the data set
# 4-Appropriately labels the data set with descriptive variable names.
# 5-From the data set in step 4, creates a second, 
# independent tidy data set with the average of each variable for each activity and each subject.

# Data set:UCI HAR Dataset

# Loading Training files
train_x <- read.table("../data/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("../data/UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("../data/UCI HAR Dataset/train/subject_train.txt")


# Loading Testing files
test_x <- read.table("../data/UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("../data/UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("../data/UCI HAR Dataset/test/subject_test.txt")


# Load features file
features <- read.table("../data/UCI HAR Dataset/features.txt" )


# Load activity_labels file
activity_data <- read.table("../data/UCI HAR Dataset/activity_labels.txt")


# Change columns names:
colnames(train_x) <- features[,2]
colnames(train_y) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(test_x) <- features[,2] 
colnames(test_y) <- "activityId"
colnames(subject_test) <- "subjectId"


colnames(activity_data) <- c('activityId','activityName')


# 1-Merging all data in one data set:

train <- cbind(train_y, subject_train, train_x)
test <- cbind(test_y, subject_test, test_x )
merged_data <- rbind(train, test)


#************************************************************#
# 2- Extracts only the measurements on the mean and standard deviation for each measurement.

col_names <- colnames(merged_data)
mesure_mean_std <- (grepl("activityId" , col_names) | 
                      grepl("subjectId" , col_names) | 
                      grepl("mean.." , col_names) | 
                      grepl("std.." , col_names) 
)

# making a data set with mean and std features only 
set_mean_std <- merged_data[,mesure_mean_std == TRUE]


#************************************************************#
# 3- Uses descriptive activity names to name the activities in the data set
set_activity_names <- merge(set_mean_std, activity_data,
                              by='activityId',
                              all.x=TRUE)


#************************************************************#
# 4-Appropriately labels the data set with descriptive variable names.
# I'm already to this step in the previous steps


#************************************************************#
# 5-From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.

tidySet <- aggregate(. ~subjectId + activityId, set_activity_names, mean)
tidySet <- tidySet[order(tidySet$subjectId, tidySet$activityId),]

# Create a tidy data set txt file
write.table(tidySet, "tidySet.txt", row.name=FALSE)






