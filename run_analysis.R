run_analysis <- function(){  
  
  # ############################################################################################
  # 1. Please set working directory to the run_analysis.R script first
  # 2. In this folder you shold create the folder UCI HAR Dataset and 
  #    unzip the UCI HAR Dataset.zip file
  # ############################################################################################
  # setwd('D:\\Archiv\\Vladimir\\projekt\\course\\coursera\\Getting_and_Cleaning_Data\\Project')
  
  # Step 1. Merges the training and the test sets to create one data set.
  # load train data
  train_data    <- read.table("./UCI HAR Dataset/train/X_train.txt")
  train_label   <- read.table("./UCI HAR Dataset/train/y_train.txt")
  train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  # load test data
  test_data    <- read.table("./UCI HAR Dataset/test/X_test.txt")
  test_label   <- read.table("./UCI HAR Dataset/test/y_test.txt")
  test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  # merge the data
  join_data    <- rbind(train_data, test_data)
  join_label   <- rbind(train_label, test_label)
  join_subject <- rbind(train_subject, test_subject)
  
  # Step 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  features <- read.table("./UCI HAR Dataset/features.txt")
  mean_stddev <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  join_data_mean_stddev <- join_data[, mean_stddev]
  names(join_data_mean_stddev) <- features[mean_stddev, 2]
  names(join_data_mean_stddev) <- gsub("\\(|\\)", "", names(join_data_mean_stddev))
  names(join_data_mean_stddev) <- tolower(names(join_data_mean_stddev)) 
  
  # Step 3. Uses descriptive activity names to name the activities in the data set  
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
  activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
  join_label[,1] = activities[join_label[,1], 2]
  names(join_label) <- "activity"
  
  # Step 4. Appropriately labels the data set with descriptive activity names.  
  names(join_subject) <- "subject"
  cleaned_data <- cbind(join_subject, join_label, join_data)
  write.table(cleaned_data, "merged_cleaned_data.txt")
  
  # Step 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.  
  unique_subjects = unique(join_subject)[,1]
  num_subjects = length(unique(join_subject)[,1])
  num_activities = length(activities[,1])
  num_cols = dim(cleaned_data)[2]
  result = cleaned_data[1:(num_subjects*num_activities), ]
  
  row = 1
  for (s in 1:num_subjects) {
    for (a in 1:num_activities) {
      result[row, 1] = unique_subjects[s]
      result[row, 2] = activities[a, 2]
      tmp <- cleaned_data[cleaned_data$subject == s & cleaned_data$activity == activities[a, 2], ]
      result[row, 3:num_cols] <- colMeans(tmp[, 3:num_cols])
      row = row + 1
    }
  }
  write.table(result, "tidy_data_set_with_the_averages.txt")  
}