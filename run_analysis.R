filename <- "Finalassigment.zip"

if (!file.exists(filename)){
  fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileurl, filename, method="curl")
}  

# checking for folder
if (!file.exists("UCI dataset")) { 
  unzip(filename) 
}

library(dplyr)

### **1. Merge the training and the test sets to create one data set.**

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
x <- rbind(x_train, x_test)
head(x)

y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
y <- rbind(y_train, y_test)
head(y)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
subject <- rbind(subject_train, subject_test)
head(subject)

## we merge all 3 dataset
datamerged <- cbind(subject, y, x)
head(datamerged)


### **2.Extracts only the measurements on the mean and standard deviation for each measurement.**

measurments <- datamerged %>% select(subject, code, contains("mean"), contains("std"))
head(measurments)

### **3. Uses descriptive activity names to name the activities in the data set**
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
measurments$code <- activities[measurments$code,2]
head(measurments)

### **4. Appropriately labels the data set with descriptive variable names.**

names(measurments)[2] = "activity"
names(measurments)<-gsub("Acc", "Accelerometer", names(measurments))
names(measurments)<-gsub("Gyro", "Gyroscope", names(measurments))
names(measurments)<-gsub("BodyBody", "Body", names(measurments))
names(measurments)<-gsub("Mag", "Magnitude", names(measurments))
names(measurments)<-gsub("^t", "Time", names(measurments))
names(measurments)<-gsub("^f", "Frequency", names(measurments))
names(measurments)<-gsub("tBody", "TimeBody", names(measurments))
names(measurments)<-gsub("-mean()", "Mean", names(measurments), ignore.case = TRUE)
names(measurments)<-gsub("-std()", "STD", names(measurments), ignore.case = TRUE)
names(measurments)<-gsub("-freq()", "Frequency", names(measurments), ignore.case = TRUE)
names(measurments)<-gsub("angle", "Angle", names(measurments))
names(measurments)<-gsub("gravity", "Gravity", names(measurments))

head(measurments)

### **5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.**

library(dplyr)
tidydataset <- measurments %>% 
  group_by(subject, activity)%>% 
  summarise_all(funs(mean))
write.table(tidydataset, "tidydataset.txt", row.name=FALSE)

head(tidydataset)
str(tidydataset)