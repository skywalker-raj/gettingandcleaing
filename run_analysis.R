library(plyr)

# Create folder for data, download data file and extract data
if(!file.exists("data.zip")){
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    data_filename <- "data.zip"
    filepath <- paste(getwd(),data_filename, sep="/")
    if(!file.exists(filepath)){
        download.file(url, filepath)
    }
}
#If data.zip is not extracted, extract it 
if(!file.exists("data")){
        unzip("data.zip")
}

#Merge the training and the test sets to create one data set.
#training data
trnData<-read.table('./data/train/X_train.txt')
trnLabel<-read.table('./data/train/y_train.txt')
trnSubject<-read.table('./data/train/subject_train.txt')

#test data
testData<-read.table('./data/test/X_test.txt')
testLabel<-read.table('./data/test/y_test.txt')
testSubject<-read.table('./data/test/subject_test.txt')

#Merge training data and test data
mergedData<-rbind(trnData,testData)
mergedLabel<-rbind(trnLabel,testLabel)
mergedSubject<-rbind(trnSubject,testSubject)

#2. Extract only the measurements on the mean and standard deviation for each measurement. 
features<-read.table('./data/features.txt')
#extract features with mean() and standard deviation
selectedFeatures<-grep("mean\\(\\)|std\\(\\)",features[,2]) #66 columns of mean and std for each type of measurements

#Subset mergedData with the selected features (mean and std only)
mergedData<-mergedData[,selectedFeatures]
#Assign colum name to the subsetted Data
colnames(mergedData)<-features[selectedFeatures,2]


#3. Use descriptive activity names to name the activities in the data set
activities <- read.table("./data/activity_labels.txt")
activityLabel <- activities[mergedLabel[,1], 2]
mergedLabel[,1] <- activityLabel
names(mergedLabel) <- "activity"


#4. Appropriately labels the data set with descriptive variable names. 
# Make column names more readable, t means Time and f Means frequency as mentioned in 
# features_info.txt

fromColRegex <- c("\\(\\)","^t","^f","BodyAcc","GravityAcc","Mag","mean","std","-")
toColRegex <- c("","TimeOf","FrequencyOf","BodyAcceleration","GravityAcceleration","Magnitude","Mean","Std","_")
for(i in 1:length(fromColRegex)){
    names(mergedData) <- gsub(fromColRegex[i],toColRegex[i], names(mergedData))
}

#Combine subject,lable and data
names(mergedSubject) <- "subject"
tidydata <- cbind(mergedSubject, mergedLabel, mergedData)

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
averageTidyData <- ddply(tidydata, .(subject, activity), function(x) colMeans(x[, 3:68]))
write.table(averageTidyData, "averageTidyData.txt", sep = "\t", row.name=FALSE)

#End