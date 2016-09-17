library(data.table)
library(knitr)

#Read Files
dtSubjectTrain <- fread(file.path(getwd(), "UCI HAR Dataset", "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(getwd(), "UCI HAR Dataset", "test", "subject_test.txt"))

dtTrainLabel <- fread(file.path(getwd(), "UCI HAR Dataset", "train", "Y_train.txt"))
dtTestLabel <- fread(file.path(getwd(), "UCI HAR Dataset", "test", "Y_test.txt"))

dtTrainSet <- fread(file.path(getwd(), "UCI HAR Dataset", "train", "X_train.txt"))
dtTestSet <- fread(file.path(getwd(), "UCI HAR Dataset", "test", "X_test.txt"))

dtFeatures <- fread(file.path(getwd(), "UCI HAR Dataset", "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureSignal"))

dtActivityLabel <- fread(file.path(getwd(), "UCI HAR Dataset", "activity_labels.txt"))
setnames(dtActivityLabel, names(dtActivityLabel), c("Label", "activityName"))

#Merge Train and Test Dataset
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "Subject")
dtLabel <- rbind(dtTrainLabel, dtTestLabel)
setnames(dtLabel, "V1", "Label")
dtSet <- rbind(dtTrainSet, dtTestSet)

#Merge Subject, Label and Set
dtSubject <- cbind(dtSubject, dtLabel)
dtSet <- cbind(dtSubject, dtSet)

#Set Subject and Label as keys
setkey(dtSet, Subject, Label)

#Extract only the mean and standard deviation from feature signal
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureSignal)]

#Create Feature Labels
dtFeatures$featureLabel <- paste0("V", dtFeatures$featureNum)

#Extract only the mean and standard deviation from set
dtSet <- dtSet[, c(key(dtSet), dtFeatures$featureLabel), with = FALSE]

#Merge Activity Label
dtSet <- merge(dtSet, dtActivityLabel, by = "Label", all.x = TRUE)
#Add Activity Name as key
setkey(dtSet, Subject, Label, activityName)
#Melting Feature
dtSet <- data.table(melt(dtSet, key(dtSet), variable.name = "featureLabel"))
#Merging Feature
dtSet <- merge(dtSet, dtFeatures[, list(featureNum, featureLabel, featureSignal)], by = "featureLabel", all.x = TRUE)
#Factor Variables
dtSet$activity <- factor(dtSet$activityName)
dtSet$feature <- factor(dtSet$featureSignal)

# Features with 2 categories
y <- matrix(seq(1, 2), nrow = 2)
x <- matrix(c(grepl("^t", dtSet$feature), grepl("^f", dtSet$feature)), ncol = nrow(y))
dtSet$featureDomain <- factor(x %*% y, labels = c("Time", "Frequency"))
x <- matrix(c(grepl("Acc", dtSet$feature), grepl("Gyro", dtSet$feature)), ncol = nrow(y))
dtSet$featureInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepl("BodyAcc", dtSet$feature), grepl("GravityAcc", dtSet$feature)), ncol = nrow(y))
dtSet$featureAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepl("mean()", dtSet$feature), grepl("std()", dtSet$feature)), ncol = nrow(y))
dtSet$featureVariable <- factor(x %*% y, labels = c("Mean", "SD"))
# Features with 1 category
dtSet$featureJerk <- factor(grepl("Jerk", dtSet$feature), labels = c(NA, "Jerk"))
dtSet$featureMagnitude <- factor(grepl("Mag", dtSet$feature), labels = c(NA, "Magnitude"))
# Features with 3 categories
y <- matrix(seq(1, 3), nrow = 3)
x <- matrix(c(grepl("-X", dtSet$feature), grepl("-Y", dtSet$feature), grepl("-Z", dtSet$feature)), ncol = nrow(y))
dtSet$featureAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

#Set key again
setkey(dtSet, Subject, activity, featureDomain, featureAcceleration, featureInstrument, 
       featureJerk, featureMagnitude, featureVariable, featureAxis)

#Create a tidy dataset
TidydtSet <- dtSet[, list(count = .N, average = mean(value)), by = key(dtSet)]
