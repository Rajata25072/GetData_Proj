#Home COm
setwd("D:/Dropbox/MOOC_Ongoing/Data_Science_Specialization_S/DS_Directory/3_GetData_Assn")

#Panya Com
#setwd("C:/Users/rajata_r/Dropbox/MOOC_Ongoing/Data_Science_Specialization_S/DS_Directory/3_GetData_Assn")

# 1. Merges the training and the test sets to create one data set.
#    1.1 Load data for variable and activity/measurement names
varNames <- read.table("UCI_HAR_Dataset/features.txt")
actNames <- read.table("UCI_HAR_Dataset/activity_labels.txt")

#    1.2 Load every data for training set
trainData <- read.table("UCI_HAR_Dataset/train/X_train.txt")
colnames(trainData) <- varNames[,2]
trainLab <- readLines("UCI_HAR_Dataset/train/y_train.txt")
trainSub <- readLines("UCI_HAR_Dataset/train/subject_train.txt")
trainAll <- cbind(type = "train", subject = trainSub)
trainAll <- cbind(trainAll, measurement = trainLab)
trainAll <- cbind(trainAll, trainData)

#     1.3 Load every data for testing set
testData <- read.table("UCI_HAR_Dataset/test/X_test.txt")
colnames(testData) <- varNames[,2]
testLab <- readLines("UCI_HAR_Dataset/test/y_test.txt")
testSub <- readLines("UCI_HAR_Dataset/test/subject_test.txt")
testAll <- cbind(type = "test", subject = testSub)
testAll <- cbind(testAll, measurement = testLab)
testAll <- cbind(testAll, testData)

#     1.4 Combine Test & Train Set to create one data set
runData <- rbind(trainAll, testAll)


# 2. Extracts only the measurements on the mean and sd for each measurement. 
#     2.1 Extract mean for each measurement
runPara1_mean <- data.frame(measurement = c(1:6), variable = "mean")
for (i in c(1:561)){
        for (m in c(1:6)){
                runPara1_mean[m,i+2] <- mean(runData[,i+3][runData$measurement == m], na.rm =TRUE)
        }
}
#     2.2 Extract sd for each measurement
runPara1_sd <- data.frame(measurement = c(1:6), variable = "sd")
for (i in c(1:561)){
        for (m in c(1:6)){
                runPara1_sd[m,i+2] <- sd(runData[,i+3][runData$measurement == m], na.rm =TRUE)
        }
}

#     2.3 Combine mean and sd to create parameter data named "runPara1"
runPara1 <- rbind(runPara1_mean, runPara1_sd)

#               2.3.1 Name the column
colnames(runPara1) <- c("measurement", "variable", as.character(varNames[,2]))

# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
runPara1$measurement <- actNames[,2]

# check the result
runPara1[,1:10]


# 5. Creates a second, independent tidy data set with the average of each variable 
#    for each activity and each subject named "runPara2"
runPara2 <- data.frame(subject = rep(1:30,each = 6), measurement = rep(1:6, 30))

#     5.1 Find mean for each subject and measurement
for (i in 1:561){
        for (m in 1:180){
                runPara2[m,i+2] <- mean(as.numeric(runData[,i+3][runData$subject == runPara2[m,1]]
                                        [runData$measurement == runPara2[m,2]]), na.rm =TRUE)        
        }
}

#     5.2 Name the column
colnames(runPara2) <- c("subject", "measurement", as.character(varNames[,2]))
#     5.3 Name each measurement
runPara2$measurement <- actNames[,2]

# 6. Export Text file to submit
write.table(runPara2, "tidyDataSet.txt")

#     6.1 Read the table to check   
str(read.table("runMeanbySubjMeas.txt"))

listOfVariables <- data.frame(names(runPara2))
write.csv(listOfVariables,"listOfVariables.csv")