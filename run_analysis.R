#!/usr/bin/which R
# load library
library(dplyr)

# Load raw data
feature.names <- read.delim("UCI HAR Dataset/features.txt", sep=" ", stringsAsFactors=FALSE, header=FALSE)[,2]
feature.names <- gsub("\\(\\)-",".", feature.names)
activity.names <- read.delim("UCI HAR Dataset/activity_labels.txt", sep=" ", stringsAsFactors=FALSE, header=FALSE)[,2]
load.data <- function(filename){
        # Task 4 Appropriately labels
        X <- read.table(sprintf("UCI HAR Dataset/%s/X_%s.txt",filename,filename), col.names=feature.names) 
        subject <- read.table(sprintf("UCI HAR Dataset/%s/subject_%s.txt",filename,filename), col.names="subject", colClasses="factor")
        activity <- read.table(sprintf("UCI HAR Dataset/%s/y_%s.txt",filename,filename), col.names="activity.id")
        # Task 3 - descriptive activity name
        activity$activity <- factor(activity$activity.id, labels=activity.names)
        cbind.data.frame(X,subject,activity=activity$activity)
}

# Task 1 Merge training and test sets
training <- load.data("train")
test <- load.data("test")
df <- tbl_df(rbind.data.frame(training, test))

# Task 2 extract measurements on mean and standard deviation
df.small <- select(df, subject, activity, contains("mean"), contains("std"))

# Task 5 calculate the average for each variable
mean.df <- df.small %>% group_by(subject, activity) %>%
        summarise_each(funs(mean))
names(mean.df) <- gsub("\\.\\.","",names(mean.df))
write.table(mean.df, file="result.txt", sep=", ", row.name=FALSE, quote=FALSE)
