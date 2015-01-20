=============================
CodeBook for Human Activity Recognition
=============================

# Raw Data
The data is collected from "Human Activity Recognition Using Smartphones Dataset
Version 1.0" leaded by Jorge L. Reyes-Ortiz et al.  The basic description of the raw data is following:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

# Feature selection

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

In this study, we will focuse on the mean and standard deviation (std) from those signals.

# Data Processing and Data Table

The *run_ayalysis.R* code is used to process the data and produce the result named as *result.txt*.  This code requires a R library **dplyr** and the unzipped data at the working directory.

We merge the data from training and test sets into a single txt file.  The firs two columns correspond to a subject and a her/his activity.  The following columns are the mean and std of each feature's signal given the subject and the activity.  The feature names are following:

tBodyAcc<?>.XYZ
tGravityAcc<?>.XYZ
tBodyAccJerk<?>.XYZ
tBodyGyro<?>.XYZ
tBodyGyroJerk<?>.XYZ
tBodyAccMag<?>
tGravityAccMag<?>
tBodyAccJerkMag<?>
tBodyGyroMag<?>
tBodyGyroJerkMag<?>
fBodyAcc<?>.XYZ
fBodyAccJerk<?>.XYZ
fBodyGyro<?>.XYZ
fBodyAccMag<?>
fBodyAccJerkMag<?>
fBodyGyroMag<?>
fBodyGyroJerkMag<?>

Where <?> stands for *mean* or *std*.  And one of *XYZ* represents the direction of movement.  And all features are normalized and bounded within [-1,1] from the raw data.


