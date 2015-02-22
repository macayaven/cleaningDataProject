How does the script work?
-First the data is downloaded and unzipped.
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="./data.zip",method="curl")
unzip("./data.zip")


-The train data set (X_test) is loaded into memory
-The test data set (X_test) is loaded into memory
-The train activity index (y_test) is loaded into memory
-The test activity index (y_test) is loaded into memory
-The train subject index (subject_index) is loaded into memory
-The test subject index (subject_test) is loaded into memory
feature_labels = read.table("./UCI HAR Dataset/features.txt", sep=" ",stringsAsFactors=FALSE)
activity_labels = read.table("./UCI HAR Dataset/activity_labels.txt", sep=" ",stringsAsFactors=FALSE)
subject_test = read.table("./UCI HAR Dataset/test/subject_test.txt", sep=" ",stringsAsFactors=FALSE)
x_test = read.table("./UCI HAR Dataset/test/X_test.txt",stringsAsFactors=FALSE)
y_test = read.table("./UCI HAR Dataset/test/y_test.txt", sep=" ",stringsAsFactors=FALSE)
subject_train = read.table("./UCI HAR Dataset/train/subject_train.txt", sep=" ",stringsAsFactors=FALSE)
x_train = read.table("./UCI HAR Dataset/train/X_train.txt",stringsAsFactors=FALSE)
y_train = read.table("./UCI HAR Dataset/train/y_train.txt", sep=" ",stringsAsFactors=FALSE)


-All the columns that are not relative to std or mean are removed
filtered_column_indexes <- sort.int(c(feature_labels[grepl("std\\(",feature_labels$V2),1],feature_labels[grepl("mean\\(",feature_labels$V2),1]))
filtered_colum_names <- feature_labels[filtered_column_indexes,2]

x_test <- x_test[,filtered_column_indexes]
x_train <- x_train[,filtered_column_indexes]

colnames(x_test) <- filtered_colum_names
colnames(x_train) <- filtered_colum_names


- The activity and subject column are added to the train and test data sets
complete_train <- cbind(x_train, y_train)
complete_train <- cbind(complete_train, subject_train)
colnames(complete_train)[67] <- "activity"
colnames(complete_train)[68] <- "subject"

complete_test <- cbind(x_test, y_test)
complete_test <- cbind(complete_test, subject_test)
colnames(complete_test)[67] <- "activity"
colnames(complete_test)[68] <- "subject"

complete_test[, "activity"] <- activity_labels[complete_test[,"activity"], "V2"]
complete_train[, "activity"] <- activity_labels[complete_train[,"activity"], "V2"]


-The train and test data sets are merged into the first complete tidy data set
tidy1 <- rbind(complete_test, complete_train)


-The final data set is built iterating for the tidy data set, and calculating the mean of the 66 variables for each activity, subject pair
tidy2 <- c("activity", names(tidy1))
tidy2 <- tidy2[!tidy2 %in% c("activity", "subject")]
tidy2 <- c("activity","subject",tidy2)

b <- data.frame(t(rep(NA,68)))
names(b) <- tidy2

for(i in 1:6){
        for(k in 1:30){
                a <- numeric()
                for(j in 1:66){
                        a <- c(a,mean(tidy1[tidy1[,"activity"]==activity_labels[i,"V2"] & tidy1[,"subject"]==k,j]))
                        #print(mean(tidy1[tidy1[,"activity"]==activity_labels[i,"V2"],j]))
                }
                print(a)
                a <- c(activity_labels[i,"V2"], k, a)
                b <- rbind(b,a)
        } 
}
b <- b[-1,]
write.table(b,"tidy.txt", srow.name=FALSE)





Codebook

Variables:

 [1] "activity”: label of the activity being measured      "subject”: integer representing the subject being measured                    
 [3] "tBodyAcc-mean()-X"           "tBodyAcc-mean()-Y"          
 [5] "tBodyAcc-mean()-Z"           "tBodyAcc-std()-X"           
 [7] "tBodyAcc-std()-Y"            "tBodyAcc-std()-Z"           
 [9] "tGravityAcc-mean()-X"        "tGravityAcc-mean()-Y"       
[11] "tGravityAcc-mean()-Z"        "tGravityAcc-std()-X"        
[13] "tGravityAcc-std()-Y"         "tGravityAcc-std()-Z"        
[15] "tBodyAccJerk-mean()-X"       "tBodyAccJerk-mean()-Y"      
[17] "tBodyAccJerk-mean()-Z"       "tBodyAccJerk-std()-X"       
[19] "tBodyAccJerk-std()-Y"        "tBodyAccJerk-std()-Z"       
[21] "tBodyGyro-mean()-X"          "tBodyGyro-mean()-Y"         
[23] "tBodyGyro-mean()-Z"          "tBodyGyro-std()-X"          
[25] "tBodyGyro-std()-Y"           "tBodyGyro-std()-Z"          
[27] "tBodyGyroJerk-mean()-X"      "tBodyGyroJerk-mean()-Y"     
[29] "tBodyGyroJerk-mean()-Z"      "tBodyGyroJerk-std()-X"      
[31] "tBodyGyroJerk-std()-Y"       "tBodyGyroJerk-std()-Z"      
[33] "tBodyAccMag-mean()"          "tBodyAccMag-std()"          
[35] "tGravityAccMag-mean()"       "tGravityAccMag-std()"       
[37] "tBodyAccJerkMag-mean()"      "tBodyAccJerkMag-std()"      
[39] "tBodyGyroMag-mean()"         "tBodyGyroMag-std()"         
[41] "tBodyGyroJerkMag-mean()"     "tBodyGyroJerkMag-std()"     
[43] "fBodyAcc-mean()-X"           "fBodyAcc-mean()-Y"          
[45] "fBodyAcc-mean()-Z"           "fBodyAcc-std()-X"           
[47] "fBodyAcc-std()-Y"            "fBodyAcc-std()-Z"           
[49] "fBodyAccJerk-mean()-X"       "fBodyAccJerk-mean()-Y"      
[51] "fBodyAccJerk-mean()-Z"       "fBodyAccJerk-std()-X"       
[53] "fBodyAccJerk-std()-Y"        "fBodyAccJerk-std()-Z"       
[55] "fBodyGyro-mean()-X"          "fBodyGyro-mean()-Y"         
[57] "fBodyGyro-mean()-Z"          "fBodyGyro-std()-X"          
[59] "fBodyGyro-std()-Y"           "fBodyGyro-std()-Z"          
[61] "fBodyAccMag-mean()"          "fBodyAccMag-std()"          
[63] "fBodyBodyAccJerkMag-mean()"  "fBodyBodyAccJerkMag-std()"  
[65] "fBodyBodyGyroMag-mean()"     "fBodyBodyGyroMag-std()"     
[67] "fBodyBodyGyroJerkMag-mean()" "fBodyBodyGyroJerkMag-std()" 
