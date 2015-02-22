download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="./data.zip",method="curl")
unzip("./data.zip")
feature_labels = read.table("./UCI HAR Dataset/features.txt", sep=" ",stringsAsFactors=FALSE)
activity_labels = read.table("./UCI HAR Dataset/activity_labels.txt", sep=" ",stringsAsFactors=FALSE)
subject_test = read.table("./UCI HAR Dataset/test/subject_test.txt", sep=" ",stringsAsFactors=FALSE)
x_test = read.table("./UCI HAR Dataset/test/X_test.txt",stringsAsFactors=FALSE)
y_test = read.table("./UCI HAR Dataset/test/y_test.txt", sep=" ",stringsAsFactors=FALSE)
subject_train = read.table("./UCI HAR Dataset/train/subject_train.txt", sep=" ",stringsAsFactors=FALSE)
x_train = read.table("./UCI HAR Dataset/train/X_train.txt",stringsAsFactors=FALSE)
y_train = read.table("./UCI HAR Dataset/train/y_train.txt", sep=" ",stringsAsFactors=FALSE)

filtered_column_indexes <- sort.int(c(feature_labels[grepl("std\\(",feature_labels$V2),1],feature_labels[grepl("mean\\(",feature_labels$V2),1]))
filtered_colum_names <- feature_labels[filtered_column_indexes,2]

x_test <- x_test[,filtered_column_indexes]
x_train <- x_train[,filtered_column_indexes]

colnames(x_test) <- filtered_colum_names
colnames(x_train) <- filtered_colum_names

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

tidy1 <- rbind(complete_test, complete_train)

#mean by activity
tidy2 <- c("activity", names(tidy1))
tidy2 <- tidy2[!tidy2 %in% c("activity", "subject")]
tidy2 <- c("activity",tidy2)

b <- data.frame(t(rep(NA,67)))
names(b) <- tidy2
        
for(i in 1:6){
        a <- numeric()
        for(j in 1:66){
                a <- c(a,mean(tidy1[tidy1[,"activity"]==activity_labels[i,"V2"],j]))
                #print(mean(tidy1[tidy1[,"activity"]==activity_labels[i,"V2"],j]))
        }

        a <- c(activity_labels[i,"V2"], a)
        print(a)
        b <- rbind(b,a)
}
b <- b[-1,]


#mean by subject
tidy2 <- c("activity", names(tidy1))
tidy2 <- tidy2[!tidy2 %in% c("activity", "subject")]
tidy2 <- c("subject",tidy2)

b <- data.frame(t(rep(NA,67)))
names(b) <- tidy2

for(i in 1:30){
        a <- numeric()
        for(j in 1:66){
                a <- c(a,mean(tidy1[tidy1[,"subject"]==i,j]))
                #print(mean(tidy1[tidy1[,"activity"]==activity_labels[i,"V2"],j]))
        }
        
        a <- c(i, a)
        print(a)
        b <- rbind(b,a)
}
b <- b[-1,]


#mean by activity and subject
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