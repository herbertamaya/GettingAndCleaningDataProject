createTidyDataSet <- function(){
        library(dplyr)
        #Defining directories and files
        projectUrl <- "UCI HAR Dataset"
        file.test.X.url <- paste(projectUrl,"/test/X_test.txt",sep="")
        file.train.X.url <- paste(projectUrl,"/train/X_train.txt",sep="")
        file.test.y.url <- paste(projectUrl,"/test/y_test.txt",sep="")
        file.train.y.url <- paste(projectUrl,"/train/y_train.txt",sep="")
        file.test.y.url <- paste(projectUrl,"/test/y_test.txt",sep="")
        file.train.subject.url <- paste(projectUrl,"/train/subject_train.txt",sep="")
        file.test.subject.url <- paste(projectUrl,"/test/subject_test.txt",sep="")
        file.activity.url <- paste(projectUrl,"/activity_labels.txt",sep="")
        file.features.url <- paste(projectUrl,"/features.txt",sep="")
        
        #reading the files
        data.train.X <- read.csv(file.train.X.url,header=FALSE,"")
        data.train.y <- read.csv(file.train.y.url,header=FALSE,"")
        data.train.subject <-read.csv(file.train.subject.url,header=FALSE,"")
        data.test.X <- read.csv(file.test.X.url,header=FALSE,"")
        data.test.y <- read.csv(file.test.y.url,header=FALSE,"")
        data.test.subject <-read.csv(file.test.subject.url,header=FALSE,"")
        
        #Merging training and test sets
        data.X <- rbind.data.frame(data.train.X,data.test.X)
        data.y <- rbind.data.frame(data.train.y,data.test.y)
        data.subject <-rbind.data.frame(data.train.subject,data.test.subject)
        
        #Extract only mean and str measures
        data.features <- read.csv(file.features.url,header=FALSE,"")
        colnames(data.features) <- c("id_features","name_features")
        data.features <- data.features[grepl("(mean\\(\\))|(std\\(\\))",data.features$name_features),]
        data.X <- data.X[,data.features$id_features]
        
        #Use descriptive activity names to name the activities
        data.activity.labels <- read.csv(file.activity.url,header = FALSE,"")
        colnames(data.activity.labels) <- c("id_activity","activity_name")
        colnames(data.y) <- c("id_activity")
        data.y <- merge(data.y,data.activity.labels,by="id_activity")
        data.y <- data.y[c("activity_name")]
        
        #label the data set with descriptive variable names
        colnames(data.X) <- data.features[,2]
        colnames(data.subject)<- c("subject_id")
        
        #Data set with the average of each variable for each activity and each subject
        #Step 1: merge data.y with data.x and data.subject
        data.all <- cbind.data.frame(data.y,data.subject,data.X)
        #Step 2: create a new data.frame and group_by()
        activity.subject <- group_by(data.all,activity_name,subject_id)
        tidy.data <- summarise_each(activity.subject,funs(mean))
        tidy.data <- arrange(tidy.data,activity_name,subject_id)
        
        #Write the tidy data set to a txt file
        write.table(tidy.data,"tidy_data_set.txt",row.names =FALSE)
}



