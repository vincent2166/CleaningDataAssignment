#Merges the training and test sets to create one data set.

#Import data from test and merge together
setwd("test")
test.value<-read.table("X_test.txt")
test.act<-read.table("y_test.txt")
test.id<-read.table("subject_test.txt")
test<-cbind(test.id,test.act,test.value)
colnames(test)[c(1,2)]<-c("ID","Activity")

#Import data from train and merge together
setwd("../train")
train.value<-read.table("X_train.txt")
train.act<-read.table("y_train.txt")
train.id<-read.table("subject_train.txt")
train<-cbind(train.id,train.act,train.value)
colnames(train)[c(1,2)]<-c("ID","Activity")

#Merge the test and train datasets together
data<-rbind(test,train)

#Extracts only the measurements on the mean and standard deviation for each measurment.

#Create a vector that distinguishes those measurements for mean and standard deviation from
#those that aren't.
setwd("../")
var<-read.table("features.txt")
mean.std.vector<-grepl('mean\\(\\)|std\\(\\)',var[,"V2"])

#Create a vector for columns that contain mean and standard deviation measurements
mean.std<-which(mean.std.vector %in% 1)
mean.std<-mean.std+2
mean.std<-append(c(1,2),mean.std)

#Extract only measuremants that contain mean and standard deviation
data<-data[,mean.std]

#Uses descriptive activity names to name the activities in the data set

#Create a vector of activities from the data set and merge with the activity labels
#to match the activity names.
act<-read.table("activity_labels.txt")
act.names<-as.data.frame(data[,2])
act.names<-as.data.frame(merge(act.names,act,by.x="data[, 2]",by.y="V1")[,2])

#Join vector to data set and remove the old activity column
data<-cbind(data[1],act.names,data[3:68])

#Appropiatelly labels the data set with descriptive variable names.

#Create a vector with the new column names
mean.std.vector<-as.data.frame(mean.std[3:68]-2)
colnames(mean.std.vector)<-"index"
new.colnames<-merge(mean.std.vector,var,by.x="index",by.y="V1")
new.colnames<-rbind(data.frame("index"=c("0","1"),"V2"=c("ID","Activity")),new.colnames)[,2]

#Rename the columns with the new names
colnames(data)<-new.colnames
data[order(data,data$ID),]

#Creates a second, independent tidy data set with the average of each variable for
#each activity and each subject
library(reshape2)
data.melt<-melt(data,id=1:2,measre.vars=3:68)
data.cast<-dcast(data.melt, ID + Activity ~ variable ,mean)