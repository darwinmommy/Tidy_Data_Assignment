#Download and unzip the required files
filename<-"UCI Data.zip"
if(!file.exists(filename)){
    URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(URL,filename)
}
if(file.exists("UCI Data.zip")){
    unzip(filename)
}

#Read the files required to construct the "Test" set, subjects and measurement type
TestMeasure<-read.csv("y_test.txt",header=FALSE)
TestSubjects<-read.csv("subject_test.txt",header=FALSE)

#Read the features.txt file, removing the numbers and unlisting to create a character vector
colnames<-read.csv("features.txt",header=FALSE,stringsAsFactors = FALSE,sep=" ")
colnames<-colnames[2]
colnames<-unlist(colnames)

#Extract only measurements with mean or SD 
MeanSD<-grep(".*mean.*|.*std.*",colnames)
MeanSD.names<-colnames[MeanSD]
MeanSD.names = gsub("-mean","Mean",MeanSD.names)
MeanSD.names = gsub ("-std","Std",MeanSD.names)
MeanSD.names = gsub("[-()]","",MeanSD.names)


#Read the X_test.txt file (contains data values) and assign the column names using the character vector created above from the 
##features.txt file
TestData<-read.table("X_test.txt",header=FALSE,stringsAsFactors = FALSE,as.is = T)
colnames(TestData)=MeanSD.names

#Compile the Test set, assign names to the two first columns
#Create a new column to identify the assoicated subject as part of the "Test" set
TestCompiled<-cbind(TestSubjects,TestMeasure)
colnames(TestCompiled)<-c("SubjectID","Activity")
library(plyr)
TestCompiled<-mutate(TestCompiled,Group="Test")
Test<-cbind(TestCompiled,TestData)

#As above, read the files required to construct the "Train" set, subjects and measurement type
TrainMeasure<-read.csv("y_train.txt",header=FALSE)
TrainSubjects<-read.csv("subject_train.txt",header=FALSE)

#As above, read the X_train file (contains data values) and assign column names using the character vector created above from the
##features.txt file
TrainData<-read.table("X_train.txt",header=FALSE,stringsAsFactors = FALSE,as.is = T)
colnames(TrainData)=MeanSD.names

#As above, compile the Train set, assign names to the first two columns
#As above, create a new column to identify the associated subject as part of the "Train" set
TrainCompiled<-cbind(TrainSubjects,TrainMeasure)
colnames(TrainCompiled)<-c("SubjectID","Activity")
library(plyr)
TrainCompiled<-mutate(TrainCompiled,Group="Train")
Train<-cbind(TrainCompiled,TrainData)

#Create a merged data set that includes all "Test" and "Train" data, subsets columns 1-82 to include only variables with Mean or Std measurements
Merged<-rbind(Train,Test)
Merged = Merged[,1:82]

#Rename activities according to code book (I tried condensing this, couldn't make it work)
Merged$Activity<-as.character(Merged$Activity)
Merged$Activity<-gsub(1,"Walking",Merged$Activity) 
Merged$Activity<-gsub(2,"Upstairs",Merged$Activity)
Merged$Activity<-gsub(3,"Downstairs",Merged$Activity)
Merged$Activity<-gsub(4,"Sitting",Merged$Activity)
Merged$Activity<-gsub(5,"Standing",Merged$Activity)
Merged$Activity<-gsub(6,"Lying",Merged$Activity)

#Change subjects from integer to factor, prints structure of merged data set
Merged$SubjectID<-factor(Merged$SubjectID)
Merged$Activity<-factor(Merged$Activity)
str(Merged)





#Melt dataset and calculate the mean of each variable based on subject number and activity
library(reshape2)
Merged.Melt<-Merged[,-which(names(Merged)=="Group")]
Merged.Melt<- melt(Merged.Melt,id=c("SubjectID", "Activity"))
Merged.Mean<- dcast(Merged.Melt,SubjectID + Activity ~variable, mean)

#Print the resulting data table
Merged.Mean





