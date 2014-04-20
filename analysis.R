rm(list=objects())

library(stringr)
source('read.data.txt')

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",'getdata-projectfiles-UCI HAR Dataset.zip',method="curl")
unzip("getdata-projectfiles-UCI HAR Dataset.zip")

if(file.exists("UCI HAR Dataset")){
	activity_labels   <- read.table('UCI HAR Dataset/activity_labels.txt')$V2
	subject           <- rbind(read.table("UCI HAR Dataset/test/subject_test.txt"),read.table("UCI HAR Dataset/train/subject_train.txt"))
	activity          <- rbind(read.table("UCI HAR Dataset/test/y_test.txt")      ,read.table("UCI HAR Dataset/train/y_train.txt"))
	activity          <- factor(activity$V1,labels=activity_labels)
	colnames(subject) <- "Subject"
	cnames<- read.table("UCI HAR Dataset/features.txt",header=FALSE,stringsAsFactors=FALSE)[,2]
	data  <- rbind(read.data.txt("UCI HAR Dataset/test/X_test.txt"," +",cnames=cnames),read.data.txt("UCI HAR Dataset/train/X_train.txt"," +",cnames=cnames))
	
	data<-cbind(subject,activity,data[,sort(c(grep('mean',cnames),grep('std',cnames),grep('Mean',cnames)))])
	write.table(data,'dados.csv',sep=',',dec='.',col.names=TRUE,row.names=FALSE) #for Protugal sep=';' and dec=','
	
	incrment <- length(activity_labels) 
	tidydata<-data.frame(matrix(nrow=(incrment*length(unique(data$Subject))),ncol=ncol(data)))
	k <- 1
	for(i in sort(unique(data$Subject))){
		dados<-matrix(nrow=incrment,ncol=(ncol(data)-2))	
		for(j in 3:ncol(data)){	
			auxVector <- tapply(data[i==data$Subject,j],data[i==data$Subject,2],mean)
			dados[,j-2]     <- auxVector   
		}
		subject   <- rep(i,length(auxVector))
		activity  <- names(auxVector)
		tidydata[k:(k+incrment-1),1]<-subject
		tidydata[k:(k+incrment-1),2]<-activity
		tidydata[k:(k+incrment-1),3:ncol(data)]<-dados	
		k <- k + incrment
    }
	colnames(tidydata)<-c('Subject','Activity',paste('Average_of',colnames(data)[3:length(colnames(data))],sep='_'))
	write.table(tidydata,'tidydata.txt',sep='\t',dec='.',col.names=TRUE,row.names=FALSE) 
}
