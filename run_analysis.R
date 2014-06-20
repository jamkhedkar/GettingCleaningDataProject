# merge train and test files.
run_analysis <- function(){
  
  # reading variable names
  features <- read.table("./UCI HAR Dataset/features.txt",colClasses="character",quote="")
  
  # reading training files
  X_train <- read.table("./UCI HAR Dataset/train/X_train.txt",colClasses="numeric",skip=0)
  
  
  y_train <- read.table("./UCI HAR Dataset/train/y_train.txt",colClasses="numeric",skip=0)
  
  
  subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",colClasses="numeric",skip=0)
  
  numColX_train = dim(X_train)[[2]]
  
  #Add subject_train and y_train as last and second last columns to X_train before merging
  
  X_train <-cbind(X_train,subject_train,y_train)
  
  #Rename X_train
  
  names(X_train)<- 1:(numColX_train+2)
  
  # readinf test files
  X_test <- read.table("./UCI HAR Dataset/test/X_test.txt",colClasses="numeric",skip=0)
  subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",colClasses="numeric",skip=0)
  y_test <- read.table("./UCI HAR Dataset/test/y_test.txt",colClasses="numeric",skip=0)
  
  
  numColX_test <- dim(X_test)[[2]]
  
  #Add subject_test and y_test as last and second last columns to X_test before merging
  
  X_test <-cbind(X_test,subject_test,y_test)
  
  
  
  names(X_test)<- 1:(numColX_test+2)
  
  # Merging training and test sets
  
  X <- merge(X_train,X_test,all=TRUE)
  
  # naming the last two columns
  names(X[numColX_test+1]) <- "subject"
  names(X[numColX_test+2]) <- "activity"
  
  
  #---------------------------------------------------------------------------------------------
  
  # Open file features to get cols with mean and std of various measured quantities
  
  # selecting mean and std variables
  
  measuredMean <- grep("mean",features[[2]],value=FALSE)
  
  measuredStd <- grep("std",features[[2]],value=FALSE)
  
  
  
  namesIndex <- sort(c(measuredMean,measuredStd))
  featureNames <- namesIndex
  len <- length(namesIndex)
  namesIndex <- c(namesIndex,numColX_train+1,numColX_train+2)
  #print(namesIndex)
  XMeanStd <- X[namesIndex]
  
  names(XMeanStd) <- features[[2]][featureNames]
  names(XMeanStd) <- gsub("()","",names(XMeanStd),fixed=TRUE)
  names(XMeanStd)[[len+1]] <- "subject"
  names(XMeanStd)[[len+2]] <- "activity"
  
  # moving the subject and activity columns to the front
  XMeanStd <- XMeanStd[,c(len+1,len+2,1:len)]
  
  # sorting in terms of subject ids
  XMeanStd <- XMeanStd[order(XMeanStd$subject),]
  
  XDataProject <- XMeanStd 
  
  
  #--------------------------------------------------------------------------------------------------
  #Replace activity number by name
  
  XDataProject$activity <- sub("1","Walking",XDataProject$activity)
  XDataProject$activity <- sub("2","Walking_Upstairs",XDataProject$activity)
  XDataProject$activity <- sub("3","Walking_Downstairs",XDataProject$activity)
  XDataProject$activity <- sub("4","Sitting",XDataProject$activity)
  XDataProject$activity <- sub("5","Standing",XDataProject$activity)
  XDataProject$activity <- sub("6","Laying",XDataProject$activity)
 
  write.table(XDataProject,file="./UCI HAR Dataset/TidyDataPart1.txt",append=FALSE,sep=" ",eol="\n",col.names=TRUE,row.names=FALSE,quote=FALSE)
  
  #--------------------------------------------------------------------------------------------------
  
  # Part 2
  #Create new data frame
  
 # factoring on of subject
 
   Xnew <- split(XMeanStd,XMeanStd$subject)
  
 # no. of subjects
   noSub <- length(Xnew)
  
  #print(noSub)
  for (i in 1:noSub) {
    
    XnoSub <- (Xnew[[i]])
    
    # splitting each subject further an basis of activity
    XnosubAct <- split(XnoSub,XnoSub$activity)
    
    
      for(j in 1:length(XnosubAct)){
        Xa <- as.matrix(XnosubAct[[j]])
        
      # colMean will find the mean of each variable for a given activity for a given subject  
        
        if (j == 1)
        {
          XaMean <- colMeans(Xa)
        }
        else{
          XaMean <- rbind(XaMean,colMeans(Xa))
        }
       
        
      } #j
    
  # Writing to new data file
    if(i==1){
      write.table(XaMean,file="./UCI HAR Dataset/TidyDataPart2.txt",append=FALSE,sep=" ",eol="\n",col.names=TRUE,row.names=FALSE,quote=FALSE)
    }
    else
      {
    write.table(XaMean,file="./UCI HAR Dataset/TidyDataPart2.txt",append=TRUE,sep=" ",eol="\n",col.names=FALSE,row.names=FALSE,quote=FALSE)  
  }
  }#i
 
}
