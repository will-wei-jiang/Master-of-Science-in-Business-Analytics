
AllData <- read.table("C:/Users/Will Jiang/Desktop/Emory Desktop/ML/Individual Assignment/Assignment 1/GradedHW1-All-Data.csv",
                      header=T,sep=",",
                      stringsAsFactors = F,na.strings="")

AllData <- AllData[AllData$Bldg.Type=="1Fam",]

RPerm <- sample(nrow(AllData))
AllData <- AllData[RPerm,]

TrainInd <- ceiling(nrow(AllData)/2)
ValInd <- ceiling((nrow(AllData)-TrainInd)/2)+TrainInd

TrainData <- AllData[1:TrainInd,]
ValData <- AllData[(TrainInd+1):ValInd,]
TestData <- AllData[(ValInd+1):nrow(AllData),]
