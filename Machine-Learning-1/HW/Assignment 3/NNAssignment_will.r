
set.seed(20201116)
DataOrig <- read.table("spambasedata-Orig.csv",sep=",",header=T,
                       stringsAsFactors=F)

ord <- sample(nrow(DataOrig))
DataOrig <- DataOrig[ord,] #re-order

# Change IsSpam to a factor

DataOrig$IsSpam <- factor(DataOrig$IsSpam)

# Doing a 60-20-20 split
TrainInd <- ceiling(nrow(DataOrig)*0.6)
TrainDF <- DataOrig[1:TrainInd,]
tmpDF <- DataOrig[-(1:TrainInd),]
ValInd <- ceiling(nrow(tmpDF)*0.5)
ValDF <- tmpDF[1:ValInd,]
TestDF <- tmpDF[-(1:ValInd),]

remove(TrainInd,tmpDF,ValInd,ord)



# Question 1: Stepwise Logistic Regression -------------------------------------------

SmallFm <- IsSpam ~ 1
Vars <- names(TrainDF)
BigFm <- paste(Vars[58],"~",paste(Vars[1:57],collapse=" + "),sep=" ")
BigFm <- formula(BigFm)

# Run stepwise logistic regression
OutSmall <- glm(SmallFm,data=TrainDF,family=binomial)
summary(OutSmall)

OutBig <- glm(BigFm,data=TrainDF,family=binomial)
summary(OutBig)

sc <- list(lower=SmallFm,upper=BigFm) # stepwise control
out <- step(OutSmall,scope=sc,direction="both") #both direction: try both forward(+) and backward(-)
summary(out) #when 'none' has smallest AIC then we got final model

# Compute the predicted probabilities for both validation and test data
prd_val <- predict.glm(out, newdata = ValDF, type = 'response') #To get predicted probability, add the argument type="response"
prd_test <- predict.glm(out, newdata = TestDF, type = 'response')

# Plot ROC curve and calculate AUC for both validation and test data
source("RocPlot.r")
ROCPlot(prd_val,ValDF$IsSpam)
ROCPlot(prd_test,TestDF$IsSpam)



# Question 2: Random Forest -------------------------------------------------------------------

if(!require("randomForest")) { install.packages("randomForest"); require("randomForest") }

# Run Random Forest
out2 <- randomForest(BigFm,data=TrainDF,ntree=1000)

# Compute the predicted probabilities for both validation and test data
prd_val_2 <- predict(out2, newdata = ValDF, type = 'prob') # type = 'prob' gives matrix of class probabilities
prd_test_2 <- predict(out2, newdata = TestDF, type = 'prob')

# Plot ROC curve and calculate AUC for both validation and test data
ROCPlot(prd_val_2[,2],ValDF$IsSpam) #use target=1 for plotting
ROCPlot(prd_test_2[,2],TestDF$IsSpam)



# Question 3: Wide, Shallow Neural Net --------------------------------------------------------------

# Write out the data for the neural net models

write.table(TrainDF,file="NNHWTrain.csv",sep=",",row.names=F,col.names=T)
write.table(ValDF,file="NNHWVal.csv",sep=",",row.names=F,col.names=T)
write.table(TestDF,file="NNHWTest.csv",sep=",",row.names=F,col.names=T)

#run Neural Net in Python...

# Read in the neural net output and compute the AUC for the validation data.
SpamNNWideTrainOutput <- read.table("SpamNNWideTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideValOutput <- read.table("SpamNNWideValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNWideTestOutput <- read.table("SpamNNWideTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

# Plot ROC curve and calculate AUC for both validation data
names(SpamNNWideValOutput)
ROCPlot(SpamNNWideValOutput$ValP,SpamNNWideValOutput$IsSpam)
ROCPlot(SpamNNWideTestOutput$TestP,SpamNNWideTestOutput$IsSpam)

# Question 4: Narrow, Deep Neural Net --------------------------------------------------------------

#run Neural Net in Python...

# Read in the neural net output and compute the AUC for the validation data.

SpamNNDeepTrainOutput <- read.table("SpamNNNarrowTrainDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNDeepValOutput <- read.table("SpamNNNarrowValDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)
SpamNNDeepTestOutput <- read.table("SpamNNNarrowTestDFOutput.csv",header=T,sep=",",quote=NULL,stringsAsFactors = F)

# Plot ROC curve and calculate AUC for both validation data
names(SpamNNDeepValOutput)
ROCPlot(SpamNNDeepValOutput$ValP,SpamNNDeepValOutput$IsSpam)
ROCPlot(SpamNNDeepTestOutput$TestP,SpamNNDeepTestOutput$IsSpam)
