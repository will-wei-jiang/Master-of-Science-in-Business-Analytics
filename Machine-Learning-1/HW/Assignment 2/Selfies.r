# Answer is correct

# Read in the data
FileName <- "SelfieImageData-Final.csv"
Labs <- scan(file=FileName,what="xx",nlines=1,sep="|")
DataAsChars <- matrix(scan(file=FileName,what="xx",sep="|",skip=1),byrow=T,ncol=length(Labs))
colnames(DataAsChars) <- Labs
dim(DataAsChars)
# size in memory in MBs
as.double(object.size(DataAsChars)/1024/1024)

ImgData <- matrix(as.integer(DataAsChars[,-1]),nrow=nrow(DataAsChars))
colnames(ImgData) <- Labs[-1]
rownames(ImgData) <- DataAsChars[,1]
# size in memory in MBs
as.double(object.size(ImgData)/1024/1024)

# Take a look
ImgData[1:8,1:8]

# Free up some memory just in case
remove(DataAsChars)

# Show each Image
for(whImg in 1:nrow(ImgData)) {
  Img <- matrix(ImgData[whImg,],byrow=T,ncol=sqrt(ncol(ImgData)))
  Img <- apply(Img,2,rev)
  par(pty="s",mfrow=c(1,1))
  image(z=t(Img),col = grey.colors(255),useRaster=T)
  Sys.sleep(1)
}

#Q2: Average Face----------------------------------------------
AvgData <- apply(ImgData,2,FUN=mean)
AvgData <- matrix(AvgData,byrow=T,sqrt(ncol(ImgData)))
AvgData <- apply(AvgData,2,rev)
par(pty="s",mfrow=c(1,1))
image(z=t(AvgData),col = grey.colors(255),useRaster=T)
title("WJIAN63: Average Face")

#Q5:Scree Plot---------------------------------------------
#centralize
ImgData_new <- sweep(ImgData,2,STATS = apply(ImgData,2,mean),FUN = '-')

#normalize
#library(caret)
#process <- preProcess(ImgData_new, method=c("range"))
#ImgData_new <- predict(process, ImgData_new)

#trick: use small dimension(sample size = 52) to perform decomposition
SDecomp <- eigen(ImgData_new %*% t(ImgData_new))
EigenValues <- SDecomp$values/(52-1)
EigenVectors <- t(ImgData_new) %*% SDecomp$vectors
EigenVectors <- sweep(EigenVectors,2,sqrt(colSums(EigenVectors**2)), FUN = '/') #re-scale vectors so that they have length 1

par(mfrow=c(1,1))
plot(EigenValues)
title("WJIAN63: Scree Plot")

#Q6--------------------------------------------------------
max(EigenValues)

#Q7: How many for 85% of the variance----------------------
sum(cumsum(EigenValues)/sum(EigenValues) < 0.85) + 1
sum(EigenValues[1:1])/sum(EigenValues)
sum(EigenValues[1:2])/sum(EigenValues)
sum(EigenValues[1:25])/sum(EigenValues)

#Q8: My Face 20D------------------------------------------
PComp20d <- ImgData_new %*% EigenVectors[,1:20] #20 Principle Component
dim(PComp20d)
Recon20d <- PComp20d %*% t(EigenVectors[,1:20])

par(mfrow=c(1,1),pty="s")
ImageData2 <- matrix(Recon20d['Wei (Will) Jiang ',],byrow=T,sqrt(ncol(ImgData)))
ImageData2 <- apply(ImageData2,2,rev)
image(z=t(ImageData2),col = grey.colors(255),useRaster=T)
title("WJIAN63: MyFace 20D")

#Q10,Q11: Eigen Face------------------------------------------------
# Graph weights - First EigenVector
for (i in 1:20){
par(pty="s",mfrow=c(1,1))
vec <- EigenVectors[,i]
vec <- (vec-min(vec))/(max(vec)-min(vec))
vec <- vec*255
vecImage <- matrix(vec,byrow=T,sqrt(ncol(ImgData)))
vecImage <- t(apply(vecImage,2,rev))
image(z=vecImage,col = grey.colors(255),useRaster=T)
title('EigenVector',i)
}

#Eigenface 8
par(pty="s",mfrow=c(1,1))
vec <- EigenVectors[,8]
vec <- (vec-min(vec))/(max(vec)-min(vec))
vec <- vec*255
vecImage <- matrix(vec,byrow=T,sqrt(ncol(ImgData)))
vecImage <- t(apply(vecImage,2,rev))
image(z=vecImage,col = grey.colors(255),useRaster=T)
title('WJIAN63: Eigenface 8')
