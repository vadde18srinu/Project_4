setwd("F:/AcadGild/workings")

lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car","lubridate",
      "zoo", "sqldf", "fuzzyjoin", "party", "mice", "tseries", "timeSeries","forecast","tidyverse")
      sapply(lib, require, character.only=TRUE, quietly=TRUE)

CancerData<-fread("F:/AcadGild/workings/Project4_DiseasePrediction/CancerData.csv", sep=",", header = TRUE)

#Exploratory Analysis  



# Missing Values verification
sapply(CancerData, function(x) sum(is.na(x))) # no missing values 
colSums(is.na(CancerData))

# Correlation and VIF 
cor(CancerData[,3:32])
library(psych)
cor.ci(CancerData[,3:32], method = "spearman")

library(usdm)
vif(CancerData[,3:32])

#outlier detection
boxplot(CancerData[,3:32])

boxplot(CancerData$area_mean)
qnt <- quantile(CancerData$area_mean, 0.75, na.rm = T)
caps <- quantile(CancerData$area_mean, 0.95, na.rm = T)
H <- 1.5 * IQR(CancerData$area_mean, na.rm = T)
CancerData$area_mean[CancerData$area_mean > (qnt +  H)] <- caps

boxplot(CancerData$area_worst)
qnt <- quantile(CancerData$area_worst, 0.75, na.rm = T)
caps <- quantile(CancerData$area_worst, 0.95, na.rm = T)
H <- 1.5 * IQR(CancerData$area_worst, na.rm = T)
CancerData$area_worst[CancerData$area_worst > (qnt +  H)] <- caps

boxplot(CancerData$texture_se)
qnt <- quantile(CancerData$texture_se, 0.75, na.rm = T)
caps <- quantile(CancerData$texture_se, 0.95, na.rm = T)
H <- 1.5 * IQR(CancerData$texture_se, na.rm = T)
CancerData$texture_se[CancerData$texture_se > (qnt +  H)] <- caps

table(CancerData$diagnosis)
class(CancerData$diagnosis)
# encoding categorical variables
CancerData$diagnosis<-ifelse(CancerData$diagnosis=="M",1,0)
CancerData$diagnosis<-factor(CancerData$diagnosis, levels = c(0,1))
table(CancerData$diagnosis)

CData<-CancerData[,2:32]
class(CData)
sapply(CData,class)

# data partition 
set.seed(2222)
ind<-sample(2, nrow(CData),replace = TRUE, prob = c(0.70, 0.30))
Ctrain<-CData[ind==1,]
Ctest<-CData[ind==2,]

#Building Logistic Model
logitmodel<-glm(diagnosis~., data=Ctrain,family = "binomial")
summary(logitmodel)
plot(logitmodel)
attributes(logitmodel)

predictedY<-predict(logitmodel,Ctest, type="response")

summary(predictedY)
attributes(predictedY)

#record factors
y_pred_num<-ifelse(predictedY>0.5,1,0)
y_pred_num
y_pred<-factor(y_pred_num, levels = c( 0,1))
y_pred
y_act<-Ctest$diagnosis
y_act

table(y_act) #66-Milignant, 97-Benign

 #Accuracy
mean(y_pred==y_act) #95%

library(pROC)
g<-roc(diagnosis~predictedY, data = Ctest)
plot(g)
attributes(g)
g$auc


#Random Forest Model
library(randomForest)
Ctrain$diagnosis<-as.factor(Ctrain$diagnosis)
summary(Ctrain$diagnosis)
set.seed(555)
RF<-randomForest(x=Ctrain, y=Ctrain$diagnosis)
summary(RF)
attributes(RF)
plot(RF)

Ctest<-data.frame(Ctest)

pred<-predict(RF,Ctrain, type = "response")
head(pred)
plot(pred)

predt<-table(pred)
View(predt)


