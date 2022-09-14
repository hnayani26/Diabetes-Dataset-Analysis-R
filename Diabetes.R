#Hariom Nayani
#10470210
#I pledge my honor that I have abided by the Stevens Honor System

#Install necessary packages
install.packages("C50")
install.packages("randomForest")
install.packages("kknn")
library('C50')
library(randomForest)
library("kknn")

#Random Forest
#Clearing all data from RAM
rm(list=ls())

#Load the diabetes dataset from desktop into R
file<-file.choose()
mydata <- read.table("C:/Users/Hariom/Documents/Assignment 2/diabetes_012_health_indicators_BRFSS2015.csv", header=TRUE,sep=",", na.strings="?")
fiftyfifty <- read.table("C:/Users/Hariom/Documents/Assignment 2/diabetes_binary_5050split_health_indicators_BRFSS2015.csv", header=TRUE,sep=",", na.strings="?")

#Factoring the data
mydata$Diabetes_012 <- as.numeric(mydata$Diabetes_012)
mydata$Diabetes_012 <- as.factor(mydata$Diabetes_012)
fiftyfifty$Diabetes_binary <- as.numeric(fiftyfifty$Diabetes_binary)
fiftyfifty$Diabetes_binary <- as.factor(fiftyfifty$Diabetes_binary)

#Creating training and test data sets
mydata<-na.omit(mydata)
idx<-sample(nrow(mydata),as.integer(.4*nrow(mydata)))
training<-mydata[idx,]
test<-mydata[-idx,]

fiftyfifty<-na.omit(fiftyfifty)
ffidx<-sample(nrow(fiftyfifty),as.integer(.7*nrow(fiftyfifty)))
fftraining<-fiftyfifty[ffidx,]
fftest<-fiftyfifty[-ffidx,]

#Use the Random Forest methodology to develop a classification model for the Diagnosis and identify important features using the imbalanced dataset.
fit <- randomForest( Diabetes_012~., data=training, importance=TRUE, ntree=100)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test[,-1])

#Table comparing accuracy of model versus the actual results
table(actual=test[,1],Prediction)
wrong<- (test[,1]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Use the Random Forest methodology to develop a classification model for the Diagnosis and identify important features using the balanced dataset.
fit <- randomForest( Diabetes_binary~., data=fftraining, importance=TRUE, ntree=100)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, fftest[,-1])

#Table comparing accuracy of model versus the actual results
table(actual=fftest[,1],Prediction)
wrong<- (fftest[,1]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Use the Random Forest methodology to develop a classification model based off the balanced data set for the Diagnosis and identify important features in the unbalanced data set.
test$Diabetes_012 <- ifelse(test$Diabetes_012 == 0, 0, 1)
Prediction <- predict(fit, test[,-1])

table(actual=test[,1],Prediction)
wrong<- (test[,1]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Clearing all data from RAM
rm(list=ls())

#KNN
#Load the diabetes dataset from desktop into R
file<-file.choose()
mydata <- read.table("C:/Users/Hariom/Documents/Assignment 2/diabetes_012_health_indicators_BRFSS2015.csv", header=TRUE,sep=",", na.strings="?")
fiftyfifty <- read.table("C:/Users/Hariom/Documents/Assignment 2/diabetes_binary_5050split_health_indicators_BRFSS2015.csv", header=TRUE,sep=",", na.strings="?")

#Factoring the data
mydata$Diabetes_012 <- as.numeric(mydata$Diabetes_012)
mydata$Diabetes_012 <- as.factor(mydata$Diabetes_012)
fiftyfifty$Diabetes_binary <- as.numeric(fiftyfifty$Diabetes_binary)
fiftyfifty$Diabetes_binary <- as.factor(fiftyfifty$Diabetes_binary)

#Creating training and test data sets
mydata<-na.omit(mydata)
idx<-sample(nrow(mydata),as.integer(.25*nrow(mydata)))
training<-mydata[idx,]
test<-mydata[-idx,]

fiftyfifty<-na.omit(fiftyfifty)
ffidx<-sample(nrow(fiftyfifty),as.integer(.7*nrow(fiftyfifty)))
fftraining<-fiftyfifty[ffidx,]
fftest<-fiftyfifty[-ffidx,]

#Normalizing data
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}
for (i in 2:ncol(mydata)){
  mydata[,i] <- mmnorm(mydata[,i],min(mydata[,i]),max(mydata[,i] ))
}
for (i in 2:ncol(fiftyfifty)){
  fiftyfifty[,i] <- mmnorm(fiftyfifty[,i],min(fiftyfifty[,i]),max(fiftyfifty[,i] ))
}

#Using the knn methodology with k=3,5, and 10
predict_k3<- kknn(formula=Diabetes_012~., training, test[,-1], k=3, kernel="rectangular")
fit <- fitted(predict_k3, type="class")
table(Actual=test$Diabetes_012,Fitted=fit)

wrong<- (test[,1]!=fit )
error_rate<-sum(wrong)/length(wrong)
error_rate 

predict_k5<- kknn(formula=Diabetes_012~., training, test[,-1], k=5, kernel="rectangular")
fit <- fitted(predict_k5, type="class")
table(Actual=test$Diabetes_012,Fitted=fit)

wrong<- (test[,1]!=fit )
error_rate<-sum(wrong)/length(wrong)
error_rate 

predict_k10<- kknn(formula=Diabetes_012~., training, test[,-1], k=10, kernel="rectangular")
fit <- fitted(predict_k10, type="class")
table(Actual=test$Diabetes_012,Fitted=fit)

wrong<- (test[,1]!=fit )
error_rate<-sum(wrong)/length(wrong)
error_rate 

predict_k3<- kknn(formula=Diabetes_binary~., fftraining, fftest[,-1], k=3, kernel="rectangular")
fit <- fitted(predict_k3, type="class")
table(Actual=fftest$Diabetes_binary,Fitted=fit)

wrong<- (fftest[,1]!=fit )
error_rate<-sum(wrong)/length(wrong)
error_rate 

predict_k5<- kknn(formula=Diabetes_binary~., fftraining, fftest[,-1], k=5, kernel="rectangular")
fit <- fitted(predict_k5, type="class")
table(Actual=fftest$Diabetes_binary,Fitted=fit)

wrong<- (fftest[,1]!=fit )
error_rate<-sum(wrong)/length(wrong)
error_rate 

predict_k10<- kknn(formula=Diabetes_binary~., fftraining, fftest[,-1], k=10, kernel="rectangular")
fit <- fitted(predict_k10, type="class")
table(Actual=fftest$Diabetes_binary,Fitted=fit)

wrong<- (fftest[,1]!=fit )
error_rate<-sum(wrong)/length(wrong)
error_rate 

#Clearing all data from RAM
rm(list=ls())
