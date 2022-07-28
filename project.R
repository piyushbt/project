dataset1<- read.csv(file.choose())
print(dataset1)
head(dataset1)
str(dataset1)
summary(dataset1)
library(knitr)
library(lattice)
library(tidyverse)
library(likert)
library(MASS)
library(psych)
library(viridis)
library(ggplot2)
library(here)
library(flextable)
library(devtools)
library(car)
library(ggord)
scat1<- scatterplotMatrix(dataset1[2:6])
scat2<- scatterplotMatrix(dataset1[7:11])
scat3<- scatterplotMatrix(dataset1[12:17])
scat4<- scatterplotMatrix(dataset1[18:22])

plot(dataset1$Diabetes_011)
plot(dataset1$HighBP)
plot(dataset1$MentHlth)
#data divide into 2 part

set.seed(111)
ind <- sample(2,nrow(dataset1),replace = TRUE,prob = c(0.9, 0.1))
training1 <- dataset1[ind==1,]
testing2 <- dataset1[ind==2,]
as.data.frame(training1)
as.data.frame(testing2)


#LDA

lda2<-lda(training1$Diabetes_011 ~ .,data=training1)
(summary(lda2))
lda2
plot(lda2)
ggord(lda2, training1$Diabetes_012, ylim = c(-10, 15))
ggplot(aes(training1$Diabetes_012 )~aes(.), data = training1, method = "lda")
p <- ggplot(data=lda2, aes(x=LD1,y=LD2,col=Diabetes_012)) + geom_point() + geom_text(aes(label = row_num))
p1 <- predict(lda2, training1)$class
tab <- table(Predicted = p1, Actual = training1$Diabetes_011)
tab
sum(diag(tab))/sum(tab)
p2 <- predict(lda2, testing2)$class
tab1 <- table(Predicted = p2, Actual = testing2$Diabetes_011)
tab1
sum(diag(tab1))/sum(tab1)

# Machine learning models

# Random forest

install.packages("randomForest")
install.packages("party")
library(randomForest)
library(party)
rt<-randomForest(training1$Diabetes_011 ~ . ,data=training1,method="classification",metric="accuracy",trControl = trControl)
attributes(rt)
summary(rt)
plot(rt)
predrt<-predict(rt,training1)
predrt
plot(predrt)
predicrt<-ifelse(predrt>0.5,'1','0')
predicrt
accuracyrt=mean(predicrt== testing2$Diabetes_011)
accuracyrt

#Decsion tree

library(party)
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)
fit <- rpart(training1$Diabetes_011~., data = training1)
fit
attributes(fit)
fit$variable.importance
rpart.plot(fit)
preddss<-predict(fit,training1,type = 'class')
preddss
predic2<-ifelse(preddss>0.5,'1','0')
predic2
mat<-table(training1$Diabetes_011,predic2)
mat
accuracydt<-sum(diag(mat)/sum(mat))
accuracydt

# logistic regrssion

corre<-cor(dataset1)
corre
cor.plot(corre)

lr1<-glm(training1$Diabetes_011 ~ .,data = training1)
s<-summary(lr1)
s
attributes(lr1)
lr1
probblr<-predict(lr1,training1,type = "response")
probblr
plot(probblr)
prediclr<-ifelse(probblr>0.5,'1','0')
prediclr
accuracylr =mean(prediclr== testing2$Diabetes_011)
accuracylr

#support vector machine learning

install.packages('e1071')
install.packages('caret')
library(e1071)
library(caret)
svm1<-svm(training1$Diabetes_011 ~.,data=training1,method= "svmlinear",trControl=trctrl,
          preProcess = c("center", "scale"),
          tuneLength = 10)
svm1
attributes(svm1)
svm1$degree
summary(svm1)
predsvm<-predict(svm1,training1)
predsvm
plot(predsvm)
predsvm1<-ifelse(predsvm>0.5,'1','0')
predsvm1
accuracysvm=mean(predsvm1== testing2$Diabetes_011)
accuracysvm

# Naive Bayes

nam<- naiveBayes(training1$Diabetes_011 ~ .,data = training1,method = "gaussian")
summary(nam)
nam
attributes(nam)
nam$tables
prednam<-predict(nam,training1)
prednam
plot(prednam)
accuracynam=mean(prednam== testing2$Diabetes_011)
accuracynam

#neuralnet
install.packages("keras")
library(keras)
install.packages("mlbench")
library(mlbench)
library(dplyr)
library(magrittr)
library(neuralnet)
nn<-neuralnet(training1$Diabetes_011 ~.,data = training1,hidden= 2,act.fct = "logistic",
linear.output = TRUE)
attributes(nn)
nn$exclude
summary(nn)
plot(nn)
prednn<-predict(nn,training1)
prednn
prednn1<-ifelse(prednn>0.5,'1','0')
prednn1
accuracynn=mean(prednn1== testing2$Diabetes_011)
accuracynn
accurrracy<-c(accuracylr,accuracynn,accuracyrt,accuracynam,accuracysvm)
plot(accurrracy)
histogram(accuracylr,accuracynn,accuracyrt,accuracynam,accuracysvm)
