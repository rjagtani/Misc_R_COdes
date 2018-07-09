library(caret)
#library(caTools)
library(data.table)
library(mltools)
library(xlsx)
library(dplyr)
library(tidyr)


setwd('C:\\Users\\rjagtani\\OneDrive - Tata industries\\Confidential\\2017-18\\Projects\\Functions\\Model Evaluation Binary Classification')
df1=read.csv('train_titanic.csv')
train_test_split=sample.split(df1$Survived)
df1_train=df1[train_test_split,]
df1_test=df1[!train_test_split,]

fmla=as.formula(paste("y_flag ~",paste(selected_cols, collapse= "+")))
fmla

lambda<-c(0.01,0.05,0.1)
trainData$flag=as.integer(as.character(trainData$flag))
oot1$flag=as.integer(as.character(oot1$flag))

boost_fit<-gbm(fmla,data=trainData,distribution="multinomial",n.trees=1500,shrinkage=0.1,
               interaction.depth=4)


#Tuning parameters in test. We train the model on a large number of trees and 
#then check which combination of learning rate and no of trees works best for test

rel_imp=as.data.frame(summary.gbm(boost_fit))
rel_imp$var=row.names(rel_imp)
rel_imp=arrange(rel_imp,desc(rel.inf))
