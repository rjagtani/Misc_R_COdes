library(caret)
library(caTools)
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
pick_1s=subset(df1_train,Survived==1)

a=glm(data=df1_train,formula=Survived~Sex+Fare,family = 'binomial')
pred=a$fitted.values
actual=df1_train$Survived
pred_test=predict(a,newdata=df1_test,type='response')
actual_test=df1_test$Survived

model_eval=function(pred_prob,actual_values,prob_cutoff=0.5,filename='model_eval',ntile=10,pred_prob_test=NULL,actual_values_test=NULL)
{
  
  #pred_prob=pred;actual_values=actual;prob_cutoff=0.5;pred_class=NULL
  
  ##### AUC , ROC Curve
  eval_metrics=function(prob,act)
  {
    auc=auc_roc(prob,act)
    gini = (2 * auc) - 1 
    auc_roc=as.data.frame(auc_roc(prob,act,returnDT=TRUE))
    
    ###### Lift curve
    
    lift = function(depvar, predcol, groups=ntile) 
    {
      if(is.factor(depvar)) depvar <- as.integer(as.character(depvar))
      if(is.factor(predcol)) predcol <- as.integer(as.character(predcol))
      helper = data.frame(cbind(depvar, predcol))
      helper[,"bucket"] = ntile(-helper[,"predcol"], groups)
      gaintable = helper %>% group_by(bucket)  %>%
        summarise_at(vars(depvar), funs(total = n(),
                                        totalresp=sum(., na.rm = TRUE),totalnonresp=total-totalresp)) %>%
        mutate(Cumresp = cumsum(totalresp),Cumresp_percent=Cumresp/sum(totalresp),Cumnonresp=cumsum(totalnonresp),Cumnonresp_percent=Cumnonresp/sum(totalnonresp),
               Cum_resp_minus_cum_non_resp=Cumresp_percent-Cumnonresp_percent,Gain=Cumresp/sum(totalresp)*100,
               Cumlift=Gain/(bucket*(100/groups)))
      gaintable=as.data.frame(gaintable)
      ks=c(max(gaintable$Cum_resp_minus_cum_non_resp),which.max(gaintable$Cum_resp_minus_cum_non_resp))
      names(ks)=c('KS_value','KS_decile')
      return(list(gaintable,ks))
    }  
    
    
    cumulative_table=lift(depvar = act,predcol = prob)
    lift_table=as.data.frame(cumulative_table[[1]])
    ks_stats=cumulative_table[[2]]
    #lift_curve_sheet=createSheet(wb=me,sheetName = 'Lift Curve')
    return(c(ks_stats[1],auc))
  }
  train1=eval_metrics(act = actual_values,prob=pred_prob)
  names(train1)[1:2]=c('KS_train','AUC_train')
  if(!is.null(actual_values_test) & !is.null(pred_prob_test))
  {
    test1=eval_metrics(act = actual_values_test,prob=pred_prob_test)
    train1=c(train1,test1)
    names(train1)[3:4]=c('KS_test','AUC_test')
  }
  
  return(train1)
}


abcd=model_eval(actual_values = actual,pred_prob = pred,filename='mv1',pred_prob_test = pred_test,actual_values_test = actual_test)
