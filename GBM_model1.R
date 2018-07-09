### FINAL MODEL 1

#/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model
setwd("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds")
setwd("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model")
load("final_model1.Rda")
varImpPlot(tree_1)



#### Random Forest Model 1 

#### Random forest - stage 1 

###### Random forest model 1

setwd("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds")
modelling_ds=read.csv("mds_1110_m1.csv")
modelling_ds$ce_date_lag=as.Date(modelling_ds$ce_date_lag)
modelling_ds$std_lag=as.factor(modelling_ds$std_lag)
# modelling_ds$name=NULL
# combined_ds=read.csv("all_vars_1808.csv")
# combined_ds$ce_date_lag=as.Date(combined_ds$ce_date_lag)
# modelling_ds=merge(modelling_ds,combined_ds[,c("name1","new_refresh_remark")])
# modelling_ds=merge(modelling_ds,combined_ds[,c("name1","ce_date_lag")])
# class(modelling_ds$ce_date_lag)
# rm(combined_ds)
head(modelling_ds$ce_date_lag)
# write.csv(modelling_ds,"modelling_ds.csv",row.names = FALSE)


########Choosing In time and out of time sample 


mds=modelling_ds
#oot1=mds[mds$ce_date_lag>='2017-01-01' & mds$ce_date_lag<'2017-02-01',]
oot1=mds[mds$ce_date_lag>='2017-02-01' & mds$ce_date_lag<='2017-07-31',]
#oot2=mds[mds$ce_date_lag>='2017-03-01' & mds$ce_date_lag<='2017-03-31',]
it=mds[mds$ce_date_lag<'2017-02-01',]

#selected_cols=colnames(mds)[c(3,4,5,14:17,24,25,27:50,52:429)]
v_imp=read.csv("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model/top_100_vars.csv",stringsAsFactors = F)
selected_cols=v_imp$var[1:100]
exclude=c("reneval_account","count_unique_renewal_dates_account","ratio_unique_renewal_dates_account","count_open_product_speed","count_circuit_industry_product","speedmbps_lag","ratio_termination","avgMRC_account","count_open_account","sr_more_than_1_renewal","count_open_sub_industry","count_open_product","opp_last_9")
selected_cols=selected_cols[!selected_cols %in% exclude]
#selected_cols=var_imp$var[1:50]


########

a=it
set.seed(66)
ind <- sample(2,nrow(a),replace=TRUE, prob=c(1,0))
trainData=a[ind==1,]
### there is no test since the proportion is zero
testData=a[ind==2,]
fmla=as.formula(paste("y_flag ~",paste(selected_cols, collapse= "+")))
fmla
trainData$y_flag=as.factor(trainData$y_flag)
tree_1 <- randomForest(fmla,data=trainData,ntree=75,nodesize=20,norm.votes=FALSE,imp=T)
summary(tree_1)
var_imp=as.data.frame(tree_1$importance)
var_imp$var=row.names(var_imp)
var_imp=arrange(var_imp,desc(MeanDecreaseAccuracy))

#write.csv(var_imp,"model1_var_imp.csv",row.names = F)
selected_cols=c(as.character(var_imp$var[1:62]))
train_predict_us=predict(tree_1,trainData,type="class")
confusionMatrix(train_predict_us,trainData$y_flag)
test_predict=predict(tree_1,testData,type="class")
confusionMatrix(test_predict,testData$y_flag)
oot_predict1=predict(tree_1,oot1,type="class")
#oot1=cbind(oot1,oot_predict1)
oot1=mds[mds$ce_date_lag>='2017-02-01' & mds$ce_date_lag<='2017-07-31',]
confusionMatrix(oot_predict1,oot1$y_flag)
#save(tree_1,file="tree_m1_1110.Rda")





fmla=as.formula(paste("y_flag ~",paste(selected_cols, collapse= "+")))
fmla

lambda<-c(0.01,0.05,0.1)
trainData$flag=as.integer(as.character(trainData$flag))
oot1$flag=as.integer(as.character(oot1$flag))

#for (i in lambda)

#{
#Running the gbm model

boost_fit<-gbm(fmla,data=trainData,distribution="multinomial",n.trees=1500,shrinkage=0.1,
               interaction.depth=4)


#Tuning parameters in test. We train the model on a large number of trees and 
#then check which combination of learning rate and no of trees works best for test

rel_imp=as.data.frame(summary.gbm(boost_fit))
rel_imp$var=row.names(rel_imp)
rel_imp=arrange(rel_imp,desc(rel.inf))

n_trees=seq(from=100, to=1500, by=100)
for(i in 1:15)
{
  
  # pred_train=predict(boost_fit,trainData,n.trees=1000,type="response")
  # pred_train=as.data.frame(pred_train)
  #colnames(pred_train)=factor_name1
  #p.pred_train=apply(pred_train,1,FUN=which.max)
  #colnames(pred_train)=1:16
  #p_class=colnames(pred_train)[p.pred_train]
  #summary(pred_train$pred_train)
  
  
  # prop.table(table(trainData$flag))
  # quantile(pred_train$pred_train,0.8)
  # pred_train$ebkt=ifelse(pred_train$pred_train>as.numeric(0.30),1,0)
  # pred_train=cbind(pred_train,trainData$flag)
  # confusionMatrix(pred_train$ebkt,pred_train$`trainData$flag`)
  
  ### test predict
  
  pred_test=data.frame()
  pred_test=predict(boost_fit,oot1,n.trees=n_trees[i],type="response")
  pred_test=as.data.frame(pred_test)
  colnames(pred_test)
  colnames(pred_test)=gsub(pattern=paste0(".",i,"00"),replace="",colnames(pred_test))
  for(j in 1:nrow(pred_test))
  {
    pred_test[j,"final_pred"]=colnames(pred_test)[which.max(pred_test[j,1:3])]
  }
  
  pred_test$final_pred=as.factor(pred_test$final_pred)
  #colnames(pred_test)=factor_name1
  #prop.table(table(oot1$flag))
  #quantile(pred_train$pred_train,0.8)
  #pred_test$ebkt=ifelse(pred_test$pred_test>as.numeric(0.30),1,0)
  pred_test=cbind(pred_test,oot1$y_flag)
  print(paste0("Case : ",i))
  print(confusionMatrix(pred_test$final_pred,pred_test$`oot1$y_flag`))
}


save(tree_1,file="/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model/prelim_model1.Rda")



##### prelim model 

write.csv(var_imp,"/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model/var_imp_prelim_model1.csv",row.names=F)
load("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model/prelim_model1.Rda")



###### Removing correlated variabls from model 1 


top_vars=as.character(var_imp$var)
mds2_summ=as.data.frame(smbinning.eda(mds[,top_vars]))
num_vars=as.character(mds2_summ[mds2_summ$eda.Type=='Num/Int',"eda.Field"])
factor_vars=top_vars[!top_vars %in% num_vars]
mds2_cor_check=mds[,num_vars]
cor1=as.data.frame(cor(mds2_cor_check))
write.csv(cor1,"/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model/cor1_check.csv")


mds_new=mds[,num_vars]
cor_num=findCorrelation(cor(mds_new), cutoff = 0.8, verbose = FALSE, names = FALSE,
                        exact = F)
mds_new1=mds_new[-cor_num]
colnames(mds_new)[cor_num]
selected_cols=c(colnames(mds_new1),factor_vars)
save(tree_1,file="final_model1.Rda")



#######


k=read.csv("/tcad2/client10/rjagtani10_restored_data/new_data_scoring/erosion_ds/final_model/model1_var_imp.csv",stringsAsFactors = F)
selected_cols=k$var
rm(k)




