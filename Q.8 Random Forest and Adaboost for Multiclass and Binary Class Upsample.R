
setwd("E:\\IDS 572\\Assg 4")
TraindataM_Upsample <- read.csv("TraindataMulticlass.csv")
TestdataMpsample <- read.csv("TestDataMulticlass.csv")

summary(TraindataM_Upsample)
ncol(TraindataM_Upsample)
ncol(TestdataMpsample)
#View(TestdataMpsample)
str(TraindataM_Upsample)

TraindataM_Upsample$target <- TraindataM_Upsample$NPS_Status
TraindataM_Upsample$target <- as.factor(TraindataM_Upsample$target)
summary(TraindataM_Upsample$target)
str(TraindataM_Upsample$target)
TraindataM_Upsample$NPS_Status <- NULL
#---------------------------------------------------Upsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataM_Upsample <- upSample(x=TraindataM_Upsample[,-ncol(TraindataM_Upsample)],y=TraindataM_Upsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataM_Upsample$target)
TraindataM_Upsample$target<- TraindataM_Upsample$Class
TraindataM_Upsample$Class <- NULL
#-----------------------------------------------------------------------

#TraindataM_Upsample_Upsample$NPS_Status <- NULL
for(i in 13:47) {
  #  TraindataM_Upsample[,i]<-as.integer(TraindataM_Upsample[,i])
  TraindataM_Upsample[,i] = factor(TraindataM_Upsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

library(dplyr)
summary(TraindataM_Upsample)
str(TraindataM_Upsample)
#View(TraindataM_Upsample)


TraindataM_Upsample$SN <- NULL
TraindataM_Upsample$AdmissionDate <-NULL
TraindataM_Upsample$DischargeDate <-NULL
TraindataM_Upsample$NPS_Status <- NULL


TraindataM_Upsample_1<-TraindataM_Upsample
TraindataM_Upsample_1$MaritalStatus <- NULL
TraindataM_Upsample_1$BedCategory <- NULL
TraindataM_Upsample_1$State <- NULL
TraindataM_Upsample_1$Country <- NULL
TraindataM_Upsample_1$EM_NURSING <- NULL
TraindataM_Upsample_1$EM_DOCTOR <- NULL
TraindataM_Upsample_1$DOC_ATTITUDE <- NULL
TraindataM_Upsample_1$NS_NURSESATTITUDE <- NULL
TraindataM_Upsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataM_Upsample_1$CE_NPS <- NULL

ncol(TraindataM_Upsample_1)
#summary(TraindataM_Upsample_1$target)
#44 columns 


#------------------------Testdata
for(i in 13:47) {
  TestdataMpsample[,i] = factor(TestdataMpsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataMpsample$NPS_Status)
TestdataMpsample$target <- TestdataMpsample$NPS_Status
TestdataMpsample$target <- as.factor(TestdataMpsample$target)
summary(TestdataMpsample$target)
summary(TestdataMpsample$target)

str(TestdataMpsample)



TestdataMpsample$SN <- NULL
TestdataMpsample$AdmissionDate <-NULL
TestdataMpsample$DischargeDate <-NULL
TestdataMpsample$NPS_Status <- NULL

TestdataMpsample_1<-TestdataMpsample
TestdataMpsample_1$MaritalStatus <- NULL
TestdataMpsample_1$BedCategory <- NULL
TestdataMpsample_1$State <- NULL
TestdataMpsample_1$Country <- NULL
TestdataMpsample_1$EM_NURSING <- NULL
TestdataMpsample_1$EM_DOCTOR <- NULL
TestdataMpsample_1$DOC_ATTITUDE <- NULL
TestdataMpsample_1$NS_NURSESATTITUDE <- NULL
TestdataMpsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataMpsample_1$CE_NPS <- NULL



#TraindataM_Upsample$SN <- NULL
#TraindataM_Upsample$AdmissionDate <-NULL
#TraindataM_Upsample$DischargeDate <-NULL

ncol(TraindataM_Upsample_1)
ncol(TestdataMpsample_1)

str(TestdataMpsample_1)


levels(TestdataMpsample_1$InsPayorcategory)<-levels(TraindataM_Upsample_1$InsPayorcategory)
summary(TestdataMpsample_1$InsPayorcategory)

#-------------------------------------------------------------------Random Forest Model with Target variable having Multiple class and Balanced using Upsample

#glimpse(TraindataM_Upsample_1)

library(randomForest)
model_RF_Multi_Upsample<- randomForest(target~.,data=TraindataM_Upsample_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Multi_Upsample)

library(caret)
Predict_Test_RF_Multi_Upsample<-predict(model_RF_Multi_Upsample,TestdataMpsample_1,type = "class")
#View(upsample)

confusionMatrix_RF_Multi_Upsample <- confusionMatrix(Predict_Test_RF_Multi_Upsample,TestdataMpsample_1$target)
confusionMatrix_RF_Multi_Upsample

Sensitivity_RF_Multi_Upsample <- confusionMatrix_RF_Multi_Upsample$table[1,1]/sum(confusionMatrix_RF_Multi_Upsample$table[1,1],confusionMatrix_RF_Multi_Upsample$table[2,1],confusionMatrix_RF_Multi_Upsample$table[3,1])
Sensitivity_RF_Multi_Upsample*100
#47.72727

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  Detractor Passive Promotor
# Detractor        21       8        2
# Passive          13      57       32
# Promotor         10      52      169
# 
# Overall Statistics
# 
# Accuracy : 0.6786          
# 95% CI : (0.6279, 0.7263)
# No Information Rate : 0.5577          
# P-Value [Acc > NIR] : 1.617e-06       
# 
# Kappa : 0.411           
# 
# Mcnemar's Test P-Value : 0.01028         
# 
# Statistics by Class:
# 
#                      Class: Detractor Class: Passive Class: Promotor
# Sensitivity                   0.47727         0.4872          0.8325
# Specificity                   0.96875         0.8178          0.6149
# Pos Pred Value                0.67742         0.5588          0.7316
# Neg Pred Value                0.93093         0.7710          0.7444
# Prevalence                    0.12088         0.3214          0.5577
# Detection Rate                0.05769         0.1566          0.4643
# Detection Prevalence          0.08516         0.2802          0.6346
# Balanced Accuracy             0.72301         0.6525          0.7237

#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost Model with Target variable having Multiple class and Balanced using Upsample
#install.packages("adabag")
library(adabag)

adaboost_Multi_Upsample <- boosting(target~., data=TraindataM_Upsample_1, boos=TRUE, 
                                     mfinal=10)

Predict_Test_Adaboost_Multi_Upsample<-predict(adaboost_Multi_Upsample,TestdataMpsample_1)#,type="class")
#View(upsample)

Predict_Test_Adaboost_Multi_Upsample$confusion
# Observed Class
# Predicted Class Detractor Passive Promotor
# Detractor        26      19        4
# Passive          14      43       43
# Promotor          4      55      156
confusionMatrix_Adaboost_Multi_Upsample <- confusionMatrix(Predict_Test_Adaboost_Multi_Upsample,TestdataMpsample_1$target)
confusionMatrix_Adaboost_Multi_Upsample
# str(TraindataM_Upsample_2$target)
# str(Testdata$target)
Accuracy=Predict_Test_Adaboost_Multi_Upsample$confusion

Accuracy1=sum(diag(Accuracy))/sum(Accuracy)
Accuracy1*100
#61.81

Sensitivity_Adaboost_Multi_Upsample <- Predict_Test_Adaboost_Multi_Upsample$confusion[1,1]/sum(Predict_Test_Adaboost_Multi_Upsample$confusion[1,1],Predict_Test_Adaboost_Multi_Upsample$confusion[2,1],Predict_Test_Adaboost_Multi_Upsample$confusion[3,1])
Sensitivity_Adaboost_Multi_Upsample*100
#59.09091

#------------------------------------------------------------------------------------






#----------------------------------Random FOrest and Adaboost for target Variable having Binary Class 

setwd("E:\\IDS 572\\Assg 4")
TraindataB_Upsample <- read.csv("TraindataMulticlass.csv")
TestdataB_Upsample <- read.csv("TestDataMulticlass.csv")

summary(TraindataB_Upsample)
ncol(TraindataB_Upsample)
ncol(TestdataB_Upsample)
#View(TestdataB_Upsample)
str(TraindataB_Upsample)

TraindataB_Upsample$target <- ifelse(TraindataB_Upsample$NPS_Status =="Detractor","Detractor","Not_Detractors")
TraindataB_Upsample$target <- as.factor(TraindataB_Upsample$target)
summary(TraindataB_Upsample$target)
str(TraindataB_Upsample$target)
TraindataB_Upsample$NPS_Status <- NULL
#---------------------------------------------------Upsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataB_Upsample <- upSample(x=TraindataB_Upsample[,-ncol(TraindataB_Upsample)],y=TraindataB_Upsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataB_Upsample$Class)
TraindataB_Upsample$target<- TraindataB_Upsample$Class
TraindataB_Upsample$Class <- NULL
#-----------------------------------------------------------------------

#TraindataB_Upsample_Upsample$NPS_Status <- NULL
for(i in 13:47) {
  #  TraindataB_Upsample[,i]<-as.integer(TraindataB_Upsample[,i])
  TraindataB_Upsample[,i] = factor(TraindataB_Upsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

library(dplyr)
summary(TraindataB_Upsample)
str(TraindataB_Upsample)
#View(TraindataB_Upsample)


TraindataB_Upsample$SN <- NULL
TraindataB_Upsample$AdmissionDate <-NULL
TraindataB_Upsample$DischargeDate <-NULL
TraindataB_Upsample$NPS_Status <- NULL


TraindataB_Upsample_1<-TraindataB_Upsample
TraindataB_Upsample_1$MaritalStatus <- NULL
TraindataB_Upsample_1$BedCategory <- NULL
TraindataB_Upsample_1$State <- NULL
TraindataB_Upsample_1$Country <- NULL
TraindataB_Upsample_1$EM_NURSING <- NULL
TraindataB_Upsample_1$EM_DOCTOR <- NULL
TraindataB_Upsample_1$DOC_ATTITUDE <- NULL
TraindataB_Upsample_1$NS_NURSESATTITUDE <- NULL
TraindataB_Upsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataB_Upsample_1$CE_NPS <- NULL

ncol(TraindataB_Upsample_1)
#summary(TraindataB_Upsample_1$target)
#44 columns 


#------------------------Testdata
for(i in 13:47) {
  TestdataB_Upsample[,i] = factor(TestdataB_Upsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataB_Upsample$NPS_Status)
TestdataB_Upsample$target <- ifelse(TestdataB_Upsample$NPS_Status=="Detractor","Detractor","Not_Detractors")
TestdataB_Upsample$target <- as.factor(TestdataB_Upsample$target)
summary(TestdataB_Upsample$target)
summary(TestdataB_Upsample$target)

str(TestdataB_Upsample)



TestdataB_Upsample$SN <- NULL
TestdataB_Upsample$AdmissionDate <-NULL
TestdataB_Upsample$DischargeDate <-NULL
TestdataB_Upsample$NPS_Status <- NULL

TestdataB_Upsample_1<-TestdataB_Upsample
TestdataB_Upsample_1$MaritalStatus <- NULL
TestdataB_Upsample_1$BedCategory <- NULL
TestdataB_Upsample_1$State <- NULL
TestdataB_Upsample_1$Country <- NULL
TestdataB_Upsample_1$EM_NURSING <- NULL
TestdataB_Upsample_1$EM_DOCTOR <- NULL
TestdataB_Upsample_1$DOC_ATTITUDE <- NULL
TestdataB_Upsample_1$NS_NURSESATTITUDE <- NULL
TestdataB_Upsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataB_Upsample_1$CE_NPS <- NULL



#TraindataB_Upsample$SN <- NULL
#TraindataB_Upsample$AdmissionDate <-NULL
#TraindataB_Upsample$DischargeDate <-NULL

ncol(TraindataB_Upsample_1)
ncol(TestdataB_Upsample_1)

str(TestdataB_Upsample_1)


levels(TestdataB_Upsample_1$InsPayorcategory)<-levels(TraindataB_Upsample_1$InsPayorcategory)
summary(TestdataB_Upsample_1$InsPayorcategory)

#-------------------------------------------------------------------Random Forest Model with Target variable having Binary class and Balanced using Upsample

#glimpse(TraindataB_Upsample_1)

library(randomForest)
model_RF_Binary_Upsample<- randomForest(target~.,data=TraindataB_Upsample_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Binary_Upsample)

library(caret)
Predict_Test_RF_Binary_Upsample<-predict(model_RF_Binary_Upsample,TestdataB_Upsample_1,type = "class")
#View(upsample)

confusionMatrix_RF_Binary_Upsample <- confusionMatrix(Predict_Test_RF_Binary_Upsample,TestdataB_Upsample_1$target)
confusionMatrix_RF_Binary_Upsample

Sensitivity_RF_Binary_Upsample <- confusionMatrix_RF_Binary_Upsample$table[1,1]/sum(confusionMatrix_RF_Binary_Upsample$table[1,1],confusionMatrix_RF_Binary_Upsample$table[2,1])
Sensitivity_RF_Binary_Upsample*100
# 40.90909


# Confusion Matrix and Statistics
# 
# Reference
# Prediction       Detractor Not_Detractors
# Detractor             18              8
# Not_Detractors        26            312
# 
# Accuracy : 0.9066          
# 95% CI : (0.8719, 0.9344)
# No Information Rate : 0.8791          
# P-Value [Acc > NIR] : 0.059556        
# 
# Kappa : 0.4664          
# 
# Mcnemar's Test P-Value : 0.003551        
#                                           
#             Sensitivity : 0.40909         
#             Specificity : 0.97500         
#          Pos Pred Value : 0.69231         
#          Neg Pred Value : 0.92308         
#              Prevalence : 0.12088         
#          Detection Rate : 0.04945         
#    Detection Prevalence : 0.07143         
#       Balanced Accuracy : 0.69205         
#                                           
#        'Positive' Class : Detractor       
#                                          

#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost Model with Target variable having Binary class and Balanced using Upsample
#install.packages("adabag")
library(adabag)

adaboost_Binary_Upsample <- boosting(target~., data=TraindataB_Upsample_1, boos=TRUE, 
                                     mfinal=10)

Predict_Test_Adaboost_Binary_Upsample<-predict(adaboost_Binary_Upsample,TestdataB_Upsample_1)#,type="class")
#View(upsample)

Predict_Test_Adaboost_Binary_Upsample$confusion
# Observed Class
# Predicted Class  Detractor Not_Detractors
# Detractor             30             38
# Not_Detractors        14            282
Accuracy=Predict_Test_Adaboost_Binary_Upsample$confusion

Accuracy1=sum(diag(Accuracy))/sum(Accuracy)
Accuracy1*100
#85.16484

Sensitivity_Adaboost_Binary_Upsample <- Predict_Test_Adaboost_Binary_Upsample$confusion[1,1]/sum(Predict_Test_Adaboost_Binary_Upsample$confusion[1,1],Predict_Test_Adaboost_Binary_Upsample$confusion[2,1])
Sensitivity_Adaboost_Binary_Upsample*100
#68.18182


confusionMatrix_Adaboost_Binary_Upsample <- confusionMatrix(Predict_Test_Adaboost_Binary_Upsample,TestdataB_Upsample_1$target)
confusionMatrix_Adaboost_Binary_Upsample
# str(TraindataB_Upsample_2$target)
# str(Testdata$target)

#------------------------------------------------------------------------------------

str(TraindataB_Upsample_1)
str(TestdataB_Upsample_1)

