
setwd("E:\\IDS 572\\Assg 4")
TraindataM_Downsample <- read.csv("TraindataMulticlass.csv")
TestdataM_Downsample <- read.csv("TestDataMulticlass.csv")

summary(TraindataM_Downsample)
ncol(TraindataM_Downsample)
ncol(TestdataM_Downsample)
#View(TestdataM_Downsample)
str(TraindataM_Downsample)

TraindataM_Downsample$target <- TraindataM_Downsample$NPS_Status
TraindataM_Downsample$target <- as.factor(TraindataM_Downsample$target)
summary(TraindataM_Downsample$target)
str(TraindataM_Downsample$target)
TraindataM_Downsample$NPS_Status <- NULL
#---------------------------------------------------Downsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataM_Downsample <- downSample(x=TraindataM_Downsample[,-ncol(TraindataM_Downsample)],y=TraindataM_Downsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataM_Downsample$Class)
TraindataM_Downsample$target<- TraindataM_Downsample$Class
TraindataM_Downsample$Class <- NULL
#-----------------------------------------------------------------------

#TraindataM_Downsample_Downsample$NPS_Status <- NULL
for(i in 13:47) {
  TraindataM_Downsample[,i]<-as.integer(TraindataM_Downsample[,i])
  TraindataM_Downsample[,i] = factor(TraindataM_Downsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

library(dplyr)
summary(TraindataM_Downsample)
str(TraindataM_Downsample)
#View(TraindataM_Downsample)


TraindataM_Downsample$SN <- NULL
TraindataM_Downsample$AdmissionDate <-NULL
TraindataM_Downsample$DischargeDate <-NULL
TraindataM_Downsample$NPS_Status <- NULL


TraindataM_Downsample_1<-TraindataM_Downsample
TraindataM_Downsample_1$MaritalStatus <- NULL
TraindataM_Downsample_1$BedCategory <- NULL
TraindataM_Downsample_1$State <- NULL
TraindataM_Downsample_1$Country <- NULL
TraindataM_Downsample_1$EM_NURSING <- NULL
TraindataM_Downsample_1$EM_DOCTOR <- NULL
TraindataM_Downsample_1$DOC_ATTITUDE <- NULL
TraindataM_Downsample_1$NS_NURSESATTITUDE <- NULL
TraindataM_Downsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataM_Downsample_1$CE_NPS <- NULL

ncol(TraindataM_Downsample_1)
#summary(TraindataM_Downsample_1$target)
#44 columns 


#------------------------Testdata
for(i in 13:47) {
  TestdataM_Downsample[,i] = factor(TestdataM_Downsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataM_Downsample$NPS_Status)
TestdataM_Downsample$target <- TestdataM_Downsample$NPS_Status
TestdataM_Downsample$target <- as.factor(TestdataM_Downsample$target)
summary(TestdataM_Downsample$target)
summary(TestdataM_Downsample$target)

str(TestdataM_Downsample)



TestdataM_Downsample$SN <- NULL
TestdataM_Downsample$AdmissionDate <-NULL
TestdataM_Downsample$DischargeDate <-NULL
TestdataM_Downsample$NPS_Status <- NULL

TestdataM_Downsample_1<-TestdataM_Downsample
TestdataM_Downsample_1$MaritalStatus <- NULL
TestdataM_Downsample_1$BedCategory <- NULL
TestdataM_Downsample_1$State <- NULL
TestdataM_Downsample_1$Country <- NULL
TestdataM_Downsample_1$EM_NURSING <- NULL
TestdataM_Downsample_1$EM_DOCTOR <- NULL
TestdataM_Downsample_1$DOC_ATTITUDE <- NULL
TestdataM_Downsample_1$NS_NURSESATTITUDE <- NULL
TestdataM_Downsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataM_Downsample_1$CE_NPS <- NULL



#TraindataM_Downsample$SN <- NULL
#TraindataM_Downsample$AdmissionDate <-NULL
#TraindataM_Downsample$DischargeDate <-NULL

ncol(TraindataM_Downsample_1)
ncol(TestdataM_Downsample_1)

str(TestdataM_Downsample_1)


levels(TestdataM_Downsample_1$InsPayorcategory)<-levels(TraindataM_Downsample_1$InsPayorcategory)
summary(TestdataM_Downsample_1$InsPayorcategory)

#-------------------------------------------------------------------Rabdom Forest Model with Target variable having Multiple class 

#glimpse(TraindataM_Downsample_1)

library(randomForest)
model_RF_Multi_Downsample<- randomForest(target~.,data=TraindataM_Downsample_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Multi_Downsample)

library(caret)
Predict_Test_RF_Multi_Downsample<-predict(model_RF_Multi_Downsample,TestdataM_Downsample_1,type = "class")
#View(Downsample)

confusionMatrix_RF_Multi_Downsample <- confusionMatrix(Predict_Test_RF_Multi_Downsample,TestdataM_Downsample_1$target)
confusionMatrix_RF_Multi_Downsample

Sensitivity_RF_Multi_Downsample <- confusionMatrix_RF_Multi_Downsample$table[1,1]/sum(confusionMatrix_RF_Multi_Downsample$table[1,1],confusionMatrix_RF_Multi_Downsample$table[2,1],confusionMatrix_RF_Multi_Downsample$table[3,1])
Sensitivity_RF_Multi_Downsample*100
#65.90909


# Confusion Matrix and Statistics
# 
# Reference
# Prediction  Detractor Passive Promotor
# Detractor        29      27        9
# Passive           9      58       66
# Promotor          6      32      128
# 
# Overall Statistics
# 
# Accuracy : 0.5907          
# 95% CI : (0.5382, 0.6416)
# No Information Rate : 0.5577          
# P-Value [Acc > NIR] : 0.1122          
# 
# Kappa : 0.3252          
# 
# Mcnemar's Test P-Value : 8.711e-05       
# 
# Statistics by Class:
# 
#                      Class: Detractor Class: Passive Class: Promotor
# Sensitivity                   0.65909         0.4957          0.6305
# Specificity                   0.88750         0.6964          0.7640
# Pos Pred Value                0.44615         0.4361          0.7711
# Neg Pred Value                0.94983         0.7446          0.6212
# Prevalence                    0.12088         0.3214          0.5577
# Detection Rate                0.07967         0.1593          0.3516
# Detection Prevalence          0.17857         0.3654          0.4560
# Balanced Accuracy             0.77330         0.5960          0.6973
#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost Model with Target variable having Multiple class
#install.packages("adabag")
library(adabag)

adaboost_Multi_Downsample <- boosting(target~., data=TraindataM_Downsample_1, boos=TRUE, 
                                       mfinal=10)

Predict_Test_Adaboost_Multi_Downsample<-predict(adaboost_Multi_Downsample,TestdataM_Downsample_1)#,type="class")
#View(Downsample)

Predict_Test_Adaboost_Multi_Downsample$confusion
# Observed Class
# Predicted Class Detractor Passive Promotor
# Detractor        24      22        7
# Passive          14      43       39
# Promotor          6      52      157

confusionMatrix_Adaboost_Multi_Downsample <- confusionMatrix(Predict_Test_Adaboost_Multi_Downsample,TestdataM_Downsample_1$target)
confusionMatrix_Adaboost_Multi_Downsample
# str(TraindataM_Downsample_2$target)
# str(Testdata$target)

Accuracy=Predict_Test_Adaboost_Multi_Downsample$confusion

Accuracy1=sum(diag(Accuracy))/sum(Accuracy)
Accuracy1*100
#61.81

Sensitivity_Adaboost_Multi_Downsample <- Predict_Test_Adaboost_Multi_Downsample$confusion[1,1]/sum(Predict_Test_Adaboost_Multi_Downsample$confusion[1,1],Predict_Test_Adaboost_Multi_Downsample$confusion[2,1],Predict_Test_Adaboost_Multi_Downsample$confusion[3,1])
Sensitivity_Adaboost_Multi_Downsample*100
#54.54545

#------------------------------------------------------------------------------------






#--------------------------------------------------------------------Random FOrest and Adaboost for Binary Class

setwd("E:\\IDS 572\\Assg 4")
TraindataB_Downsample <- read.csv("TraindataMulticlass.csv")
TestdataB_Downsample <- read.csv("TestDataMulticlass.csv")

summary(TraindataB_Downsample)
ncol(TraindataB_Downsample)
ncol(TestdataB_Downsample)
#View(TestdataB_Downsample)
str(TraindataB_Downsample)

TraindataB_Downsample$target <- ifelse(TraindataB_Downsample$NPS_Status =="Detractor","Detractor","Not_Detractors")
TraindataB_Downsample$target <- as.factor(TraindataB_Downsample$target)
summary(TraindataB_Downsample$target)
str(TraindataB_Downsample$target)
TraindataB_Downsample$NPS_Status <- NULL
#---------------------------------------------------Downsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataB_Downsample <- downSample(x=TraindataB_Downsample[,-ncol(TraindataB_Downsample)],y=TraindataB_Downsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataB_Downsample$Class)
TraindataB_Downsample$target<- TraindataB_Downsample$Class
TraindataB_Downsample$Class <- NULL
#-----------------------------------------------------------------------

#TraindataB_Downsample_Downsample$NPS_Status <- NULL
for(i in 13:47) {
  TraindataB_Downsample[,i]<-as.integer(TraindataB_Downsample[,i])
  TraindataB_Downsample[,i] = factor(TraindataB_Downsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

library(dplyr)
summary(TraindataB_Downsample)
str(TraindataB_Downsample)
#View(TraindataB_Downsample)


TraindataB_Downsample$SN <- NULL
TraindataB_Downsample$AdmissionDate <-NULL
TraindataB_Downsample$DischargeDate <-NULL
TraindataB_Downsample$NPS_Status <- NULL


TraindataB_Downsample_1<-TraindataB_Downsample
TraindataB_Downsample_1$MaritalStatus <- NULL
TraindataB_Downsample_1$BedCategory <- NULL
TraindataB_Downsample_1$State <- NULL
TraindataB_Downsample_1$Country <- NULL
TraindataB_Downsample_1$EM_NURSING <- NULL
TraindataB_Downsample_1$EM_DOCTOR <- NULL
TraindataB_Downsample_1$DOC_ATTITUDE <- NULL
TraindataB_Downsample_1$NS_NURSESATTITUDE <- NULL
TraindataB_Downsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataB_Downsample_1$CE_NPS <- NULL

ncol(TraindataB_Downsample_1)
#summary(TraindataB_Downsample_1$target)
#44 columns 


#------------------------Testdata


for(i in 13:47) {
  TestdataB_Downsample[,i] = factor(TestdataB_Downsample[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataB_Downsample$NPS_Status)
TestdataB_Downsample$target <- ifelse(TestdataB_Downsample$NPS_Status=="Detractor","Detractor","Not_Detractors")
TestdataB_Downsample$target <- as.factor(TestdataB_Downsample$target)
summary(TestdataB_Downsample$target)
summary(TestdataB_Downsample$target)

str(TestdataB_Downsample)



TestdataB_Downsample$SN <- NULL
TestdataB_Downsample$AdmissionDate <-NULL
TestdataB_Downsample$DischargeDate <-NULL
TestdataB_Downsample$NPS_Status <- NULL

TestdataB_Downsample_1<-TestdataB_Downsample
TestdataB_Downsample_1$MaritalStatus <- NULL
TestdataB_Downsample_1$BedCategory <- NULL
TestdataB_Downsample_1$State <- NULL
TestdataB_Downsample_1$Country <- NULL
TestdataB_Downsample_1$EM_NURSING <- NULL
TestdataB_Downsample_1$EM_DOCTOR <- NULL
TestdataB_Downsample_1$DOC_ATTITUDE <- NULL
TestdataB_Downsample_1$NS_NURSESATTITUDE <- NULL
TestdataB_Downsample_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataB_Downsample_1$CE_NPS <- NULL



#TraindataB_Downsample$SN <- NULL
#TraindataB_Downsample$AdmissionDate <-NULL
#TraindataB_Downsample$DischargeDate <-NULL

ncol(TraindataB_Downsample_1)
ncol(TestdataB_Downsample_1)

str(TestdataB_Downsample_1)


levels(TestdataB_Downsample_1$InsPayorcategory)<-levels(TraindataB_Downsample_1$InsPayorcategory)
summary(TestdataB_Downsample_1$InsPayorcategory)

#-------------------------------------------------------------------Rabdom Forest Model with Target variable having Binary class 

#glimpse(TraindataB_Downsample_1)

library(randomForest)
model_RF_Binary_Downsample<- randomForest(target~.,data=TraindataB_Downsample_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Binary_Downsample)

library(caret)
Predict_Test_RF_Binary_Downsample<-predict(model_RF_Binary_Downsample,TestdataB_Downsample_1,type = "class")
#View(Downsample)

confusionMatrix_RF_Binary_Downsample <- confusionMatrix(Predict_Test_RF_Binary_Downsample,TestdataB_Downsample_1$target)
confusionMatrix_RF_Binary_Downsample

Sensitivity_RF_Binary_Downsample <- confusionMatrix_RF_Binary_Downsample$table[1,1]/sum(confusionMatrix_RF_Binary_Downsample$table[1,1],confusionMatrix_RF_Binary_Downsample$table[2,1])
Sensitivity_RF_Binary_Downsample*100
#70.45455


# Confusion Matrix and Statistics
# 
# Reference
# Prediction       Detractor Not_Detractors
# Detractor             31             55
# Not_Detractors        13            265
# 
# Accuracy : 0.8132          
# 95% CI : (0.7693, 0.8519)
# No Information Rate : 0.8791          
# P-Value [Acc > NIR] : 0.9999          
# 
# Kappa : 0.3773          
# 
# Mcnemar's Test P-Value : 6.627e-07       
#                                           
#             Sensitivity : 0.70455         
#             Specificity : 0.82812         
#          Pos Pred Value : 0.36047         
#          Neg Pred Value : 0.95324         
#              Prevalence : 0.12088         
#          Detection Rate : 0.08516         
#    Detection Prevalence : 0.23626         
#       Balanced Accuracy : 0.76634         
#                                           
#        'Positive' Class : Detractor   
#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost Model with Target variable having Binary class
#install.packages("adabag")
library(adabag)

adaboost_Binary_Downsample <- boosting(target~., data=TraindataB_Downsample_1, boos=TRUE, 
                                       mfinal=10)

Predict_Test_Adaboost_Binary_Downsample<-predict(adaboost_Binary_Downsample,TestdataB_Downsample_1)#,type="class")
#View(Downsample)

Predict_Test_Adaboost_Binary_Downsample$confusion
# Observed Class
# Predicted Class  Detractor Not_Detractors
# Detractor             31             64
# Not_Detractors        13            256
confusionMatrix_Adaboost_Binary_Downsample <- confusionMatrix(Predict_Test_Adaboost_Binary_Downsample,TestdataB_Downsample_1$target)
confusionMatrix_Adaboost_Binary_Downsample
# str(TraindataB_Downsample_2$target)
# str(Testdata$target)
Accuracy=Predict_Test_Adaboost_Binary_Downsample$confusion

Accuracy1=sum(diag(Accuracy))/sum(Accuracy)
Accuracy1*100
#78.84615

Sensitivity_Adaboost_Binary_Downsample <- Predict_Test_Adaboost_Binary_Downsample$confusion[1,1]/sum(Predict_Test_Adaboost_Binary_Downsample$confusion[1,1],Predict_Test_Adaboost_Binary_Downsample$confusion[2,1])
Sensitivity_Adaboost_Binary_Downsample*100
#70.45455

##------------------------------------------------------------------------------------
str(TraindataB_Downsample_1)
str(TestdataB_Downsample_1)
