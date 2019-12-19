
setwd("E:\\IDS 572\\Assg 4")
TraindataM_SMOTE <- read.csv("TraindataMulticlass.csv")
TestdataM_SMOTE <- read.csv("TestDataMulticlass.csv")

summary(TraindataM_SMOTE)
str(TraindataM_SMOTE$NPS_Status)
ncol(TestdataM_SMOTE)
#View(TestdataM_SMOTE)

#---------------------------------------------------SMOTE Classifier fot handelling imbalance(it balances the number of rows for each class)
#install.packages('UBL')
library(UBL)
TraindataM_SMOTE <- SmoteClassif(NPS_Status~., TraindataM_SMOTE, C.perc="balance", k=5, dist="HEOM", p=2)
#summary(balancedata$Training_final_265.target) 
#summary(TraindataM_SMOTE$target)

#-----------------------------------------------------------------------

#TraindataM_SMOTE_SMOTE$NPS_Status <- NULL
for(i in 13:47) {
  TraindataM_SMOTE[,i]<-as.integer(TraindataM_SMOTE[,i])
  TraindataM_SMOTE[,i] = factor(TraindataM_SMOTE[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

TraindataM_SMOTE$target <- TraindataM_SMOTE$NPS_Status 
TraindataM_SMOTE$target <- as.factor(TraindataM_SMOTE$target)
summary(TraindataM_SMOTE$target)

str(TraindataM_SMOTE)
ncol(TraindataM_SMOTE)
#View(TraindataM_SMOTE)


TraindataM_SMOTE$SN <- NULL
TraindataM_SMOTE$AdmissionDate <-NULL
TraindataM_SMOTE$DischargeDate <-NULL
TraindataM_SMOTE$NPS_Status <- NULL


TraindataM_SMOTE_1<-TraindataM_SMOTE
TraindataM_SMOTE_1$MaritalStatus <- NULL
TraindataM_SMOTE_1$BedCategory <- NULL
TraindataM_SMOTE_1$State <- NULL
TraindataM_SMOTE_1$Country <- NULL
TraindataM_SMOTE_1$EM_NURSING <- NULL
TraindataM_SMOTE_1$EM_DOCTOR <- NULL
TraindataM_SMOTE_1$DOC_ATTITUDE <- NULL
TraindataM_SMOTE_1$NS_NURSESATTITUDE <- NULL
TraindataM_SMOTE_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataM_SMOTE_1$CE_NPS <- NULL

ncol(TraindataM_SMOTE_1)
#summary(TraindataM_SMOTE_1$target)
#39 columns 


#------------------------Testdata
for(i in 13:47) {
  TestdataM_SMOTE[,i] = factor(TestdataM_SMOTE[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataM_SMOTE$NPS_Status)
TestdataM_SMOTE$target <- TestdataM_SMOTE$NPS_Status
TestdataM_SMOTE$target <- as.factor(TestdataM_SMOTE$target)
summary(TestdataM_SMOTE$target)
str(TestdataM_SMOTE$target)

str(TestdataM_SMOTE)



TestdataM_SMOTE$SN <- NULL
TestdataM_SMOTE$AdmissionDate <-NULL
TestdataM_SMOTE$DischargeDate <-NULL
TestdataM_SMOTE$NPS_Status <- NULL

TestdataM_SMOTE_1<-TestdataM_SMOTE
TestdataM_SMOTE_1$MaritalStatus <- NULL
TestdataM_SMOTE_1$BedCategory <- NULL
TestdataM_SMOTE_1$State <- NULL
TestdataM_SMOTE_1$Country <- NULL
TestdataM_SMOTE_1$EM_NURSING <- NULL
TestdataM_SMOTE_1$EM_DOCTOR <- NULL
TestdataM_SMOTE_1$DOC_ATTITUDE <- NULL
TestdataM_SMOTE_1$NS_NURSESATTITUDE <- NULL
TestdataM_SMOTE_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataM_SMOTE_1$CE_NPS <- NULL



#TraindataM_SMOTE$SN <- NULL
#TraindataM_SMOTE$AdmissionDate <-NULL
#TraindataM_SMOTE$DischargeDate <-NULL

ncol(TraindataM_SMOTE_1)
ncol(TestdataM_SMOTE_1)

levels(TestdataM_SMOTE_1$InsPayorcategory)<-levels(TraindataM_SMOTE_1$InsPayorcategory)
summary(TestdataM_SMOTE_1$InsPayorcategory)

str(TraindataM_SMOTE_1)
#-------------------------------------------------------------------Random Forest Model with Target variable having Multiple class and Balanced using SMOTE

library(randomForest)
model_RF_Multi_SMOTE<- randomForest(target~.,data=TraindataM_SMOTE_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Multi_SMOTE)

library(caret)
Predict_Test_RF_Multi_SMOTE<-predict(model_RF_Multi_SMOTE,TestdataM_SMOTE_1,type = "class")
#View(upsample)
confusionMatrix_RF_Multi_SMOTE <- confusionMatrix(Predict_Test_RF_Multi_SMOTE,TestdataM_SMOTE_1$target)
confusionMatrix_RF_Multi_SMOTE$table

Sensitivity_RF_Multi_SMOTE <- confusionMatrix_RF_Multi_SMOTE$table[1,1]/sum(confusionMatrix_RF_Multi_SMOTE$table[1,1],confusionMatrix_RF_Multi_SMOTE$table[2,1],confusionMatrix_RF_Multi_SMOTE$table[3,1])
Sensitivity_RF_Multi_SMOTE*100
#52.27273

# Confusion Matrix and Statistics
# 
# Reference
# Prediction  Detractor Passive Promotor
# Detractor        23      15        3
# Passive          16      65       66
# Promotor          5      37      134
# 
# Overall Statistics
# 
# Accuracy : 0.6099          
# 95% CI : (0.5577, 0.6603)
# No Information Rate : 0.5577          
# P-Value [Acc > NIR] : 0.02503         
# 
# Kappa : 0.3353          
# 
# Mcnemar's Test P-Value : 0.03360         
# 
# Statistics by Class:
# 
#                      Class: Detractor Class: Passive Class: Promotor
# Sensitivity                   0.52273         0.5556          0.6601
# Specificity                   0.94375         0.6680          0.7391
# Pos Pred Value                0.56098         0.4422          0.7614
# Neg Pred Value                0.93498         0.7604          0.6330
# Prevalence                    0.12088         0.3214          0.5577
# Detection Rate                0.06319         0.1786          0.3681
# Detection Prevalence          0.11264         0.4038          0.4835
# Balanced Accuracy             0.73324         0.6118          0.6996
#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost  Model with Target variable having Multiple class and Balanced using SMOTE
#install.packages("adabag")
library(adabag)

adaboost_Multi_SMOTE <- boosting(target~., data=TraindataM_SMOTE_1, boos=TRUE, 
                                  mfinal=10)

Predict_Test_Adaboost_Multi_SMOTE<-predict(adaboost_Multi_SMOTE,TestdataM_SMOTE_1)#,type="class")
#View(upsample)

Predict_Test_Adaboost_Multi_SMOTE$confusion
# Observed Class
# Predicted Class Detractor Passive Promotor
# Detractor        23      14        2
# Passive          17      48       44
# Promotor          4      55      157


Accuracy=Predict_Test_Adaboost_Multi_SMOTE$confusion

Accuracy1=sum(diag(Accuracy))/sum(Accuracy)
Accuracy1*100

Sensitivity_Adaboost_Multi_SMOTE <- Predict_Test_Adaboost_Multi_SMOTE$confusion[1,1]/sum(Predict_Test_Adaboost_Multi_SMOTE$confusion[1,1],Predict_Test_Adaboost_Multi_SMOTE$confusion[2,1],Predict_Test_Adaboost_Multi_SMOTE$confusion[3,1])
Sensitivity_Adaboost_Multi_SMOTE*100
#52.27273

confusionMatrix_Adaboost_Multi_SMOTE <- confusionMatrix(Predict_Test_Adaboost_Multi_SMOTE,TestdataM_SMOTE_1$target)
confusionMatrix_Adaboost_Multi_SMOTE
# str(TraindataM_SMOTE_2$target)
# str(Testdata$target)

#------------------------------------------------------------------------------------





#-------------------------------------------------------Random FOrest and Adaboost for Binary data after Balancing using SMOTE


setwd("E:\\IDS 572\\Assg 4")
TraindataB_SMOTE <- read.csv("TraindataMulticlass.csv")
TestdataB_SMOTE <- read.csv("TestDataMulticlass.csv")

summary(TraindataB_SMOTE)
str(TraindataB_SMOTE$NPS_Status)
ncol(TestdataB_SMOTE)
#View(TestdataB_SMOTE)

#---------------------------------------------------SMOTE Classifier fot handelling imbalance(it balances the number of rows for each class)
#install.packages('UBL')
library(UBL)
TraindataB_SMOTE <- SmoteClassif(NPS_Status~., TraindataB_SMOTE, C.perc="balance", k=5, dist="HEOM", p=2)
#summary(balancedata$Training_final_265.target) 
#summary(TraindataB_SMOTE$target)

#-----------------------------------------------------------------------

#TraindataB_SMOTE_SMOTE$NPS_Status <- NULL
for(i in 13:47) {
  TraindataB_SMOTE[,i]<-as.integer(TraindataB_SMOTE[,i])
  TraindataB_SMOTE[,i] = factor(TraindataB_SMOTE[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

TraindataB_SMOTE$target <- ifelse(TraindataB_SMOTE$NPS_Status =="Detractor","Detractor","Not_Detractors")
TraindataB_SMOTE$target <- as.factor(TraindataB_SMOTE$target)
summary(Traindata$target)
str(Traindata$target)
str(TraindataB_SMOTE)
ncol(TraindataB_SMOTE)
#View(TraindataB_SMOTE)


TraindataB_SMOTE$SN <- NULL
TraindataB_SMOTE$AdmissionDate <-NULL
TraindataB_SMOTE$DischargeDate <-NULL
TraindataB_SMOTE$NPS_Status <- NULL


TraindataB_SMOTE_1<-TraindataB_SMOTE
TraindataB_SMOTE_1$MaritalStatus <- NULL
TraindataB_SMOTE_1$BedCategory <- NULL
TraindataB_SMOTE_1$State <- NULL
TraindataB_SMOTE_1$Country <- NULL
TraindataB_SMOTE_1$EM_NURSING <- NULL
TraindataB_SMOTE_1$EM_DOCTOR <- NULL
TraindataB_SMOTE_1$DOC_ATTITUDE <- NULL
TraindataB_SMOTE_1$NS_NURSESATTITUDE <- NULL
TraindataB_SMOTE_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataB_SMOTE_1$CE_NPS <- NULL

ncol(TraindataB_SMOTE_1)
#summary(TraindataB_SMOTE_1$target)
#39 columns 


#------------------------Testdata
for(i in 13:47) {
  TestdataB_SMOTE[,i] = factor(TestdataB_SMOTE[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataB_SMOTE$NPS_Status)
TestdataB_SMOTE$target <- ifelse(TestdataB_SMOTE$NPS_Status=="Detractor","Detractor","Not_Detractors")
TestdataB_SMOTE$target <- as.factor(TestdataB_SMOTE$target)
summary(TestdataB_SMOTE$target)
str(TestdataB_SMOTE$target)

str(TestdataB_SMOTE)



TestdataB_SMOTE$SN <- NULL
TestdataB_SMOTE$AdmissionDate <-NULL
TestdataB_SMOTE$DischargeDate <-NULL
TestdataB_SMOTE$NPS_Status <- NULL

TestdataB_SMOTE_1<-TestdataB_SMOTE
TestdataB_SMOTE_1$MaritalStatus <- NULL
TestdataB_SMOTE_1$BedCategory <- NULL
TestdataB_SMOTE_1$State <- NULL
TestdataB_SMOTE_1$Country <- NULL
TestdataB_SMOTE_1$EM_NURSING <- NULL
TestdataB_SMOTE_1$EM_DOCTOR <- NULL
TestdataB_SMOTE_1$DOC_ATTITUDE <- NULL
TestdataB_SMOTE_1$NS_NURSESATTITUDE <- NULL
TestdataB_SMOTE_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataB_SMOTE_1$CE_NPS <- NULL



#TraindataB_SMOTE$SN <- NULL
#TraindataB_SMOTE$AdmissionDate <-NULL
#TraindataB_SMOTE$DischargeDate <-NULL

ncol(TraindataB_SMOTE_1)
ncol(TestdataB_SMOTE_1)

levels(TestdataB_SMOTE_1$InsPayorcategory)<-levels(TraindataB_SMOTE_1$InsPayorcategory)
summary(TestdataB_SMOTE_1$InsPayorcategory)

#-------------------------------------------------------------------Random Forest Model with Target variable having Binary class and Balanced using SMOTE 

library(randomForest)
model_RF_Binary_SMOTE<- randomForest(target~.,data=TraindataB_SMOTE_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Binary_SMOTE)

library(caret)
Predict_Test_RF_Binary_SMOTE<-predict(model_RF_Binary_SMOTE,TestdataB_SMOTE_1,type = "class")
#View(upsample)

confusionMatrix_RF_Binary_SMOTE <- confusionMatrix(Predict_Test_RF_Binary_SMOTE,TestdataB_SMOTE_1$target)
confusionMatrix_RF_Binary_SMOTE

Sensitivity_RF_Binary_SMOTE <- confusionMatrix_RF_Binary_SMOTE$table[1,1]/sum(confusionMatrix_RF_Binary_SMOTE$table[1,1],confusionMatrix_RF_Binary_SMOTE$table[2,1])
Sensitivity_RF_Binary_SMOTE*100
#47.72727


# Confusion Matrix and Statistics
# 
# Reference
# Prediction       Detractor Not_Detractors
# Detractor             21             14
# Not_Detractors        23            306
# 
# Accuracy : 0.8984          
# 95% CI : (0.8626, 0.9274)
# No Information Rate : 0.8791          
# P-Value [Acc > NIR] : 0.1474          
# 
# Kappa : 0.4755          
# 
# Mcnemar's Test P-Value : 0.1884          
#                                           
#             Sensitivity : 0.47727         
#             Specificity : 0.95625         
#          Pos Pred Value : 0.60000         
#          Neg Pred Value : 0.93009         
#              Prevalence : 0.12088         
#          Detection Rate : 0.05769         
#    Detection Prevalence : 0.09615         
#       Balanced Accuracy : 0.71676         
#                                           
#        'Positive' Class : Detractor  
#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost Model with Target variable having Binary class and Balanced using SMOTE 
#install.packages("adabag")
library(adabag)

adaboost_Binary_SMOTE <- boosting(target~., data=TraindataB_SMOTE_1, boos=TRUE, 
                                  mfinal=10)

Predict_Test_Adaboost_Binary_SMOTE<-predict(adaboost_Binary_SMOTE,TestdataB_SMOTE_1)#,type="class")
#View(upsample)

Predict_Test_Adaboost_Binary_SMOTE$confusion
# Observed Class
# Predicted Class  Detractor Not_Detractors
# Detractor             25             16
# Not_Detractors        19            304
# str(TraindataB_SMOTE_2$target)
# str(Testdata$target)

Accuracy=Predict_Test_Adaboost_Binary_SMOTE$confusion

Accuracy1=sum(diag(Accuracy))/sum(Accuracy)
Accuracy1*100
#90.38

Sensitivity_Adaboost_Binary_SMOTE <- Predict_Test_Adaboost_Binary_SMOTE$confusion[1,1]/sum(Predict_Test_Adaboost_Binary_SMOTE$confusion[1,1],Predict_Test_Adaboost_Binary_SMOTE$confusion[2,1])
Sensitivity_Adaboost_Binary_SMOTE*100
#56.81818

#------------------------------------------------------------------------------------


