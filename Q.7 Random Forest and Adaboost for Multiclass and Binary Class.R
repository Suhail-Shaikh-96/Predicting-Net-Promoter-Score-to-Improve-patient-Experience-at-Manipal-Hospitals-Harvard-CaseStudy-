
setwd("E:\\IDS 572\\Assg 4")
TraindataM <- read.csv("TraindataMulticlass.csv")
TestdataM <- read.csv("TestDataMulticlass.csv")
str(TestdataM)
ncol(Testdata)
View(Testdata)

#TraindataM$NPS_Status <- NULL
for(i in 13:47) {
  TraindataM[,i] = factor(TraindataM[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

TraindataM$target <- TraindataM$NPS_Status
TraindataM$target <- as.factor(TraindataM$target)
summary(Traindata$target)
str(Traindata$target)
str(TraindataM)
ncol(TraindataM)
#View(TraindataM)


TraindataM$SN <- NULL
TraindataM$AdmissionDate <-NULL
TraindataM$DischargeDate <-NULL
TraindataM$NPS_Status <- NULL


TraindataM_1<-TraindataM
TraindataM_1$MaritalStatus <- NULL
TraindataM_1$BedCategory <- NULL
TraindataM_1$State <- NULL
TraindataM_1$Country <- NULL
TraindataM_1$EM_NURSING <- NULL
TraindataM_1$EM_DOCTOR <- NULL
TraindataM_1$DOC_ATTITUDE <- NULL
TraindataM_1$NS_NURSESATTITUDE <- NULL
TraindataM_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataM_1$CE_NPS <- NULL

ncol(TraindataM_1)
#summary(TraindataM_1$target)
#39 columns 
str(TraindataM_1)

#Testdata
for(i in 13:47) {
  TestdataM[,i] = factor(TestdataM[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataM$NPS_Status)
TestdataM$target <- TestdataM$NPS_Status
TestdataM$target <- as.factor(TestdataM$target)
summary(TestdataM$target)
str(TestdataM$target)

str(TestdataM)



TestdataM$SN <- NULL
TestdataM$AdmissionDate <-NULL
TestdataM$DischargeDate <-NULL
TestdataM$NPS_Status <- NULL

#Quasi static variables
TestdataM_1<-TestdataM
TestdataM_1$MaritalStatus <- NULL
TestdataM_1$BedCategory <- NULL
TestdataM_1$State <- NULL
TestdataM_1$Country <- NULL
TestdataM_1$EM_NURSING <- NULL
TestdataM_1$EM_DOCTOR <- NULL
TestdataM_1$DOC_ATTITUDE <- NULL
TestdataM_1$NS_NURSESATTITUDE <- NULL
TestdataM_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataM_1$CE_NPS <- NULL



#TraindataM$SN <- NULL
#TraindataM$AdmissionDate <-NULL
#TraindataM$DischargeDate <-NULL

str(TraindataM_1)
str(TestdataM_1)

levels(TestdataM_1$InsPayorcategory)<-levels(TraindataM_1$InsPayorcategory)
summary(TestdataM_1$InsPayorcategory)

#-------------------------------------------------------------------Rabdom Forest Model with Multi class 
library(randomForest)
model_RF_Multi<- randomForest(target~.,data=TraindataM_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Multi)

library(caret)
Predict_Test_RF_Multi<-predict(model_RF_Multi,TestdataM_1,type = "class")
#View(upsample)

confusionMatrix_RF_Multi <- confusionMatrix(Predict_Test_RF_Multi,TestdataM_1$target)
confusionMatrix_RF_Multi

Sensitivity_RF_Multi <- confusionMatrix_RF_Multi$table[1,1]/sum(confusionMatrix_RF_Multi$table[1,1],confusionMatrix_RF_Multi$table[2,1],confusionMatrix_RF_Multi$table[3,1])
Sensitivity_RF_Multi*100
#47.72727


# Confusion Matrix and Statistics
# 
# Reference
# Prediction  Detractor Passive Promotor
# Detractor        21       9        1
# Passive          11      50       26
# Promotor         12      58      176
# 
# Overall Statistics
# 
# Accuracy : 0.6786          
# 95% CI : (0.6279, 0.7263)
# No Information Rate : 0.5577          
# P-Value [Acc > NIR] : 1.617e-06       
# 
# Kappa : 0.4003          
# 
# Mcnemar's Test P-Value : 7.538e-05       
# 
# Statistics by Class:
# 
#                      Class: Detractor Class: Passive Class: Promotor
# Sensitivity                   0.47727         0.4274          0.8670
# Specificity                   0.96875         0.8502          0.5652
# Pos Pred Value                0.67742         0.5747          0.7154
# Neg Pred Value                0.93093         0.7581          0.7712
# Prevalence                    0.12088         0.3214          0.5577
# Detection Rate                0.05769         0.1374          0.4835
# Detection Prevalence          0.08516         0.2390          0.6758
# Balanced Accuracy             0.72301         0.6388          0.7161
#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost for Multi class
install.packages("adabag")
library(adabag)

adaboost_Multi <- boosting(target~., data=TraindataM_1, boos=TRUE, 
                            mfinal=10)

Predict_Test_Adaboost_Multi<-predict(adaboost_Multi,TestdataM_1)#,type="class")
#View(upsample)

Predict_Test_Adaboost_Multi$confusion
# Observed Class
# Predicted Class Detractor Passive Promotor
# Detractor        19      10        1
# Passive          11      33       17
# Promotor         14      74      185

#Accuracy is 67.2

Sensitivity_Adaboost_Multi <- Predict_Test_Adaboost_Multi$confusion[1,1]/sum(Predict_Test_Adaboost_Multi$confusion[1,1],Predict_Test_Adaboost_Multi$confusion[2,1],Predict_Test_Adaboost_Multi$confusion[3,1])
Sensitivity_Adaboost_Multi*100
#47.72727

# str(TraindataM_2$target)
# str(Testdata$target)

#------------------------------------------------------------------------------------





#-------------------------------------Applying Random Forest and Adaptive boosting to Binary Class

setwd("E:\\IDS 572\\Assg 4")
TraindataB <- read.csv("TraindataMulticlass.csv")
TestdataB <- read.csv("TestDataMulticlass.csv")

str(TestdataB)
ncol(TestdataB)
#View(TestdataB)

#TraindataB$NPS_Status <- NULL
for(i in 13:47) {
  TraindataB[,i] = factor(TraindataB[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

TraindataB$target <- ifelse(TraindataB$NPS_Status=="Detractor","Detractor","Not_Detractors")
TraindataB$target <- as.factor(TraindataB$target)
summary(TraindataB$target)
str(TraindataB$target)
str(TraindataB)
ncol(TraindataB)
#View(TraindataB)


TraindataB$SN <- NULL
TraindataB$AdmissionDate <-NULL
TraindataB$DischargeDate <-NULL
TraindataB$NPS_Status <- NULL


TraindataB_1<-TraindataB
TraindataB_1$MaritalStatus <- NULL
TraindataB_1$BedCategory <- NULL
TraindataB_1$State <- NULL
TraindataB_1$Country <- NULL
TraindataB_1$EM_NURSING <- NULL
TraindataB_1$EM_DOCTOR <- NULL
TraindataB_1$DOC_ATTITUDE <- NULL
TraindataB_1$NS_NURSESATTITUDE <- NULL
TraindataB_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataB_1$CE_NPS <- NULL

ncol(TraindataB_1)
#summary(TraindataB_1$target)
#39 columns 


#Testdata
for(i in 13:47) {
  TestdataB[,i] = factor(TestdataB[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataB$NPS_Status)
TestdataB$target <- ifelse(TestdataB$NPS_Status=="Detractor","Detractor","Not_Detractors")
TestdataB$target <- as.factor(TestdataB$target)
summary(TestdataB$target)
str(TestdataB$target)

str(TestdataB)



TestdataB$SN <- NULL
TestdataB$AdmissionDate <-NULL
TestdataB$DischargeDate <-NULL
TestdataB$NPS_Status <- NULL

TestdataB_1<-TestdataB
TestdataB_1$MaritalStatus <- NULL
TestdataB_1$BedCategory <- NULL
TestdataB_1$State <- NULL
TestdataB_1$Country <- NULL
TestdataB_1$EM_NURSING <- NULL
TestdataB_1$EM_DOCTOR <- NULL
TestdataB_1$DOC_ATTITUDE <- NULL
TestdataB_1$NS_NURSESATTITUDE <- NULL
TestdataB_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataB_1$CE_NPS <- NULL



#TraindataB$SN <- NULL
#TraindataB$AdmissionDate <-NULL
#TraindataB$DischargeDate <-NULL

str(TraindataB_1)
str(TestdataB_1)

levels(TestdataB_1$InsPayorcategory)<-levels(TraindataB_1$InsPayorcategory)
summary(TestdataB_1$InsPayorcategory)

#-------------------------------------------------------------------Rabdom Forest Model with binary class 
library(randomForest)
model_RF_Binary<- randomForest(target~.,data=TraindataB_1,ntree=500,mtry=8,importance=TRUE,verbose=TRUE)#,proximity=TRUE)
summary(model_RF_Binary)

library(caret)
Predict_Test_RF_Binary<-predict(model_RF_Binary,TestdataB_1,type = "class")
#View(upsample)

confusionMatrix_RF_Binary <- confusionMatrix(Predict_Test_RF_Binary,TestdataB_1$target)
confusionMatrix_RF_Binary

Sensitivity_RF_Binary <- confusionMatrix_RF_Binary$table[1,1]/sum(confusionMatrix_RF_Binary$table[1,1],confusionMatrix_RF_Binary$table[2,1])
Sensitivity_RF_Binary*100
#45.45455

# Confusion Matrix and Statistics
# 
# Reference
# Prediction       Detractor Not_Detractors
# Detractor             20              8
# Not_Detractors        24            312
# 
# Accuracy : 0.9121          
# 95% CI : (0.8782, 0.9391)
# No Information Rate : 0.8791          
# P-Value [Acc > NIR] : 0.02832         
# 
# Kappa : 0.5094          
# 
# Mcnemar's Test P-Value : 0.00801         
#                                           
#             Sensitivity : 0.45455         
#             Specificity : 0.97500         
#          Pos Pred Value : 0.71429         
#          Neg Pred Value : 0.92857         
#              Prevalence : 0.12088         
#          Detection Rate : 0.05495         
#    Detection Prevalence : 0.07692         
#       Balanced Accuracy : 0.71477         
#                                           
#        'Positive' Class : Detractor
#----------------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------AdaBoost for binary class
#install.packages("adabag")
library(adabag)

adaboost_Binary <- boosting(target~., data=TraindataB_1, boos=TRUE, 
                            mfinal=10)

Predict_Test_Adaboost_Binary<-predict(adaboost_Binary,TestdataB_1)#,type="class")
#View(upsample)

Predict_Test_Adaboost_Binary$confusion

# Observed Class
# Predicted Class  Detractor Not_Detractors
# Detractor             17             12
# Not_Detractors        27            308

#Accuracy-89%

Sensitivity_Adaboost_Binary <- Predict_Test_Adaboost_Binary$confusion[1,1]/sum(Predict_Test_Adaboost_Binary$confusion[1,1],Predict_Test_Adaboost_Binary$confusion[2,1])
Sensitivity_Adaboost_Binary*100
#38.63636

# str(TraindataB_2$target)
# str(Testdata$target)

#------------------------------------------------------------------------------------



