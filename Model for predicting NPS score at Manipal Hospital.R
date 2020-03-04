setwd("E:\\IDS 572\\Assg 4")
#library(xlsx)
#data <- read.csv("IMB651-XLS-ENG.xlsx",sheetName = "Training Data for Multi-Class M")
Traindata <- read.csv("TrainDataMulticlass.csv")
str(Traindata)
ncol(Traindata)


# converting a multi-class problem to a binary classi???cation problem 
#when the objective is to understand the detractors among the group? Apply logistic regression on a binary classi???cation problem. 
#Use all variables except the ones identi???ed as leading to quasi-complete separation (use step-wise regression to build the model). 
#Keep this in mind that you need to convert the attributes of survey questionnaires to ordinal variables before building the logistic model.

for(i in 13:47) {
  Traindata[,i] = factor(Traindata[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(Traindata$NPS_Status)
Traindata$target <- ifelse(Traindata$NPS_Status =="Detractor","Detractor","Not_Detractors")
Traindata$target <- as.factor(Traindata$target)
summary(Traindata$target)
str(Traindata$target)

Traindata$NPS_Status <- NULL
Traindata$SN <- NULL
Traindata$AdmissionDate <-NULL
Traindata$DischargeDate <-NULL

library(caret)
lgm <- glm(target~., data = Traindata, family = 'binomial')
summary(lgm)
#View(Variab)
Variab <- data.frame(lgm$coefficients)
a <- as.vector(rownames(Variab))
str(a)
Variables1 <- data.frame(a,round(Variab,2))
#View(Variables1)
colnames(Variables1) <- c("variable","coefficient")
#View(Variables1)
Quasistatic_Variables=as.data.frame(Variables1[abs(Variables1$coefficient) > 20,])
nrow(Quasistatic_Variables)
#View(Quasistatic_Variables)

#Hence we can remove the fllowing variables which are quasi complete
RemovingVariables <- c("MaritalStatus","BedCategory","State","Country","EM_NURSING","EM_DOCTOR","DOC_ATTITUDE","NS_NURSESATTITUDE","OVS_OVERALLSTAFFATTITUDE","CE_NPS")
ncol(Traindata)
#Removing Variables  
# # Traindata_1 <- Traindata
# # a=c()
# # for (i in RemovingVariables)
# # {
# # #Traindata_1 <-Traindata[ ,-RemovingVariables]
# # #Remove <-which(colname==RemovingVariables) 
# # Remove <-which(colname==RemovingVariables) 
# # 
# # b=c(a,Remove)
# # }
# colname <- colnames(Traindata_1)
# colname=data.frame(colname)
# Remove <-which(colname==RemovingVariables) 
# 
# ncol(Traindata_1)
# 
# Traindata[,RemovingVariables]

Traindata_1 <- Traindata

Traindata_1$MaritalStatus <- NULL
Traindata_1$BedCategory <- NULL
Traindata_1$State <- NULL
Traindata_1$Country <- NULL
Traindata_1$EM_NURSING <- NULL
Traindata_1$EM_DOCTOR <- NULL
Traindata_1$DOC_ATTITUDE <- NULL
Traindata_1$NS_NURSESATTITUDE <- NULL
Traindata_1$OVS_OVERALLSTAFFATTITUDE <- NULL
Traindata_1$CE_NPS <- NULL

ncol(Traindata_1)

lgm_New <- glm(target~., data = Traindata_1, family = 'binomial')
summary(lgm_New)

#lgm_New$coefficients[lgm_New$]

#Variables with p-Value below 0.05
ImpVariables <- c("Department","STATEZONE","CE_ACCESSIBILITY","CE_CSAT","CE_VALUEFORMONEY","AD_TARRIFFPACKAGESEXPLAINATION","INR_ROOMPEACE","FNB_STAFFATTITUDE","AE_ATTENDEEFOOD","DOC_VISITS")

Traindata_2 <-Traindata_1[,c(ImpVariables,"target")]
#View(Traindata_2)

#Again applyong model on important variables
lgm_2 <- glm(target~., data = Traindata_2, family = 'binomial')
summary(lgm_2)


Testdata <- read.csv("TestDataMulticlass.csv")

Testdata1 <- Testdata

for(i in 13:47) {
  Testdata1[,i] = factor(Testdata1[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}
Testdata1$target <- ifelse(Testdata1$NPS_Status=="Detractor","Detractor","Not_Detractors")
Testdata1$target <- as.factor(Testdata1$target)
summary(Testdata1$target)

variablesfromtrain <- colnames(Traindata_2)

Testdata1 <- Testdata1[,variablesfromtrain]

#Removing the variable NS_NURSEPROACTIVENESS since there is no training data for class zero of this variable in train data
Traindata_2$NS_NURSEPROACTIVENESS <- NULL
Testdata1$NS_NURSEPROACTIVENESS <- NULL

Predict_Test_glm_binary<-predict(lgm_2,newdata = Testdata1)
#View(upsample)

#make prediction
probability_glmFonal <-predict(lgm_2,Testdata1, type ="response")
predicted_glmFinal <- ifelse(probability_glmFonal > .6, "Not_Detractors","Detractor")
predicted_glmFinal

observed_classes_glm_Final <- Traindata_2$target
Accuracy_glm_Final <- mean(predicted_glmFinal == observed_classes_glm_Final)
Accuracy_glm_Final
#0.8232111
#So the accuracy of GLM Model with only 11 important (with p-Value below 0.05) is 82.3%


str(Testdata$target)


#-------------------------------------------------------------------For stepwise 
setwd("E:\\IDS 572\\Assg 4")
TraindataB_stepwise <- read.csv("TraindataMulticlass.csv")
TestdataB_stepwise <- read.csv("TestDataMulticlass.csv")

str(TestdataB_stepwise)
ncol(TestdataB_stepwise)
#View(TestdataB_stepwise)

#TraindataB_stepwise$NPS_Status <- NULL
for(i in 13:47) {
  TraindataB_stepwise[,i] = factor(TraindataB_stepwise[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

TraindataB_stepwise$target <- ifelse(TraindataB_stepwise$NPS_Status=="Detractor","Detractor","Not_Detractors")
TraindataB_stepwise$target <- as.factor(TraindataB_stepwise$target)
summary(TraindataB_stepwise$target)
str(TraindataB_stepwise$target)
str(TraindataB_stepwise)
ncol(TraindataB_stepwise)
#View(TraindataB_stepwise)


TraindataB_stepwise$SN <- NULL
TraindataB_stepwise$AdmissionDate <-NULL
TraindataB_stepwise$DischargeDate <-NULL
TraindataB_stepwise$NPS_Status <- NULL

#Removing Quasistatic variables 
RemovingVariables <- c("MaritalStatus","BedCategory","State","Country","EM_NURSING","EM_DOCTOR","DOC_ATTITUDE","NS_NURSESATTITUDE","OVS_OVERALLSTAFFATTITUDE","CE_NPS")

TraindataB_stepwise_1<-TraindataB_stepwise
TraindataB_stepwise_1$MaritalStatus <- NULL
TraindataB_stepwise_1$BedCategory <- NULL
TraindataB_stepwise_1$State <- NULL
TraindataB_stepwise_1$Country <- NULL
TraindataB_stepwise_1$EM_NURSING <- NULL
TraindataB_stepwise_1$EM_DOCTOR <- NULL
TraindataB_stepwise_1$DOC_ATTITUDE <- NULL
TraindataB_stepwise_1$NS_NURSESATTITUDE <- NULL
TraindataB_stepwise_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TraindataB_stepwise_1$CE_NPS <- NULL

ncol(TraindataB_stepwise_1)
#summary(TraindataB_stepwise_1$target)
#39 columns 



#Testdata
for(i in 13:47) {
  TestdataB_stepwise[,i] = factor(TestdataB_stepwise[,i],levels= c("1","2","3","4"),ordered = TRUE)
  
}

summary(TestdataB_stepwise$NPS_Status)
TestdataB_stepwise$target <- ifelse(TestdataB_stepwise$NPS_Status=="Detractor","Detractor","Not_Detractors")
TestdataB_stepwise$target <- as.factor(TestdataB_stepwise$target)
summary(TestdataB_stepwise$target)
str(TestdataB_stepwise$target)

str(TestdataB_stepwise)

#Removing  Variables which are not significant
TestdataB_stepwise$SN <- NULL
TestdataB_stepwise$AdmissionDate <-NULL
TestdataB_stepwise$DischargeDate <-NULL
TestdataB_stepwise$NPS_Status <- NULL

#Removing  Variables which are quasi complete
TestdataB_stepwise_1<-TestdataB_stepwise
TestdataB_stepwise_1$MaritalStatus <- NULL
TestdataB_stepwise_1$BedCategory <- NULL
TestdataB_stepwise_1$State <- NULL
TestdataB_stepwise_1$Country <- NULL
TestdataB_stepwise_1$EM_NURSING <- NULL
TestdataB_stepwise_1$EM_DOCTOR <- NULL
TestdataB_stepwise_1$DOC_ATTITUDE <- NULL
TestdataB_stepwise_1$NS_NURSESATTITUDE <- NULL
TestdataB_stepwise_1$OVS_OVERALLSTAFFATTITUDE <- NULL
TestdataB_stepwise_1$CE_NPS <- NULL

ncol(TestdataB_stepwise_1)

#TraindataB_stepwise$SN <- NULL
#TraindataB_stepwise$AdmissionDate <-NULL
#TraindataB_stepwise$DischargeDate <-NULL

str(TraindataB_stepwise_1)
str(TestdataB_stepwise_1)

levels(TestdataB_stepwise_1$InsPayorcategory)<-levels(TraindataB_stepwise_1$InsPayorcategory)
summary(TestdataB_stepwise_1$InsPayorcategory)

summary(TraindataB_stepwise_1$NS_NURSEPROACTIVENESS)
summary(TestdataB_stepwise_1$NS_NURSEPROACTIVENESS)

#levels(TestdataB_stepwise_1$NS_NURSEPROACTIVENESS)<-levels(TraindataB_stepwise_1$NS_NURSEPROACTIVENESS)
#summary(TestdataB_stepwise_1$NS_NURSEPROACTIVENESS)

#As there is no data for score 1 in train data this is showing error when prediction is run for stepwise model so remove this column    ***********
TraindataB_stepwise_1$NS_NURSEPROACTIVENESS <- NULL
TestdataB_stepwise_1$NS_NURSEPROACTIVENESS <- NULL

###################################### Stepwise model building on test data#####################
str(Test_multi)
str(TraindataB_stepwise_1)
library(MASS)
library(dplyr)
#lgm_New <- step(glm(target~., data = Traindata_1, family = 'binomial'))
lgm_New_1 <- glm(target~., data = TraindataB_stepwise_1, family = 'binomial')
step_model <- lgm_New_1 %>% stepAIC(trace = 1, direction = c("both"))
summary(step_model)


#checking Prediction from Stepwise Model

#make prediction
probabilitystep <-predict(step_model,TestdataB_stepwise_1, type ="response")
predicted_Class_step <- ifelse(probabilitystep > .6, "Not_Detractors","Detractor")


observed_classes_step <- TraindataB_stepwise_1$target
Accuracy_stepwise <- mean(predicted_Class_step == observed_classes_step)
Accuracy_stepwise
#Accuracy of stepwise model
#89.28 %


#Random Forest and Adaboost for Multiclass and Binary Class

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

#Rabdom Forest Model with Multi class 
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
#


#AdaBoost for Multi class
#install.packages("adabag")
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

#Applying Random Forest and Adaptive boosting to Binary Class

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

#-Rabdom Forest Model with binary class 
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
#


#AdaBoost for binary class
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


#-------------------------------RF and Adaboost on SMOTE data
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


#Testdata
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

#Random Forest Model with Target variable having Binary class and Balanced using SMOTE 

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
#


#AdaBoost Model with Target variable having Binary class and Balanced using SMOTE 
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


#-------------------------------------Downsampling the Data and applying Random Forest and Adaptive boosting

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

#ownsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataM_Downsample <- downSample(x=TraindataM_Downsample[,-ncol(TraindataM_Downsample)],y=TraindataM_Downsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataM_Downsample$Class)
TraindataM_Downsample$target<- TraindataM_Downsample$Class
TraindataM_Downsample$Class <- NULL

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


#Testdata
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

#Rabdom Forest Model on DOwnsampled data with Target variable having Multiple class 

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


#AdaBoost Model with downsampled data with Target variable having Multiple class
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

#Random FOrest and Adaboost on downsampled data with Binary Class

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

#Downsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataB_Downsample <- downSample(x=TraindataB_Downsample[,-ncol(TraindataB_Downsample)],y=TraindataB_Downsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataB_Downsample$Class)
TraindataB_Downsample$target<- TraindataB_Downsample$Class
TraindataB_Downsample$Class <- NULL
#

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


#Testdata
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


#Rabdom Forest Model with Target variable having Binary class 
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


#AdaBoost Model on Downsampled data with Target variable having Binary class
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


#-----------------------------------------------------------RandomForest & Adaboost for Multiclass and Binary Class Upsample

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

#psample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataM_Upsample <- upSample(x=TraindataM_Upsample[,-ncol(TraindataM_Upsample)],y=TraindataM_Upsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataM_Upsample$target)
TraindataM_Upsample$target<- TraindataM_Upsample$Class
TraindataM_Upsample$Class <- NULL


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


#Testdata
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

#Random Forest Model with Target variable having Multiple class and Balanced using Upsample
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

#-----------------------------------------------AdaBoost Model on Upsample data with Target variable having Multiple class and Balanced using Upsample
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

#Random FOrest and Adaboost on Upsample data with target Variable having Binary Class 

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


#Upsample Classifier fot handelling imbalance(it balances the number of rows for each class)
library(caret)
#upsampling to balance imbalnced data sets..................
TraindataB_Upsample <- upSample(x=TraindataB_Upsample[,-ncol(TraindataB_Upsample)],y=TraindataB_Upsample$target)
#summary(balancedata$Training_final_265.target) 
summary(TraindataB_Upsample$Class)
TraindataB_Upsample$target<- TraindataB_Upsample$Class
TraindataB_Upsample$Class <- NULL


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


#-Testdata
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


#-Random Forest Model with Target variable having Binary class and Balanced using Upsample
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



#AdaBoost Model on Upsampled data with Target variable having Binary class and Balanced using Upsample
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



