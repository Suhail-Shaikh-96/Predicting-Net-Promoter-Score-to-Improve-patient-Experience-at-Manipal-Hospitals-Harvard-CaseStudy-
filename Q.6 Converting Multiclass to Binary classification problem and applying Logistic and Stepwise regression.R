setwd("E:\\IDS 572\\Assg 4")
#library(xlsx)
#data <- read.csv("IMB651-XLS-ENG.xlsx",sheetName = "Training Data for Multi-Class M")
Traindata <- read.csv("TrainDataMulticlass.csv")
str(Traindata)
ncol(Traindata)


#Q.6)How can we convert a multi-class problem to a binary classi???cation problem 
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

setwd("E:\\IDS 572\\Assg 4")
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


