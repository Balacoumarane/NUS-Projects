# load libraries
library(tidyverse) 
library(caret)
library(ROCR)
library(reshape2)
library(car)
library(corrplot)
library(caTools)
library(magrittr)
library(MASS)
library(DMwR)
library(data.table)
library(plyr)
library(dplyr)
library(MKmisc)
library(ResourceSelection)
library(lmtest)
library(aod)
library(unbalanced)
library(ROSE)
library(mice)
library(bnlearn)
library(e1071)
library(readxl)

# Read the input file
vehicle_data <- read.csv("vehicle_safety_NASS2010_2000_2012.csv",header=T)
str(vehicle_data)
names(vehicle_data)

# Imputing missing data using MICE package and 'prediction mean matching' method.
md.pattern(vehicle_data)
tempData <- mice(vehicle_data[,c(1,2,3,4,7,8,10,12,16,20,21)],m=5,maxit=25,meth='pmm',seed=500)
completedData <- complete(tempData,1)
completedData1 <- vehicle_data[,c(5,6,9,11,14,15,17,13)]
summary(completedData)
Data <- cbind(completedData,completedData1)
summary(Data)
names(Data)
str(Data)

vehicle_safety_imp_JMP <- Data

# Changing class to factor variables
cols_to_change = c('GV_WGTCDTR','OA_BAGDEPLY','OA_MANUSE','OA_SEX','VE_GAD1')
sapply(vehicle_safety_imp_JMP[cols_to_change], class)
vehicle_safety_imp_JMP[cols_to_change] = lapply(vehicle_safety_imp_JMP[cols_to_change], factor)
sapply(vehicle_safety_imp_JMP[cols_to_change], class)

# Categorizing the numeric variables
Data_cat <- vehicle_safety_imp_JMP %>% 
  mutate(GV_CURBWGT_cat=cut(GV_CURBWGT, breaks=c(0,1500,3000,4500), labels=c('light wt vehicle','medium wt vehicle','heavy wt vehicle')))

Data_cat <- Data_cat %>%
  mutate(GV_DVLAT_cat=abs(GV_DVLAT)) %>%
  mutate(GV_DVLAT_cat=cut(GV_DVLAT_cat, breaks=c(-1,30,70,120), labels=c('Low speed Lat','Medium speed Lat','High speed Lat')))

Data_cat <- Data_cat %>%
  mutate(GV_DVLONG_cat=abs(GV_DVLONG)) %>%
  mutate(GV_DVLONG_cat=cut(GV_DVLONG_cat, breaks=c(-1,30,70,150), labels=c('Low speed Long','Medium speed Long','High speed Long')))

Data_cat <- Data_cat %>%
  mutate(GV_ENERGY_cat=cut(GV_ENERGY, breaks=c(0,500,2000,10000), labels=c('Low Impact','Medium Impact','High Impact')))

Data_cat <- Data_cat %>% 
  mutate(GV_OTVEHWGT_cat=cut(GV_OTVEHWGT, breaks=c(0,1500,3000,4600), labels=c('light wt vehicle-ot','medium wt vehicle-ot','heavy wt vehicle-ot')))

Data_cat <- Data_cat %>% 
  mutate(GV_SPLIMIT_cat=cut(GV_SPLIMIT, breaks=c(-1,30,50,100), labels=c('low speed limit','medium speed limit','high speed limit')))

Data_cat <- Data_cat %>% 
  mutate(OA_AGE_cat=cut(OA_AGE, breaks=c(-1,20,40,60,100), labels=c('age less than 20','age between 20 & 40','age between 40 & 60','age above 60')))

Data_cat <- Data_cat %>% 
  mutate(OA_HEIGHT_cat=cut(OA_HEIGHT, breaks=c(0,165,180,250), labels=c('Short','Medium','tall')))

Data_cat <- Data_cat %>% 
  mutate(OA_WEIGHT_cat=cut(OA_WEIGHT, breaks=c(0,60,90,160), labels=c('Wt below 60','Wt between 60 & 90','Wt above 90')))

Data_cat <- Data_cat %>% 
  mutate(GV_FOOTPRINT_cat=cut(GV_FOOTPRINT, breaks=c(0,4,5,10), labels=c('Less area','Medium area','Large area')))

Data_cat <- Data_cat %>% 
  mutate(GV_LANES_cat=cut(GV_LANES, breaks=c(0,1.1,2.1,3.1,10), labels=c('Single lane','Double lane','Triple lane','Multi lane')))

Data_cat <- Data_cat %>% 
  mutate(GV_MODELYR_cat=cut(GV_MODELYR, breaks=c(1999,2005,2010,2013), labels=c('Car mftd before 2005','Car mftd between 2005 & 2010','Car mftd after 2010')))

Data_cat <- Data_cat %>% 
  mutate(VE_PDOF_TR_cat=cut(VE_PDOF_TR, breaks=c(0,90,180,270,500), labels=c('left','front','right','rear')))

Data_cat <- Data_cat %>% 
  mutate(OA_MAIS_cat=cut(OA_MAIS, breaks=c(-1,1.9,4.1,6.1), labels=c('Not injured/Minor injury','Moderate/Serious/Severe injury','Critical/Maximum')))

str(Data_cat)

# Imputing NA and blanks in categorical variables
Data_cat$GV_LANES_cat[is.na(Data_cat$GV_LANES_cat)] = 'Double lane'
Data_cat$OA_MANUSE[is.na(Data_cat$OA_MANUSE)] = 1
Data_cat$OA_SEX[is.na(Data_cat$OA_SEX)] = 'Male'
Data_cat$VE_GAD1[is.na(Data_cat$VE_GAD1)] = 'Front'
Data_cat$OA_SEX[Data_cat$OA_SEX!='Male' & Data_cat$OA_SEX!='Female' ] = 'Male'
Data_cat$VE_GAD1[Data_cat$VE_GAD1!='Front' & Data_cat$VE_GAD1!='Left' & 
                   Data_cat$VE_GAD1!='Rear' & Data_cat$VE_GAD1!='Right'] = 'Front'


# Get the categorical variables to a dataset
summary(Data_cat)
names(Data_cat)
Veh_Data <- Data_cat[,c(20,21,22,23,24,25,26,27,28,29,30,31,32,14,15,16,17,18,33)]
str(Veh_Data)
summary(Veh_Data)

Veh_Data <- as.data.frame(Veh_Data)

# Find if any missing population is there
missing_population = Veh_Data %>%
  filter(!complete.cases(.))
nrow(missing_population)
#---
non_missing_population = Veh_Data %>%
  filter(complete.cases(.))
nrow(non_missing_population)

# Split sample into train and test
set.seed(3)
splitData = sample.split(non_missing_population$OA_MAIS,SplitRatio = 0.7)
veh_train=non_missing_population[splitData,]
nrow(veh_train)/nrow(non_missing_population)
veh_test=non_missing_population[!splitData,]
nrow(veh_test)/nrow(non_missing_population)

## Naive bayes
model <- naiveBayes(OA_MAIS_cat ~ ., data = veh_train)
class(model)
summary(model)
print(model)

#confusion matrix of Train
nb_train_predict <- predict(model,veh_train[,-1])
Train_conf <- table(pred=nb_train_predict,true=veh_train$OA_MAIS_cat)

#Accuracy & Precision for each class in Train Data 
Train_accuracy <- sum(diag(Train_conf)) / sum(Train_conf)
Train_precision <- diag(Train_conf) / rowSums(Train_conf)

#Overall stats of the model in train dataset
confusionMatrix(nb_train_predict, veh_train$OA_MAIS_cat, positive = 'Not injured/Minor injury')

#confusion matrix of Test
nb_test_predict <- predict(model,veh_test[,-1])
Test_conf <- table(pred=nb_test_predict,true=veh_test$OA_MAIS_cat)

#Accuracy & Precision for each class in Test Data 
Test_accuracy <- sum(diag(Test_conf)) / sum(Test_conf)
Test_precision <- diag(Test_conf) / rowSums(Test_conf)

#Overall stats of the model in test dataset
confusionMatrix(nb_test_predict, veh_test$OA_MAIS_cat, positive = 'Not injured/Minor injury')