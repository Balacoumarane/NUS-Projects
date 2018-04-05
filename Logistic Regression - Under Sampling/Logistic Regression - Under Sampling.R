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
library(dplyr)
library(MKmisc)
library(ResourceSelection)
library(lmtest)
library(aod)
library(unbalanced)
library(ROSE)

# Import dataset
bank_population <- read.table("bank-additional-full.txt",header=TRUE,sep='\t')

# Details about imported Dataset
names(bank_population)
head(bank_population)
dim(bank_population)
summary(bank_population)
str(bank_population)

# Find if any missing population is there
missing_population = bank_population %>%
  filter(!complete.cases(.))
nrow(missing_population)
#---
non_missing_population = bank_population %>%
  filter(complete.cases(.))
nrow(non_missing_population)

# Adding new variable 'ynum' as 'yes=1 & no=0'
bank_population <- bank_population %>%
  mutate(ynum = ifelse(bank_population$y == "no",0,1))

# Remove duplicates from the population
bank_population <- unique(bank_population[ , 1:22])

# Plot the distribution of each numeric variable in population
bank_population %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Plot target variable V/s numeric variables in population
data_long_bank = melt(bank_population[, sapply(bank_population, is.numeric)], id='ynum')
#---
ggplot(data_long_bank, aes(x = value, group=ynum, color=factor(ynum)))+
  geom_density()+ facet_wrap(~variable, scales="free")

# under-sampling 
bank_under_sample <- ovun.sample(y~.,data=bank_population, N=9278, seed=10, method='under')$data
summary(bank_under_sample)
bank_under_sample <- unique(bank_under_sample[ , 1:22])

########## TO MATCH UNDER SAMPLE & POPULATION - BEGIN
# Plot the distribution of each numeric variable in over sample
bank_under_sample %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Plot target variable V/s numeric variables in under sample
data_bank_uner_sample = melt(bank_under_sample[, sapply(bank_under_sample, is.numeric)], id='ynum')
#---
ggplot(data_bank_under_sample, aes(x = value, group=ynum, color=factor(ynum)))+
  geom_density()+ facet_wrap(~variable, scales="free")
########## TO MATCH SAMPLE & POPULATION - END

# Categorizing pdays and previous into categorical in over-sample
bank_under_sample_cat <- bank_under_sample %>% mutate(previous_cat=cut(previous, breaks=c(-1,0.99,100), labels=c('not contacted','contacted before')))
bank_under_sample_cat <- bank_under_sample_cat %>% mutate(pdays_cat=cut(pdays, breaks=c(-1,998,1000), labels=c('contacted in the previous campaign','never contacted')))

# Split sample into train and test for under-sample
set.seed(7)
splitData = sample.split(bank_under_sample_cat$ynum,SplitRatio = 0.6)
##View(splitData)
train=bank_under_sample_cat[splitData,]
nrow(train)/nrow(bank_under_sample_cat)
test=bank_under_sample_cat[!splitData,]
nrow(test)/nrow(bank_under_sample_cat)

# use train from under-sample to create our model
# Model with all the variables
model_population = glm(train$ynum ~ . -duration -pdays_cat -previous_cat -y, data = train, family = binomial)
summary(model_population)

# Final model after several iterations
model_sample = glm(train$ynum ~ . -previous_cat -education -day_of_week -cons.conf.idx -campaign -marital -housing -loan -nr.employed -duration -default -age -pdays -previous -y, data = train, family = binomial)
summary(model_sample)

#confusion matrix_train under sampling
trainPredict = predict(model_sample, newdata = train, type = 'response')
p_class = ifelse(trainPredict > 0.5, 1,0)
confusionMatrix(p_class, train$ynum, positive = '1')

#AUC TRAIN under sampling
auc = colAUC(trainPredict, train$ynum, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)

#confusion matrix_test under sampling
testPredict = predict(model_sample, newdata = test, type = 'response') 
p_class1 = ifelse(testPredict > 0.5, 1,0)
confusionMatrix(p_class1, test$ynum, positive = '1')

#AUCTEST under sampling
auc = colAUC(testPredict, test$ynum, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)

#Hosmer-Lemeshow Test under samping
HLgof.test(fit = fitted(model_sample), obs = train$ynum)
hoslem.test(train$ynum, fitted(model_sample), g=10)

# Log-likelihood Test
lrtest(model_population, model_sample)