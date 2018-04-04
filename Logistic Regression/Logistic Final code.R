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

# SMOTE sampling
bank_smote_sample <- SMOTE(y ~ ., bank_population, perc.over = 100, perc.under=200)
summary(bank_smote_sample)
str(bank_smote_sample)

# Remove duplicates in SMOTE
bank_smote_sample <- unique(bank_smote_sample[ , 1:22])

########## TO MATCH SAMPLE & POPULATION - BEGIN
# Plot the distribution of each numeric variable in sample
bank_smote_sample %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Plot target variable V/s numeric variables in sample
data_long_bank_sample = melt(bank_smote_sample[, sapply(bank_smote_sample, is.numeric)], id='ynum')
#---
ggplot(data_long_bank_sample, aes(x = value, group=ynum, color=factor(ynum)))+
  geom_bar()+ facet_wrap(~variable, scales="free")
########## TO MATCH SAMPLE & POPULATION - END

# Categorizing pdays and previous into categorical in sample
bank_smote_sample_cat <- bank_smote_sample %>% mutate(pdays_cat=cut(pdays, breaks=c(-1,998,1000), labels=c('contacted in the previous campaign','never contacted')))
bank_smote_sample_cat <- bank_smote_sample_cat %>% mutate(previous_cat=cut(previous, breaks=c(-1,0.99,100), labels=c('not contacted','contacted before')))

# Removing the columns that are categorized
#remove_cols = c('pdays', 'previous')
#bank_smote_sample_cat = bank_smote_sample_cat %>% 
#  select(-one_of(remove_cols))

# Correlation plot of numeric variables
corrplot(cor(bank_smote_sample_cat[sapply(bank_smote_sample_cat, is.numeric)]), method = "number", type='upper')
 
# Split sample into train and test
set.seed(7)
splitData = sample.split(bank_smote_sample_cat$ynum,SplitRatio = 0.6)
##View(splitData)
train=bank_smote_sample_cat[splitData,]
nrow(train)/nrow(bank_smote_sample_cat)
test=bank_smote_sample_cat[!splitData,]
nrow(test)/nrow(bank_smote_sample_cat)

# use train to create our model
# model_sample = glm(train$ynum ~ . -y -duration -euribor3m -nr.employed -pdays -previous, data = train, family = binomial)
# summary(model_sample)

# model_population = glm(train$ynum ~ . -duration -pdays -previous -y, data = train, family = binomial)
# summary(model_population)

model_sample = glm(train$ynum ~ . -nr.employed -campaign -default -housing -loan -marital -duration -age -previous -pdays -y, data = train, family = binomial)
summary(model_sample)

#confusion matrix_train
trainPredict = predict(model_sample, newdata = train, type = 'response')
p_class = ifelse(trainPredict > 0.5, 1,0)
confusionMatrix(p_class, train$ynum, positive = '1')

#AUC TRAIN
auc = colAUC(trainPredict, train$ynum, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)

#confusion matrix_test
testPredict = predict(model_sample, newdata = test, type = 'response') 
p_class1 = ifelse(testPredict > 0.5, 1,0)
confusionMatrix(p_class1, test$ynum, positive = '1')

#AUCTEST
auc = colAUC(testPredict, test$ynum, plotROC = TRUE)
legend(0.1,0.9, round(auc,4), title= 'AUC', cex=.5)

#Hosmer-Lemeshow Test
HLgof.test(fit = fitted(model_sample), obs = train$ynum)
hoslem.test(train$ynum, fitted(model_sample), g=10) 

# Log-likelihood Test
lrtest(model_population, model_sample)
lrtest(model_sample, model_population)

# wald.test 
coeftest(model_sample)
anova(model_sample,model_population)
wald.test(model_population,model_sample,test="Chisq")