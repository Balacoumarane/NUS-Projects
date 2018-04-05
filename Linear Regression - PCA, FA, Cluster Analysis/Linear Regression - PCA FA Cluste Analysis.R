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
library(cluster) 

# Load data
park_data <- read.csv("telemonitoring_parkinsons_updrs_data.csv",header=T)
names(park_data)

# Summary and String of data
summary(park_data)
str(park_data)

# Remove duplicates from the population
park_data <- unique(park_data[ , 1:22])

# Find if any missing population is there
missing_population = park_data %>%
  filter(!complete.cases(.))
nrow(missing_population)
#---
non_missing_population = park_data %>%
  filter(complete.cases(.))
nrow(non_missing_population)

#target vs independent
data_park = melt(park_data[, sapply(park_data, is.numeric)], id='total_updrs')
#---
ggplot(data_park, aes(x = value,y=total_updrs ))+
  geom_point()+ facet_wrap(~variable, scales="free")

#corplot of numeric variables
corrplot(cor(park_data[sapply(park_data, is.numeric)]), method = "number", type='upper')
corrplot(cor(park_data[sapply(park_data, is.numeric)]), method = "circle", type='upper')

## QQ-Plot
qqnorm(park_data$total_updrs)

#outlier
quantiles <- quantile(park_data$motor_updrs, probs = c(.01, .99))
range <- 1.5 * IQR(park_data$motor_updrs)
park_data_new <- subset(park_data,
                     park_data$motor_updrs > (quantiles[1] - range) & park_data$motor_updrs < (quantiles[2] + range))

quantiles <- quantile(park_data_new$total_updrs, probs = c(.25, .75))
range <- 1.5 * IQR(park_data_new$total_updrs)
park_data_new <- subset(park_data_new,
                        park_data_new$total_updrs > (quantiles[1] - range) & park_data_new$total_updrs < (quantiles[2] + range))

quantiles <- quantile(park_data_new$jitter, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$jitter)
park_data_new <- subset(park_data_new,
                        park_data_new$jitter > (quantiles[1] - range) & park_data_new$jitter < (quantiles[2] + range))

quantiles <- quantile(park_data_new$jitter_abs, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$jitter_abs)
park_data_new <- subset(park_data_new,
                        park_data_new$jitter_abs > (quantiles[1] - range) & park_data_new$jitter_abs < (quantiles[2] + range))

quantiles <- quantile(park_data_new$jitter_rap, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$jitter_rap)
park_data_new <- subset(park_data_new,
                        park_data_new$jitter_rap > (quantiles[1] - range) & park_data_new$jitter_rap < (quantiles[2] + range))

quantiles <- quantile(park_data_new$jitter_ppq5, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$jitter_ppq5)
park_data_new <- subset(park_data_new,
                        park_data_new$jitter_ppq5 > (quantiles[1] - range) & park_data_new$jitter_ppq5 < (quantiles[2] + range))

quantiles <- quantile(park_data_new$jitter_ddp, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$jitter_ddp)
park_data_new <- subset(park_data_new,
                        park_data_new$jitter_ddp > (quantiles[1] - range) & park_data_new$jitter_ddp < (quantiles[2] + range))

quantiles <- quantile(park_data_new$shimmer, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$shimmer)
park_data_new <- subset(park_data_new,
                        park_data_new$shimmer > (quantiles[1] - range) & park_data_new$shimmer < (quantiles[2] + range))

quantiles <- quantile(park_data_new$shimmer_db, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$shimmer_db)
park_data_new <- subset(park_data_new,
                        park_data_new$shimmer_db > (quantiles[1] - range) & park_data_new$shimmer_db < (quantiles[2] + range))

quantiles <- quantile(park_data_new$shimmer_apq3, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$shimmer_apq3)
park_data_new <- subset(park_data_new,
                        park_data_new$shimmer_apq3 > (quantiles[1] - range) & park_data_new$shimmer_apq3 < (quantiles[2] + range))

quantiles <- quantile(park_data_new$shimmer_apq5, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$shimmer_apq5)
park_data_new <- subset(park_data_new,
                        park_data_new$shimmer_apq5 > (quantiles[1] - range) & park_data_new$shimmer_apq5 < (quantiles[2] + range))

quantiles <- quantile(park_data_new$shimmer_apq11, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$shimmer_apq11)
park_data_new <- subset(park_data_new,
                        park_data_new$shimmer_apq11 > (quantiles[1] - range) & park_data_new$shimmer_apq11 < (quantiles[2] + range))

quantiles <- quantile(park_data_new$shimmer_dda, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$shimmer_dda)
park_data_new <- subset(park_data_new,
                        park_data_new$shimmer_dda > (quantiles[1] - range) & park_data_new$shimmer_dda < (quantiles[2] + range))

quantiles <- quantile(park_data_new$nhr, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$nhr)
park_data_new <- subset(park_data_new,
                        park_data_new$nhr > (quantiles[1] - range) & park_data_new$nhr < (quantiles[2] + range))

quantiles <- quantile(park_data_new$hnr, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$hnr)
park_data_new <- subset(park_data_new,
                        park_data_new$hnr > (quantiles[1] - range) & park_data_new$hnr < (quantiles[2] + range))

quantiles <- quantile(park_data_new$rpde, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$rpde)
park_data_new <- subset(park_data_new,
                        park_data_new$rpde > (quantiles[1] - range) & park_data_new$rpde < (quantiles[2] + range))

quantiles <- quantile(park_data_new$dfa, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$dfa)
park_data_new <- subset(park_data_new,
                        park_data_new$dfa > (quantiles[1] - range) & park_data_new$dfa < (quantiles[2] + range))

quantiles <- quantile(park_data_new$ppe, probs = c(.01, .99))
range <- 1.5 * IQR(park_data_new$ppe)
park_data_new <- subset(park_data_new,
                        park_data_new$ppe > (quantiles[1] - range) & park_data_new$ppe < (quantiles[2] + range))

# Write the file
write.csv(park_data_new ,file='parkison_No_outlier.csv')

# PCA
pc_data<-princomp(data_1, cor = T, scores = TRUE)
summary(pc_data)
loadings(pc_data)

# Eigen value
ev <- eigen(cor(data_1))
str(ev)

# Scree plot
screeplot(pc_data,type = 'line',main = 'ScreePlot')
plot(pc_data)
View(pc_data)

# Factor loadings 
fc_data<-principal(data_1, nfactors = 4,rotate ="varimax",scores =T)
summary(fc_data)
str(fc_data)
fc_scores<-fc_data$scores
str(fc_scores1)
fc_scores1<- unclass(fc_scores)
fc_scores2<-as.data.frame(fc_scores)
ld<-loadings(fc_data)
str(ld)
class(ld)
ld_unclas<-unclass(ld)
xtable(unclass(ld))
fc_data1<-as.data.frame(ld)

# Clustering
cluster_data <- kmeans(fc_scores2, 3)
aggregate(fc_scores2,by=list(cluster_data$cluster),FUN=mean)

# Cluster plots
clusplot(fc_scores2, cluster_data$cluster, color=T, shade=F, lines=0)
data_1<-cbind(data_1,clusterNum = cluster_data$cluster)
cluster_data<- data.frame(cluster_data, cluster_data$cluster)

# linear regression
data_1<-cbind(park_data,updrs_data)
model<-lm(total_updrs ~ Factor1+Factor2+Factor3+Factor4+sex, data=data_1)
summary(model)
