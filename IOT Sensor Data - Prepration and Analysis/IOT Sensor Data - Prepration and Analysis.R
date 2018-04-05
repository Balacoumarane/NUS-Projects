# install/load multiple libraries
pacman :: p_load(tidyverse, reshape2, readxl, jsonlite, corrplot, XLConnect, magrittr)
pacman::p_load(tidyverse, reshape2,magrittr, lubridate, data.table)
library(scales)
library(sqldf)
library(ggrepel)

# Read the files
Data_1 <- read.csv("Assignment_Data.csv",header=T)
Data_2 <- read.csv("Assignment_Data2.csv",header=T)

#append two files
Data_1_2<-rbind(Data_1,Data_2)
names(Data_1_2)

#Rename column date_time to time_1
old_names=c('date_time')
new_names=c('time_1')
setnames(Data_1_2, old=old_names,new=new_names)
setnames(Data_2, old=old_names,new=new_names)

# Get data type of each variable in merged data set 
str(Data_1_2)

# Change data type of date
Data_1$time_1 = ymd_hms(Data_1$time_1)
Data_2$time_1 = ymd_hms(Data_2$time_1)
Data_1_2$time_1 = ymd_hms(Data_1_2$time_1)

# make sure all in same time zones
Data_1$time_1 = force_tz(Data_1$time_1, "Asia/Kuala_Lumpur")
Data_2$time_1 = force_tz(Data_2$time_1, "Asia/Kuala_Lumpur")
Data_1_2$time_1 = force_tz(Data_1_2$time_1, "Asia/Kuala_Lumpur")

# copy to new data frame
Data_merge <- Data_1_2

### Get the time diffrerence of two consequetive recording in whole dataset
# Get the time of next row in new column
Data_1_2 = Data_1_2 %>%
  mutate(time_2 = c(Data_1_2$time_1[-1], NA)) %>%
  na.omit()
# Get the time interval
inter_Data_1_2 <- interval(Data_1_2$time_1,Data_1_2$time_2)
Data_1_2$seconds <- inter_Data_1_2/dseconds()
Data_1_2$minutes <- inter_Data_1_2/dminutes()
Data_1_2$dhours <- inter_Data_1_2/dhours()
Data_1_2$days <- inter_Data_1_2/ddays()
###########################################################################

# Ignoring the record that is showing diff as March 31st
Data_1_2 <- Data_1_2[-c(43085),]

# Get only date from timestamp in Data_1_2
Data_1_2$only_date = as.Date(format(Data_1_2$time_1, format="%Y-%m-%d")) 
Data_1_2$weekday <- weekdays(Data_1_2$only_date)

# Check the class of new variable
cols_to_change = c('weekday','only_date')
sapply(Data_1_2[cols_to_change], class)
# Check the new variable class to fator
Data_1_2[cols_to_change] = lapply(Data_1_2[cols_to_change], factor)
sapply(Data_1_2[cols_to_change], class)

# Get hours from the time of Data_1_2
Data_1_2$time_hours <- substr(Data_1_2$time_1, 12, 13)

# Get minutes from the time of Data_1_2
Data_1_2$time_minutes <- substr(Data_1_2$time_1, 15, 16)

# convert the hours & minutes into integer
Data_1_2$time_hours <- as.integer(Data_1_2$time_hours)
Data_1_2$time_minutes <- as.integer(Data_1_2$time_minutes)

# data types of the variables
str(Data_1_2)

### Plot for getting the number of recorings in each day ###
Data_1_2 %>%
  group_by(only_date) %>%  
  summarise(count=n()) %>%
  ggplot(aes(x=only_date,y=count)) + 
  geom_bar(stat='identity',color='steelblue') + xlab("Date") + ylab("Count of no. of records") + 
  ggtitle("Recordings for each day") +
  theme(axis.text.x=element_text(angle=0, hjust=1)) + 
  geom_text(aes(label=count),color='Red') +
  coord_flip()

# Getting the dates with no. of recording not equal to 1440
Data_1_2 %>%
  group_by(only_date) %>%  
  summarise(count=n()) %>%
  subset(count!=1440)

#########################################################################
## Imputation for missing values greater than 15Min
# Execute the code from here for imputation of missing values
# Initialize the variables
a <- 0
b <- 0
i <- 0
j <- 0
k <- 0
p <- 1
q <- 1
c <- 0
nrows <- nrow(Data_1_2)

# create dummy data frames as per original frile
prev_day_data <- as.data.frame(Data_1_2)
prev_day_data <- prev_day_data[F,]
next_day_data <- as.data.frame(Data_1_2)
next_day_data <- next_day_data[F,]
curr_miss_data <- as.data.frame(Data_1_2)
curr_miss_data <- curr_miss_data[F,c(1:8)]

for (i in 1:nrows) {
  if (Data_1_2$minutes[i]>15) {
    curr_date <- Data_1_2$time_1[i]
    prev_date <- Data_1_2$time_1[i] - 24*60*60
    prev_date <- as.POSIXct(prev_date, format = "%d-%m-%Y %H:%M:%S")
    prev_date_time <- prev_date + Data_1_2$seconds[i]
    prev_date_time <- as.POSIXct(prev_date_time, format = "%d-%m-%Y %H:%M:%S")
    next_date <- Data_1_2$time_1[i] + 24*60*60
    next_date <- as.POSIXct(next_date, format = "%d-%m-%Y %H:%M:%S")
    next_date_time <- next_date + Data_1_2$seconds[i]
    next_date_time <- as.POSIXct(next_date_time, format = "%d-%m-%Y %H:%M:%S")
    for (j in 1:nrows){
      if ((Data_1_2$time_1[j] > prev_date) & (Data_1_2$time_1[j] < prev_date_time)){
        a<-a+1
        prev_day_data[a,] <- Data_1_2[j,]
      }
    }
    for (k in 1:nrows){
      if ((Data_1_2$time_1[k] > next_date) & (Data_1_2$time_1[k] < next_date_time)){
        b<-b+1
        next_day_data[b,] <- Data_1_2[k,]
      }
    }
    if (nrow(prev_day_data) == nrow(next_day_data))
      R1 <- nrow(prev_day_data)
    else if (nrow(prev_day_data) < nrow(next_day_data)) 
      R1 <- nrow(prev_day_data)
    else if (nrow(prev_day_data) > nrow(next_day_data)) 
      R1 <- nrow(next_day_data)
    while (p <= R1) {
      while (q <= R1) {
        if (prev_day_data$time_hours[p] == next_day_data$time_hours[q]){
          c=c+1
          curr_date <- curr_date + 60
          curr_miss_data[c,1] <- curr_date
          curr_miss_data[c,2] <- Data_1_2$unitid[i]
          curr_miss_data[c,3] <- (prev_day_data[q,3] + next_day_data[q,3])/2
          curr_miss_data[c,4] <- (prev_day_data[q,4] + next_day_data[q,4])/2
          curr_miss_data[c,5] <- (prev_day_data[q,5] + next_day_data[q,5])/2
          curr_miss_data[c,6] <- (prev_day_data[q,6] + next_day_data[q,6])/2
          curr_miss_data[c,7] <- (prev_day_data[q,7] + next_day_data[q,7])/2
          curr_miss_data[c,8] <- (prev_day_data[q,8] + next_day_data[q,8])/2
        }
        q=q+1
        p<-q
      }
    }
  }
}
###############################################################################

## details of imputed data
str(curr_miss_data)

# Combine imputed data to original merge data and process
Data_no_miss <-rbind(Data_merge,curr_miss_data)

# Time-zone
Data_no_miss$time_1 = force_tz(Data_no_miss$time_1, "Asia/Kuala_Lumpur")

# sort with datetime column
Data_time_order <- Data_no_miss[order(Data_no_miss$time_1, decreasing = F), ]

# Get the time of next row in new column
Data_Pro = Data_time_order %>%
  mutate(time_2 = c(Data_time_order$time_1[-1], NA)) %>%
  na.omit()
# Get the time interval
inter_Data_1_2 <- interval(Data_Pro$time_1,Data_Pro$time_2)
Data_Pro$seconds <- inter_Data_1_2/dseconds()
Data_Pro$minutes <- inter_Data_1_2/dminutes()
Data_Pro$dhours <- inter_Data_1_2/dhours()
Data_Pro$days <- inter_Data_1_2/ddays()
###########

# Get only date from timestamp in Data_1_2
Data_Pro$only_date = as.Date(format(Data_Pro$time_1, format="%Y-%m-%d")) 
Data_Pro$weekday <- weekdays(Data_Pro$only_date)

# Ignoring the record that is showing diff as March 31st
Data_Pro <- Data_Pro[-c(43192),]

# Get hours from the time of Data_Pro
Data_Pro$time_hours <- substr(Data_Pro$time_1, 12, 13)

# Get hours from the time of Data_Pro
Data_Pro$time_minutes <- substr(Data_Pro$time_1, 15, 16)

# convert the hours into integer
Data_Pro$time_hours <- as.integer(Data_Pro$time_hours)
Data_Pro$time_minutes <- as.integer(Data_Pro$time_minutes)

# data types of the variables
str(Data_Pro)

# Getting the dates with no. of recording not equal to 1440
Data_Pro %>%
  group_by(only_date) %>%  
  summarise(count=n()) %>%
  subset(count!=1440)

# Copy the new data frame into Data_Pro
Data_P <- Data_Pro

#########################################################################################
# After missing value imputation, there are two days with more than 1440 observations
# Delete records with more than 1440 observations per day
# Create below temporary data frames
Temp_df_1 <- as.data.frame(Data_Pro)
Temp_df_1 <- Temp_df_1[F,]
Temp_df_2 <- Temp_df_1[F,]
Data_Pro_1 <- as.data.frame(Data_Pro)
Data_Pro_1 <- Data_Pro_1[F,]
i <- 0
j <- 0
k <- 0
p <- 0
q <- 1
a <- 0
b <- 0
c <- 0
m <- 0
# Get the dates above 1440 observations
Dates_above_1440 <- Data_Pro %>%
  group_by(only_date) %>%  
  summarise(count=n()) %>%
  subset(count>1440)
# Get the observations from those dates which are from same hour and same minute
for (i in 1:nrow(Dates_above_1440)) {
  for (j in 1:nrow(Data_Pro)) {
    if (Dates_above_1440$only_date[i] == Data_Pro$only_date[j]) {
      a <- a + 1
      Temp_df_1[a,] <- Data_Pro[j,]
    }
  }
  for (k in 1:(nrow(Temp_df_1)-1)) {
    if ((Temp_df_1$time_hours[k] == Temp_df_1$time_hours[k+1]) &  
        (Temp_df_1$time_minutes[k] == Temp_df_1$time_minutes[k+1])) { 
      b <- b +1
      Temp_df_2[b,] <- Temp_df_1[k,]
    }
  }
}
Temp_df_2 <- unique(Temp_df_2[ , 1:17])
# Remove those observations from the whole data
for (p in 1:nrow(Temp_df_2)) {
  while (q <= nrow(Data_Pro)) {
    if (Temp_df_2$time_1[p] != Data_Pro$time_1[q]) { 
      c <- c + 1
      Data_Pro_1[c,] <- Data_Pro[q,] }
    else if (Temp_df_2$time_1[p] == Data_Pro$time_1[q]) {
      m <- q + 1
      if (p!=nrow(Temp_df_2)) {break} 
    }
    q <- q +1
  }
  q <- m
}

### Plot for getting the number of recorings in each day ###
Data_Pro_1 %>%
  group_by(only_date) %>%  
  summarise(count=n()) %>%
  ggplot(aes(x=only_date,y=count)) + 
  geom_bar(stat='identity',color='steelblue') + xlab("Date") + ylab("Count of no. of records") + 
  ggtitle("Recordings for each day") +
  theme(axis.text.x=element_text(angle=0, hjust=1)) + 
  geom_text(aes(label=count),color='Red') +
  coord_flip()

# Copy the new data frame into Data_Pro
Data_Pro <- Data_Pro_1

#########################################################################################
#########################################################################################
############## Duplications in each variable ############## 
###########################################################
# Create a temperarory data frame as below.
Temp_data_frame <- as.data.frame(Data_Pro)
Temp_data_frame <- Temp_data_frame[F,c(1:8)]
count <- data.frame(Count=integer())
Temp_data_frame <- cbind(Temp_data_frame, count)

##  ##  ## ## ## ## ## ##  ##  ##  ## ## ## ## ## ##  ##  ##  ## ## ## ## 
# Function for Identifyin the duplicate values greater than a Threshold
# and Store in a Temporary Data frame
Identify_dup <- function(Variable,Threshold,i,k,a,b,c){
nrows <- nrow(Data_Pro)
Temp_data_frame <- Temp_data_frame[F,]
while (i < nrows) {
  while (k==1) {
    if (Data_Pro[[Variable]][i] == Data_Pro[[Variable]][i+1]) {
      c <- c + 1
      i <- i + 1 
      if (i == nrows) {
        if (c > Threshold) {
          a <- a + 1
          Temp_data_frame[a,c(1:8)] <- Data_Pro[i,c(1:8)]
          Temp_data_frame[a,9] <- c }
        k <- 0 } }
    else if (Data_Pro[[Variable]][i] != Data_Pro[[Variable]][i+1]) {
      if (c > Threshold) { 
        a <- a + 1
        Temp_data_frame[a,c(1:8)] <<- Data_Pro[i,c(1:8)]
        Temp_data_frame[a,9] <<- c
      }
      i <- i + 1
      k <- 0
      c <- 0
    }
  }
  k<-1
}
return(1)
}

# Function to update the main file with imputed values based on a Value
Update_dup <- function(Variable,Value,p,j,q,l,e){
nrows <- nrow(Data_Pro)
ntemp <- nrow(Temp_data_frame)
if (ntemp > 0) {
  for (j in 1:nrows) {
    for (l in 1:ntemp) {
      if (Temp_data_frame$time_1[l] == Data_Pro$time_1[j]) {
        p <- (Temp_data_frame$Count[l]%/%2)
        q <- j - p
        for (e in 1:p) {
          Data_Pro[[Variable]][q-e] <<- Data_Pro[[Variable]][q-e] - Value*e
          Data_Pro[[Variable]][q+e] <<- Data_Pro[[Variable]][q+e] + Value*e
        }
      }
    }
  }
} 
return(1)
}
##  ##  ## ## ## ## ## ##  ##  ##  ## ## ## ## ## ##  ##  ##  ## ## ## ##

# Call the functions for all the variables with respective Threshold and
# Imputing level
####                                                                 ####
# Temperature at the threshold of 15 & interval of 0.01
Identify_dup(Variable = "Temperature",Threshold=15,i=1,k=1,a=0,b=0,c=0)
Update_dup(Variable = "Temperature",Value=0.01,p=0,j=1,q=0,l=1,e=0)

# Noise at the threshold of 15 & interval of 0.01
Identify_dup(Variable = "Noise",Threshold=5,i=1,k=1,a=0,b=0,c=0)
Update_dup(Variable = "Noise",Value=0.1,p=0,j=1,q=0,l=1,e=0)

# Light at the threshold of 15 & interval of 0.01
Identify_dup(Variable = "Light",Threshold=5,i=1,k=1,a=0,b=0,c=0)
Update_dup(Variable = "Light",Value=0.5,p=0,j=1,q=0,l=1,e=0)

# Co2 at the threshold of 15 & interval of 0.01
Identify_dup(Variable = "Co2",Threshold=5,i=1,k=1,a=0,b=0,c=0)
Update_dup(Variable = "Co2",Value=0.25,p=0,j=1,q=0,l=1,e=0)

# VOC at the threshold of 15 & interval of 0.01
Identify_dup(Variable = "VOC",Threshold=5,i=1,k=1,a=0,b=0,c=0)
Update_dup(Variable = "VOC",Value=0.25,p=0,j=1,q=0,l=1,e=0)

# Humidity at the threshold of 15 & interval of 0.01
Identify_dup(Variable = "Humidity",Threshold=7,i=1,k=1,a=0,b=0,c=0)
Update_dup(Variable = "Humidity",Value=0.1,p=0,j=1,q=0,l=1,e=0)

###################################################################################
###################################################################################
################# Imputing abnormal values ###########################
# Create a temperory data frame
Temp_df_3 <- as.data.frame(Data_Pro)
##  ##  ## ## ## ## ## ##  ##  ##  ## ## ## ## ## ##  ##  ##  ## ## ##
# Function to identify the abnormal values base on a level and store 
# in a Temp data Frame and to update the Main file
Remove_abnorm <- function(Variable,level,i,a) {
Temp_df_3 <<- Temp_df_3[F,]
while (i < (nrow(Data_Pro)-1)) {
  if (abs(Data_Pro[[Variable]][i] - Data_Pro[[Variable]][i+1]) > level){
    if (abs(Data_Pro[[Variable]][i+1] - Data_Pro[[Variable]][i+2]) > level) {
      if (abs(Data_Pro[[Variable]][i] - Data_Pro[[Variable]][i+2]) < level) {
        a <- a + 1
        Temp_df_3[a,] <<- Data_Pro[i+1,] 
        Data_Pro[[Variable]][i+1] <<- (Data_Pro[[Variable]][i] + Data_Pro[[Variable]][i+2])/2 
      }
    }
  }
  i = i + 1
}
return(1)
}
##  ##  ## ## ## ## ## ##  ##  ##  ## ## ## ## ## ##  ##  ##  ## ## ##
names(Data_Pro)
# Call the function for Temperature at the level of 1
Remove_abnorm(Variable = "Temperature",level=1,i=1,a=0)

# Call the function for Noise at the level of 1
Remove_abnorm(Variable = "Noise",level=2,i=1,a=0)

# Call the function for Light at the level of 1
Remove_abnorm(Variable = "Light",level=250,i=1,a=0)

# Call the function for Co2 at the level of 1
Remove_abnorm(Variable = "Co2",level=10,i=1,a=0)

# Call the function for VOC at the level of 1
Remove_abnorm(Variable = "VOC",level=10,i=1,a=0)

# Call the function for Humidity at the level of 1
Remove_abnorm(Variable = "Humidity",level=2,i=1,a=0)

# Copy to new data frame
Data_Pro_3 <- Data_Pro

###################################################################################
# Plot for Noise abnormal values
# Before imputing the values
Data_Pro_2 %>%
  subset(time_1 > '2017-03-04 21:01:04' & time_1 <= '2017-03-04 21:17:04') %>%
  ggplot(aes(x=time_1,y=Noise)) + 
  geom_point() + geom_line(color='red') +
  xlab("Time") + ylab("Noise") + 
  ggtitle("Abnormal Recordings of Noise on 3rd March")

# After imputing the values
Data_Pro %>%
  subset(time_1 > '2017-03-04 21:01:04' & time_1 <= '2017-03-04 21:17:04') %>%
  ggplot(aes(x=time_1,y=Noise)) + 
  geom_point() + geom_line(color='red') +
  xlab("Time") + ylab("Noise") + 
  ggtitle("Abnormal Recordings of Noise on 3rd March")
###################################################################################

# Write to a CSV file if needed
# write.csv(Data_Pro ,file='Imputed_Data_IOT.csv')

###################################################################################
###################################################################################
# Findings/inferences from the prepared data 
###################################################################################
###################################################################################

# Changing time_hours to factor for grouping by
# Check the class of new variable
cols_to_change = c('time_hours')
sapply(Data_Pro[cols_to_change], class)
# Check the new variable class to fator
Data_Pro[cols_to_change] = lapply(Data_Pro[cols_to_change], factor)
sapply(Data_Pro[cols_to_change], class)

###################################################################################
# Comparing the Average level of attributes at each hour of the day
# Temperature Vs Time in hours
Data_Pro %>%
  group_by(time_hours) %>%
  summarise(avg_Temperature=mean(Temperature)) %>%
  ggplot(aes(x=time_hours,y=avg_Temperature,group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Time of the day") + ylab("Avg. Temperature") +
  ggtitle("Recordings of Temperature at each hour of a day")

# Noise Vs Time in hours
Data_Pro %>%
  group_by(time_hours) %>%
  summarise(avg_Noise=mean(Noise)) %>%
  ggplot(aes(x=time_hours,y=avg_Noise,group=1)) +
  geom_point() + geom_line(color='black') +
  xlab("Time") + ylab("Avg. Noise") +
  ggtitle("Recordings of Noise at each hour of a day")

# Light Vs Time in hours
Data_Pro %>%
  group_by(time_hours) %>%
  summarise(avg_Light=mean(Light)) %>%
  ggplot(aes(x=time_hours,y=avg_Light, group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Time") + ylab("Avg. Light") +
  ggtitle("Recordings of Light at each hour of a day")

# Co2 Vs Time in hours
Data_Pro %>%
  group_by(time_hours) %>%
  summarise(avg_Co2=mean(Co2)) %>%
  ggplot(aes(x=time_hours,y=avg_Co2,group = 1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Time") + ylab("Avg. Co2") +
  ggtitle("Recordings of Co2 at each hour of a day")

# VOC Vs Time in hours
Data_Pro %>%
  group_by(time_hours) %>%
  summarise(avg_VOC=mean(VOC)) %>%
  ggplot(aes(x=time_hours,y=avg_VOC,group = 1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Time") + ylab("Avg. VOC") +
  ggtitle("Recordings of VOC at each hour of a day")

# Humidity Vs Time in hours
Data_Pro %>%
  group_by(time_hours) %>%
  summarise(avg_Humidity=mean(Humidity)) %>%
  ggplot(aes(x=time_hours,y=avg_Humidity,group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Time") + ylab("Avg. Humidity") +
  ggtitle("Recordings of Humidity at each hour of a day")
#########################################################################

#########################################################################
# Comparing the average level of attributes on each day of week
# Temperature vs day of the week
Data_Pro %>%
  group_by(weekday) %>%
  summarise(avg_Temperature=mean(Temperature)) %>%
  ggplot(aes(x=reorder(weekday,avg_Temperature),y=avg_Temperature,group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Day of the week") + ylab("Avg. Temperature") +
  ggtitle("Recordings of Temperature on days of the week")

# Noise Vs day of the week
Data_Pro %>%
  group_by(weekday) %>%
  summarise(avg_Noise=mean(Noise)) %>%
  ggplot(aes(x=reorder(weekday,avg_Noise),y=avg_Noise,group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Day of the week") + ylab("Avg. Noise") +
  ggtitle("Recordings of Noise on days of the week")

# Light Vs day of the week
Data_Pro %>%
  group_by(weekday) %>%
  summarise(avg_Light=mean(Light)) %>%
  ggplot(aes(x=reorder(weekday,avg_Light),y=avg_Light,group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Day of the week") + ylab("Avg. Light") +
  ggtitle("Recordings of Light on days of the week")

# Humidity Vs day of the week
Data_Pro %>%
  group_by(weekday) %>%
  summarise(avg_Humidity=mean(Humidity)) %>%
  ggplot(aes(x=reorder(weekday,avg_Humidity),y=avg_Humidity,group=1)) + 
  geom_point() + geom_line(color='black') +
  xlab("Day of the week") + ylab("Avg. Humidity") +
  ggtitle("Recordings of Humidity on days of the week")
#########################################################################

#########################################################################
# Compare the sensor recordings at each hour
# Heat Map of Sensor Vs Time in hours
Data_Pro %>%
  group_by(unitid,time_hours) %>%
  summarise(Rec_Count=n()) %>%
  ggplot(aes(unitid,time_hours))+
  geom_tile(aes(fill=Rec_Count),colour="white")+
  scale_fill_gradient(low="lightgreen",high = "darkred")+
  xlab("Sensor_id")+
  ylab("Time in hours")+
  ggtitle(" Gridded Heat Map: Time in hrs vs Sensor_id") 
#########################################################################

#########################################################################
# compare sensor recordings at day level
Data_Pro %>%
  group_by(weekday,unitid) %>%
  summarise(count=n()) %>%
  ggplot(aes(x=weekday,y=count)) + 
  geom_point() +
  geom_text_repel(aes(label = unitid),size=3,colour="steelblue") +
  xlab("Day of the week") + ylab("Count of recordings") +
  ggtitle("Sensor recordings at day level")
#########################################################################

#########################################################################
# Get only date from timestamp in Data_Pro
# Convert only_date type date
Data_Pro$only_date = as.Date(format(Data_Pro$time_1, format="%Y-%m-%d"))
# Sensor recordings at month end of two months
Data_Pro %>%
  group_by(only_date) %>%  
  summarise(count=n()) %>%
  ggplot(aes(x=only_date,y=count)) + 
  geom_bar(stat='identity',color='steelblue') + 
  xlab("Date") + ylab("Count of no. of records") + 
  ggtitle("Recordings for each day") +
  theme(axis.text.x=element_text(angle=0, hjust=1)) 
#########################################################################