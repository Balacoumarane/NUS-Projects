################## TASK - 1 ################## 
# load libraries
pacman :: p_load(tidyverse, reshape2, readxl, jsonlite, corrplot, XLConnect, magrittr, ggrepel)

# Read data into r from CSV file
titanic_data <- read.csv("titanic3.csv",header=TRUE)

# Get column names
names(titanic_data)

# View the summary of data
summary(titanic_data)

# Get details of each column
str(titanic_data)

## Task - 1
############################ 1 Gender on Survival ############################
# Get the class of Gender
cols_to_change = c('sex')
sapply(titanic_data[cols_to_change], class)

# Plot the distribution of Gender on Survival
titanic_data %>%
  group_by(sex,survived) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(titanic_data)) %>%
  ggplot(aes(x= as.factor(survived),y=count_level,fill=sex)) + 
  geom_bar(stat='identity',position='dodge') + xlab("Survival/Death") + ylab("Count") + 
  ggtitle("Survival/Death based on Gender") +
  geom_text(aes(label=round(percentage,2)),vjust=2) + scale_fill_brewer(palette = "Set1")

# Chi square test for Gender on Survival
chisq.test(table(titanic_data$sex, titanic_data$survived))

############################ 2 Embarked on Survival ############################
# Get the class of Embarkation
cols_to_change = c('embarked')
sapply(titanic_data[cols_to_change], class)

# Two observations with NA are removed and distribution of Embarktion on Survival is plotted
titanic_data %>%
  subset(embarked != "") %>%
  group_by(embarked,survived) %>%
  filter(!is.na(embarked)) %>%  
  summarise(count_level = n(),
            percentage = n()/nrow(titanic_data)) %>%
  ggplot(aes(x= as.factor(survived),y=count_level,fill=embarked)) + 
  geom_bar(stat='identity',position='dodge') + xlab("Survival/Death") + ylab("Count") + 
  ggtitle("Survival/Death based on Embarkation point") +
  geom_text(aes(label=round(percentage,2)),vjust=2) + scale_fill_brewer(palette = "Set1")

# Chi square test for embarkation point on Survival
chisq.test(table(titanic_data$embarked, titanic_data$survived))

############################ 3 Cabin on Survival ############################
# Generate new variable with first letter of Cabin
titanic_data$cabin_first <- substr(titanic_data$cabin,1,1)
# Check the class of new variable
cols_to_change = c('cabin_first')
sapply(titanic_data[cols_to_change], class)
# Change the new variable class to factor
titanic_data[cols_to_change] = lapply(titanic_data[cols_to_change], factor)
sapply(titanic_data[cols_to_change], class)

# Mosaic plot of different cabins on survival 
mosaicplot(table(titanic_data$cabin_first, titanic_data$survived), 
           xlab='Cabins of people',ylab='Survival',
           main='Mosaic plot of cabin vs survival', shade=T, color = T)

# Generate a new variable to check if passenger is having cabin or not
titanic_data$cabin_cat[titanic_data$cabin_first==""] <- 'No-cabin'
titanic_data$cabin_cat[titanic_data$cabin_first!=""] <- 'cabin'

# distribution of cabin on Survival is plotted
titanic_data %>%
  group_by(cabin_cat,survived) %>%
  summarise(count_level = n(),
            percentage = n()/nrow(titanic_data)) %>%
  ggplot(aes(x= as.factor(survived),y=count_level,fill=cabin_cat)) + 
  geom_bar(stat='identity',position='dodge') + xlab("Survival/Death") + ylab("Count") + 
  ggtitle("Survival/Death based on Cabin") +
  geom_text(aes(label=round(percentage,2)),vjust=2) + scale_fill_brewer(palette = "Set1")

# Chi square test of cabin on survival
chisq.test(table(titanic_data$cabin, titanic_data$survived))

############################ 4 Fare on Survival ############################
# Split the fare level into following categories
titanic_data$fare_level[titanic_data$fare == 0] = 'No Info'
titanic_data$fare_level[titanic_data$fare < 100 & titanic_data$fare > 0] = 'Low'
titanic_data$fare_level[titanic_data$fare < 250 & titanic_data$fare >= 100] = 'Medium'
titanic_data$fare_level[titanic_data$fare >= 250] = 'High'

# Check the class of new variable
cols_to_change = c('fare_level')
sapply(titanic_data[cols_to_change], class)
# Check the new variable class to fator
titanic_data[cols_to_change] = lapply(titanic_data[cols_to_change], factor)
sapply(titanic_data[cols_to_change], class)

# Mosaic plot of different fare levels on survival 
mosaicplot(table(titanic_data$fare_level, titanic_data$survived),
           xlab='fare levels of people',ylab='Survival',
           main='fare-level by survival', shade=TRUE)

# bar plot representation of different fare levels on survival
titanic_data %>%
  group_by(fare_level,survived) %>%
  filter(!is.na(fare_level)) %>%  
  summarise(count_level = n(),
            percentage = n()/nrow(titanic_data)) %>%
  ggplot(aes(x= as.factor(survived),y=count_level,fill=fare_level)) + 
  geom_bar(stat='identity',position='dodge') + xlab("Survival/Death") + ylab("Count") + 
  ggtitle("Survival/Death based on fare levels") +
  geom_text(aes(label=round(percentage,2)),vjust=0) + scale_fill_brewer(palette = "Set1")

# Chi square test
chisq.test(table(titanic_data$fare, titanic_data$survived))

################ END OF TASK-1 ################

################## TASK - 2 ################## 

# Read IMDB data into R from Excel
IMDB2000 <- read.csv("IMDB_2000.csv",header=TRUE)

# To get the summary of the data loaded and string details of data.
summary(IMDB2000)
str(IMDB2000)

# To find number of missing and non-missing rows
missing = IMDB2000 %>%
  filter(!complete.cases(.))
nrow(missing) 
non_missing = IMDB2000 %>%
  filter(complete.cases(.))
nrow(non_missing)

# Q1. To identify pattern of total number of movies produced in each year

# Plot
IMDB2000 %>%
  subset(title_year != "") %>%
  group_by(title_year) %>%  
  filter(!is.na(title_year)) %>% 
  summarise(count_level = n()) %>%
  ggplot(aes(x= title_year,y=count_level)) + 
  geom_point() + geom_line(color='red') +
  ggtitle("Number of movies in each year") + xlab("Year") + ylab("No.of movies") + 
  scale_x_continuous(breaks = seq(2000,2016,1)) +
  geom_text(aes(label=count_level),vjust=1.5,color="black",size=3)

######################################################################################

# Q2. To understand whether gross of each movie would be effected by IMDB rating

# Categorize IMDB Score
IMDB2000$imdb_cat[IMDB2000$imdb_score >= 1 & IMDB2000$imdb_score < 3] = 'Bad'
IMDB2000$imdb_cat[IMDB2000$imdb_score >= 3 & IMDB2000$imdb_score < 6] = 'Average'
IMDB2000$imdb_cat[IMDB2000$imdb_score >= 6 & IMDB2000$imdb_score < 8] = 'Good'
IMDB2000$imdb_cat[IMDB2000$imdb_score >= 8] = 'Excellent'

# Check the class of new variable
cols_to_change = c('imdb_cat')
sapply(IMDB2000[cols_to_change], class)
# Change the new variable class to factor
IMDB2000[cols_to_change] = lapply(IMDB2000[cols_to_change], factor)
sapply(IMDB2000[cols_to_change], class)

# Plot
IMDB2000 %>%
  filter(!is.na(gross)) %>%
  group_by(imdb_cat) %>% 
  summarise(avg_gross = mean((gross))/1000000) %>%
  ggplot(aes(x=reorder(imdb_cat, avg_gross),y=avg_gross)) + 
  geom_bar(stat='identity',fill='steelblue',color='red') + 
  xlab("IMDB Rating categories") + 
  ylab("Average Gross of Movies in Millions") + 
  ggtitle("Average Gross of Movies vs IMDB Ratings") +
  geom_text(aes(label=round(avg_gross, digits=2)),vjust=1.5,color="white",size=3)
######################################################################################

## Q3.	To identify top 10 profitable movies and the movie director from the whole data set
# Plot
IMDB2000 %>% 
  subset(movie_title != "") %>%
  subset(director_name != "") %>%
  filter(!is.na(movie_title)) %>%
  filter(!is.na(director_name)) %>%
  filter(!is.na(gross)) %>%
  filter(!is.na(budget)) %>%
  mutate(profit = ((gross - budget)/budget) * 100)%>%
  top_n(10, profit) %>%
  ggplot(aes(x=reorder(movie_title, profit), y=profit)) + 
  geom_point() + 
  geom_text_repel(aes(label = director_name),size=3,colour="steelblue") + 
  xlab("Movie name") + 
  ylab("Profit percentage") +  
  ggtitle("10 Most Profitable Movies & their director names") +  
  coord_flip()
######################################################################################

# Q4. To understand the duration of movies across each language
# Plot
IMDB2000 %>%
  subset(language != "") %>%
  subset(duration != "") %>%
  filter(!is.na(language)) %>%
  filter(!is.na(duration)) %>%
  group_by(language,duration) %>% 
  ggplot(aes(x=reorder(language, duration) ,y=duration, fill=language)) + 
  geom_boxplot() + 
  xlab("Movie language") + ylab("duration of the movies") + 
  ggtitle("Duration of movies in each Language") + guides(fill=FALSE) +
  coord_flip()
######################################################################################

# Q5.	To identify the movies from which content rating has performed well
# Plot
IMDB2000 %>%
  subset(content_rating != "") %>%
  filter(!is.na(gross)) %>%
  group_by(content_rating) %>% 
  filter(!is.na(content_rating)) %>%
  summarise(avg_gross = mean(gross)/1000000) %>%
  ggplot(aes(x=reorder(content_rating, avg_gross),y=avg_gross)) + 
  geom_bar(stat='identity',fill='steelblue',color = 'red') + xlab("Content ratings") + 
  ylab("Avg. Gross in million") + 
  ggtitle("Average Gross vs Content Ratings") +
  geom_text(aes(label=round(avg_gross,digits = 2)),vjust=1.5,color="Black",size=3)
######################################################################################

# Q6 To understand the prominent language in each country.
# Plot
IMDB2000 %>%
  subset(country != "") %>%
  subset(language != "") %>%
  group_by(country,language) %>%
  filter(!is.na(country)) %>%
  filter(!is.na(language)) %>%
  summarise(Movie_Count=n()) %>%
  ggplot(aes(language,country))+
  geom_tile(aes(fill=log(Movie_Count)),colour="whit e")+
  scale_fill_gradient(low="lightgreen",high = "darkred")+
  xlab("Language")+
  ylab("Country")+
  ggtitle(" Gridded Heat Map: Country vs Language")+
  theme(axis.text.x=element_text(angle=45, hjust=1)) +  guides(fill=FALSE)
######################################################################################

# Q7 To identify whether the number of movies with profit is higher than the number of
#    movies with loss in the recent years
IMDB2000 %>%
  filter(!is.na(gross)) %>%
  filter(!is.na(budget)) %>%
  subset(title_year != "") %>%
  mutate(profit = (gross - budget)) %>%
  mutate(profit_cat = ifelse(profit>0,'profit','loss')) %>%
  group_by(title_year,profit_cat) %>%
  filter(!is.na(title_year)) %>%
  summarise(count_level = n()) %>%
  ggplot(aes(x= title_year,y=count_level,fill=profit_cat)) + 
  geom_bar(stat='identity',position='dodge') + xlab("year") + ylab("No.of movies") + 
  geom_text(aes(label=count_level),vjust=1.5,color="black",size=3) +
  ggtitle("Plot of number of movies with profit/loss in each year") + 
  scale_x_continuous(breaks = seq(2000,2016,1)) +
  scale_fill_brewer(palette = "Set1")
