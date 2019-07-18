library(dplyr)
library(tidyr)
library(tidyverse)
library(sjmisc)
library(dplyr)
library(ggplot2)
library(caret)

#Data Wrangling

setwd("~/work")
Mydata <- read.csv("vsal.csv")
head(Mydata)
str(Mydata)
nrow(Mydata)

#selecting variables
library(dplyr)
str(Mydata)
datal <- Mydata[ , c("Rank","Name","Platform","Total_Shipped", "Critic_Score")]
dataw <- Mydata[ , c("Rank","Name","Platform","Total_Shipped", "Critic_Score", "Year")]


#removing na
datal <- na.omit(datal)
dataw <- na.omit(dataw)

sapply(dataw,class)

class(dataw$Year)

view(dataw$Year)


write.csv(dataw, file = "dataw.csv")
dataw <- read.csv("dataw.csv")


#Testing data 20% - Traing Data 80% rando sampels


validation_index <- createDataPartition(dataw$Total_Shipped, p=0.80, list=FALSE)
validation <- dataw[-validation_index,]
# use the remaining 80% of data to training and testing the models
dataw <- dataw[validation_index,]

#Exploratoary Data
#dimensbion of data
dim(dataw)

# list types for each attribute
sapply(dataw, class)


# take a peek at the first 5 rows of the data
head(dataw)


levels(dataw$Platform)


# summarize the class distribution
percentage <- prop.table(table(dataw$Total_Shipped)) * 100
cbind(freq=table(dataw$Total_Shipped), percentage=percentage)


# summarize attribute distributions
summary(dataw)


#Statistcal Anaylsis
#calculate the diffennces in mean for each platform for critic score
datal  %>%
  group_by(Platform) %>%
  summarise(n_games = n(),
            mean_critic = mean(Critic_Score),
            std_error = sd(Critic_Score) / sqrt(n_games))
#calculate the diffrence in mean for each platform of total shipped
datal  %>%
  group_by(Platform) %>%
  summarise(n_games = n(),
            mean_Shipped = mean(Total_Shipped),
            std_error = sd(Total_Shipped) / sqrt(n_games))




#histogram with Year vs total shipped
dta_hist <- ggplot(dataw, aes(Year))
dta_hist + geom_bar(aes(fill=Total_Shipped), width = 0.6) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Total Shipped determined by Platform")

#histogram with critic score vs total shipped
dta_hist1 <- ggplot(dataw, aes(Critic_Score))
dta_hist1 + geom_bar(aes(fill=Total_Shipped), width = 0.6,stat ="identity") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Total Shipped determined by Platform")


#Histogram
dataw %>% count(Platform) %>% arrange(desc(n)) %>% 
  top_n(50) %>% 
  ggplot(aes( x = reorder(Platform,n), y  =  n, fill = Platform))  +
  geom_bar(stat = 'identity')  + theme(legend.position = 'none', axis.text.x = element_text(angle = 90)) +
  coord_flip()



#Scatterplot with critic sCORE VS pLATFORM
ggplot(datal, aes(x = Critic_Score, y = Total_Shipped, color="yellow")) + geom_point()


#Scatter plot for totalshipped vs platform
ggplot(datal, aes(x = Platform, y = Total_Shipped)) +
  geom_point() + geom_jitter()

#Scatter plot for totalshipped vs platform
ggplot(datal, aes(x = Rank, y = Total_Shipped)) +
  geom_point() 

ggplot(dataw, aes(x=Critic_Score, y=Total_Shipped)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50')

ggplot(dataw, aes(x=Year, y=Total_Shipped)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, color='#2C3E50')


library(ggplot2)
theme_set(theme_classic())

# Allow Default X Axis Labels Time series plot
ggplot(dataw, aes(x=Year)) + 
  geom_line(aes(y=Total_Shipped)) + 
  labs(title="Time Series Chart", 
       subtitle="Total Shipped per year", 
       caption="Source: Year", 
       y="Total Shipped %")

#Time series plot
ggplot(dataw, aes(x=Year)) + 
  geom_line(aes(y=Critic_Score)) + 
  labs(title="Time Series Chart", 
       subtitle="Critic Score Per Year", 
       caption="Source: year", 
       y="Critic Score %")

#Machine learniing 
summary(dataw)

model0 = lm(Total_Shipped ~., data = dataw)
summary(model0)

mode10$residuals

SSE = sum(model0$residuals^2)
SSE



model1 = lm(Total_Shipped ~ Year, data = dataw)
summary(model1)

model1$residuals

SSE = sum(model1$residuals^2)
SSE
#Model9
model9 = lm(Total_Shipped ~ Critic_Score + Year + Rank + Platform + Name, data = dataw)
summary(model9)

model9$residuals

SSE = sum(model9$residuals^2)
SSE
#Model3
model3 = lm(Total_Shipped ~ Year + Platform, data = dataw)
summary(model3)

model3$residuals

SSE = sum(model3$residuals^2)
SSE
#Model4
model4 = lm(Total_Shipped ~ Year + Platform + Critic_Score + Name, data = dataw)
summary(model4)

model4$residuals

SSE = sum(model4$residuals^2)
SSE

#Model5
model5 = lm(Total_Shipped ~ Platform,  data = dataw)
summary(model5)

model5$residuals

SSE = sum(model5$residuals^2)
SSE

results <- kmeans(dataw, "3")

str(Mydata)
summary(Mydata)

head(dataw)
str(dataw)

control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Total_Shipped~Critic_Score + Year + Rank + Platform, data=dataw, method="lm", metric="RMSE", trControl=control)
print(fit)



model7 = lm(Total_Shipped ~ Critic_Score, data = dataw)
summary(model7)

model3$residuals

SSE = sum(model7$residuals^2)
SSE

#0.21152779
control <- trainControl(method="cv", number=5)
set.seed(7)
fit <- train(Total_Shipped ~ Critic_Score + Year + Rank + Platform, method="lm", metric="RMSE", trControl=control)
print(fit)

#Model9 for testing data
model9 = lm(Total_Shipped ~ Critic_Score + Year + Rank + Platform, data = validation)
summary(model9)
#29 model for training data
model29 = lm(Total_Shipped ~ Critic_Score + Year + Rank + Platform, data = dataw)
summary(model9)

summary(model9$)



