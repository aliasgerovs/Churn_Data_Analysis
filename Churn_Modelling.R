# Homework
library(tidyverse)
library(scorecard)
library(skimr)
library(inspectdf)
library(caret)
library(glue)
library(highcharter)
library(h2o)
library(dplyr)
library(readr)
library(readxl)
library(Amelia)
library(caTools)
library(Information)
library(pROC)
library(randomForest)

my_data <- read.csv("Churn_Modelling.csv")

head(my_data)

str(my_data)

#view(my_data)


#2. Remove unneeded columns.

my_data$Exited <- as.factor(my_data$Exited)
my_data$EstimatedSalary <- as.factor(my_data$EstimatedSalary)
my_data$IsActiveMember <- as.factor(my_data$IsActiveMember)
my_data$HasCrCard <- as.factor(my_data$HasCrCard)
my_data$NumOfProducts <- as.factor(my_data$NumOfProducts)
my_data$Balance <- as.factor(my_data$Balance)
my_data$Tenure <- as.factor(my_data$Tenure)
my_data$Age <- as.factor(my_data$Age)
my_data$Gender <- as.factor(my_data$Gender)
my_data$Geography <- as.factor(my_data$Geography)
my_data$CreditScore <- as.factor(my_data$CreditScore)

my_data <- select(my_data , c(-Surname ,-Geography))

#Determining how many rows have "NA"

any(is.na(my_data)) # Answer is False it means we don't have any missing value in our dataset

missmap(my_data, main="Data - Missings Map", 
        col=c("yellow", "blue"), legend=FALSE)  # Again checking with missmap and seeing that there is no NA values

#my_data$Exited <- ifelse(my_data$Exited == 1, "Exited" , "NotExited")

my_data$HasCrCard <- ifelse(my_data$HasCrCard==1 , "Has card ", "Not have card")

my_data$IsActiveMember <- ifelse(my_data$IsActiveMember==1 , "Active ", "Passive")


xtabs(~ Exited + Exited, data=my_data)
xtabs(~ Exited + EstimatedSalary, data=my_data)
xtabs(~ Exited + IsActiveMember, data=my_data)
xtabs(~ Exited + HasCrCard, data=my_data)
xtabs(~ Exited + NumOfProducts, data=my_data)
xtabs(~ Exited + Balance, data=my_data)
xtabs(~ Exited + Tenure, data=my_data)
xtabs(~ Exited + Age, data=my_data)
xtabs(~ Exited + Gender, data=my_data)
xtabs(~ Exited + Geography, data=my_data)
xtabs(~ Exited + CreditScore, data=my_data)



#3. Build Churn model.

set.seed(123)

sample <- sample.split(my_data$Geography, SplitRatio = 0.70) 

# Training Data
train = subset(my_data, sample == TRUE)

# Testing Data
test = subset(my_data, sample == FALSE)

Churn <- glm(Exited ~ . , data = my_data, family="binomial")

summary(Churn)

#4. Compare model results for training and test sets.

# Train

Churn_tr <- glm(Exited ~ IsActiveMember + HasCrCard , data = train, family="binomial")
summary(Churn_tr)

# Test

Churn_te <- glm(Exited ~ IsActiveMember + HasCrCard , data = test, family="binomial")
summary(Churn_te)


fitted.probabilities <- predict(Churn_tr,newdata=test,type='response')

fitted.results <- ifelse(fitted.probabilities > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Exited)
print(paste('Accuracy',1-misClasificError)) #Accuracy is almost 81%

#5. Evaluate and explain model results using ROC & AUC curves.

train$Balance <- as.numeric(train$Balance)
train$CreditScore <- as.numeric(train$CreditScore )
train $Age <- as.numeric(train$Age)

lrROC1 <- roc(Exited ~ Balance, plot = T ,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves" , data = train)
lrROC2 <- roc(Exited ~ CreditScore, plot = T ,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves" , data = train)
lrROC3 <- roc(Exited ~ Balance + CreditScore + Age , plot = T ,print.auc=TRUE,col="green",lwd =4,legacy.axes=TRUE,main="ROC Curves" , data = train)

#6-7. Name your final homework Script as “Churn_Modelling”
 
#https://github.com/aliasgerovs/Churn_Data_Analysis

