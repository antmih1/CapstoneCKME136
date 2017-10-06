# Antoaneta Mihaila

install.packages("data.table")
library(data.table)
raw.churn <- fread("churn_all.txt", header = F,
                   colClasses = c("character","numeric","integer","character",
                                  "character","character","integer","numeric",
                                  "numeric","numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric","numeric",
                                  "numeric", "integer","character"),
                   col.names=
                     c("state", "account_length", "area_code", "phone_number",
                       "international_plan", "voice_mail_plan", "number_vmail_messages",
                       "total_day_minutes", "total_day_calls", "total_day_charge",
                       "total_eve_minutes", "total_eve_calls", "total_eve_charge",
                       "total_night_minutes", "total_night_calls", "total_night_charge",
                       "total_intl_minutes", "total_intl_calls", "total_intl_charge",
                       "number_customer_service_calls", "churn"), stringsAsFactors =F, data.table = F)
summary(raw.churn)
str(raw.churn)
nrow(raw.churn)

# Change the international_plan, voice_mail_plan, churn to factors
raw.churn$international_plan <- as.factor(ifelse(raw.churn$international_plan == "yes", "IntPlan", "NoIntPlan"))
raw.churn$voice_mail_plan <- as.factor(ifelse(raw.churn$voice_mail_plan == "yes", "VmPlan", "NoVmPlam"))
raw.churn$churn <- as.factor(ifelse(grepl("False", raw.churn$churn), "NotChurner", "Churner"))
table(raw.churn$international_plan)
table(raw.churn$voice_mail_plan)
table(raw.churn$churn)

#Save the churn dataframe in bunary format
saveRDS(raw.churn, file="C:\\D-Drive\\CKME136\\R_code\\churn.RDS")

# Load the dataframe in binary format
churn_data <- readRDS("churn.Rds")
summary(churn_data)
# Check the distribution of data
h1 <- hist(churn_data$total_day_calls, breaks = 15, col ="green", main = "Total day calls", freq = FALSE)
h2 <- hist(churn_data$total_eve_calls, breaks = 15, col ="blue", main = "Total evening calls", freq = FALSE)

table(churn_data$churn)

#Plot the boxplots for the numeric variables
boxplot(churn_data[5:10])
boxplot(churn_data[11:18])

# create a visualization to show the correlation between the churn dependent variables and all other variables
library(ggplot2)
library(dplyr)

ggplot(churn_data,aes(account_length)) + geom_histogram(aes(fill=churn),color='black',binwidth=1) + theme_bw()

#2. Clean the dataset

# check the missing data
# From the summary we can see that are not missing data
# We can also check with 
table(is.na(churn_data))

# Remove the columns that will not be used
churn_data$state <-NULL
churn_data$phone_number <- NULL
str(churn_data)

# 3.Split the data into 80% train and 20% test datasets
# First shuffle the dataset
library(caTools)
set.seed(3152)
churn_data <- churn_data[sample.int(nrow(churn_data)),]

# Then split the data 80% train and 20% test
set.seed(441)
split_t <- sample.split(churn_data$churn, SplitRatio = 0.8)
churn_train <- subset(churn_data, split_t == T)
churn_test <- subset(churn_data, split_t == F)
str(churn_train)
str(churn_test)

# Apply algorithm
model1 <- glm(churn ~ ., family = binomial(logit), data = churn_train)

summary(model1)
# We can notice that international_plan, number of customer service calls and international minutes
# are the most significant variables in customer churn

#test the model on the test dataset 
churn_test$predicted.churn <- predict(model1, newdata = churn_test, type = "response")
# confusion matrix
table(churn_test$churn, churn_test$predicted.churn > 0.5)

# calculate accuracy
(29+842)/(29+842+112+17)
  # the accuracy is 87%
#calculate recall
29/(29+112)
  # the recall is 20%
# calculate precision
29/(29+17)
  # the precission is 63%
# I will try to improve this number by using step method and cross-fold

# Apply decision tree algorithm
# Install the library rpart
install.packages("rpart")
library(rpart)
# train the model with decision tree model
 model2 <- rpart(churn ~ ., method = "class", data = churn_train)

# predict on the test dataset
churn.predict <- predict(model2, churn_test)
head(churn.predict) # these are the probaliities that a customer will churn

# join these 2 columns
churn.predict <- as.data.frame(churn.predict)

join <- function(x){
  if (x>0.5){return ("Churner")
    }else{return("NotChurner")}
}
churn.predict$churn <- sapply(churn.predict$Churner, join)
head(churn.predict)

# Confusion matrix
table(churn.predict$churn, churn_test$churn)

# accuracy
(97+842)/(97+44+17+842)
  # the accuracy is 93%
# recall
97/(97+17)
  # the recall is 85%
# precision
97/(97+44)
  # the precision is 97%

# Use the libtrary rpart.plot to plot the decision tree
library(rpart.plot)
prp(model2)

# Apply random forest algorithm
library(randomForest)

model3 <- randomForest(churn ~ ., data = churn_train, importance = TRUE)

predRF <- predict(model3, churn_test)

# Confusion matrix for the random forest
 
table(predRF, churn_test$churn)

# calculate accuracy
(102+849)/(102+39+10+849)
  # the accuracy is 95%, it is better the decision tree model
# calculate recall
102/(102+10)
  # the recall is 91%
# calculate precision
102/(102+39)
  #the precision is 72%