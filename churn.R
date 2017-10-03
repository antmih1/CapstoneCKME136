# Antoaneta Mihaila

install.packages("data.table")
library(data.table)
raw.churn <- fread("C:\\D-Drive\\CKME136\\R_code\\churn_all.txt", header = F,
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


