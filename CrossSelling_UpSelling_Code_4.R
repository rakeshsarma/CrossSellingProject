# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject") 


library(readxl)
library(XLConnect)
library(caret)
library(MASS) # for StepAIC calculation
library(DMwR) # FOr calculating Error Metrics
library(dplyr) # Make sure that Dplyr is loaded in last, as we need select funciton in dplyr
marketing_data<- read_excel("~/Data Science/CrossSellingProject/Target Marketing and cross selling - Data.xls")
summary(marketing_data)
str(marketing_data)
unique(marketing_data$Job_Code)
#replace space with underscore
names(marketing_data) <- gsub(" ", "_", names(marketing_data))
names(marketing_data) 

# ######################### data preprocessing starts ########################
# Step 0.1 : converting customer type into factor with 1,2,3
marketing_data$Customer_Type_1<- as.factor(as.character(substring(text = marketing_data$Customer_Type, first = nchar(marketing_data$Customer_Type)-1, last = nchar(marketing_data$Customer_Type))))


# ######################### data preprocessing ends########################
#rolling the data to customer level
#Step 1 : Order the data based on Customer ID and Call Duration
marketing_data_1 <- marketing_data[order(marketing_data$Customer_ID,marketing_data$Call_Date),]
#Sanity Check
View(marketing_data_1)
#Step 2: Remove unnecessay attributes, Use dplyr package to do this 
marketing_data_2 <- select(marketing_data_1, -c(Customer_Name:Contact, Ticket_Number,Current_Email, Week_Ending_Date, Call_Time, Schedule_Time, Dispatch_Time, Complete_Time))
#Sanity Check
View(marketing_data_2)
#Step 3: Take the difference of call dates
marketing_data_3<-marketing_data_2 %>% arrange(Customer_ID, Call_Date) %>% group_by(Customer_ID) %>% mutate(diff =  as.Date(Call_Date)- lag(as.Date(Call_Date)))
View(marketing_data_3)
#Step 4: Lets count the total number of rows for each customer
Customer_count<-marketing_data_2 %>% group_by(Customer_ID) %>% summarise(count =  n()) %>% arrange(count)
Customer_count_1<-marketing_data_4 %>% group_by(Customer_ID) %>% summarise(count =  n()) %>% arrange(count)
View(Customer_count)
#Step 5: Extracting the first and the last columns

marketing_data_4<- marketing_data_3 %>% arrange(Customer_ID, Call_Date) %>%
  slice(c(1, n())) %>%
  ungroup()
# Sanity Check
nrow(marketing_data_4) # 2116
length(unique(marketing_data_4$Customer_ID)) #1058 customer ids
######################### Preparing Test data set Part 1/2 ##########
#Step 5.1 Extracting the last columns for test data
View(marketing_data_4)
marketing_data_test_1 <- marketing_data_4[which(!is.na(marketing_data_4$diff)),]
View(marketing_data_test_1)
nrow(marketing_data_test_1) # 1058 
#step 5.2 Remove rows that have only two rows, These rows will not feature in the train dataset
marketing_data_test_2 <- marketing_data_test_1[-grep(c("C000197"), fixed = T,marketing_data_test_1$Customer_ID),]
marketing_data_test_2 <- marketing_data_test_2[-grep(c("C000436"),fixed = T, marketing_data_test_2$Customer_ID),]
View(marketing_data_test_2) # sanity test
nrow(marketing_data_test_2)

######################### Preparing Test data set Part 1/2 ##########

#Step 6 preparing the train dataset by removing first and last rows from the orogonal dataset
marketing_data_5 <- setdiff(marketing_data_3, marketing_data_4)
nrow(marketing_data_5) #3168 rows
View(marketing_data_5)

# Find mean call duration  for each customer type for train set
cust_type_call_duration_train <- marketing_data_5 %>% group_by(Customer_Type) %>% summarise(mean_call_duration = mean(diff))

# Find mean call duration  for each customer type for test set
cust_type_call_duration_test <- marketing_data_test_2 %>% group_by(Customer_Type) %>% summarise(mean_call_duration = mean(diff))

##    ****Observation : Mean difference between calls is very different for train and Test set


#Step 7: Generating new features
#F1: - Train_data - Difference between calldate and complete date: diff_call_complete
marketing_data_5$diff_call_complete <- as.Date(marketing_data_5$Complete_Date)-as.Date(marketing_data_5$Call_Date)
#F1: - Test_data - Difference between calldate and complete date: diff_call_complete
marketing_data_test_2 $diff_call_complete <- as.Date(marketing_data_test_2$Complete_Date)-as.Date(marketing_data_test_2$Call_Date)
#--------------------------------
#F2:- Train_data - Difference between Last_Service_date and Setup_date: Customer_age
marketing_data_5$cust_age <- as.Date(marketing_data_5$Last_Service_Date)-as.Date(marketing_data_5$Setup_Date)
View(marketing_data_5)
#F2:- Train_data - Difference between Last_Service_date and Setup_date: Customer_age
marketing_data_test_2$cust_age <- as.Date(marketing_data_test_2$Last_Service_Date)-as.Date(marketing_data_test_2$Setup_Date)
View(marketing_data_test_2)

#---------------------------------
#F3 Train_data - Have a new feature for month of a year
marketing_data_5$month <- as.integer(substring(text = marketing_data_5$Year_Month, first = nchar(marketing_data_5$Year_Month)-1, last = nchar(marketing_data_5$Year_Month)))
View(marketing_data_5)
#F3 Test_data - Have a new feature for month of a year
marketing_data_test_2$month <- as.integer(substring(text = marketing_data_test_2$Year_Month, first = nchar(marketing_data_test_2$Year_Month)-1, last = nchar(marketing_data_test_2$Year_Month)))
View(marketing_data_test_2)

#---------------------------------
# Step 8: Remove unnecessary features; Customer_id, Branch_ID, Bill_To,setupdate, 
#last_service_date, call_date, complete date, Year_month, Schedule date
marketing_data_6<- select(marketing_data_5, -ends_with(c("Date"))) 
View(marketing_data_6)
marketing_data_7 <- select(marketing_data_6, -c(Branch_ID:Bill_To, Year_Month) )
rm(marketing_data_6)
View(marketing_data_7)
#Reoreding the variables with target variable in the front
marketing_data_7 <- select(marketing_data_7, diff, everything())
# converting customertype into numbers
marketing_data_7$Customer_Type<- as.factor(as.character(substring(
  text = marketing_data_7$Customer_Type, 
  first = nchar(marketing_data_7$Customer_Type)-1, 
  last = nchar(marketing_data_7$Customer_Type))))

#Step 8.1: Doing step 8 for for test data
marketing_data_test_3<- select(marketing_data_test_2, -ends_with(c("Date"))) 
marketing_data_test_4 <- select(marketing_data_test_3, -c(Branch_ID:Bill_To, Year_Month) )
rm(marketing_data_test_3)
View(marketing_data_test_4)
#Reoreding the variables with target variable in the front
marketing_data_test_4 <- select(marketing_data_test_4, diff, everything())
# converting customertype into numbers
marketing_data_test_4$Customer_Type<- as.factor(as.character(substring(
  text = marketing_data_test_4$Customer_Type, 
  first = nchar(marketing_data_test_4$Customer_Type)-1, 
  last = nchar(marketing_data_test_4$Customer_Type))))


#Step 9 Check if the data is in right format
summary(marketing_data_7)
str(marketing_data_7)
#step 10 Converting to appropriate format
marketing_data_7$Rev_Code <- as.factor(as.character(marketing_data_7$Rev_Code))
marketing_data_7$Job_Code <- as.factor(as.character(marketing_data_7$Job_Code))
marketing_data_7$diff_call_complete <- as.numeric(marketing_data_7$diff_call_complete)
marketing_data_7$cust_age <- as.numeric(marketing_data_7$cust_age)
marketing_data_7$diff <- as.numeric(as.character(marketing_data_7$diff))
marketing_data_7$month <- as.factor(as.character(marketing_data_7$month))
#sanity check
str(marketing_data_7)
class(marketing_data_7)
marketing_data_7 <- as.data.frame(marketing_data_7)
class(marketing_data_7) # sanity check
######################### Preparing Test data set Part 2/2 ##########
#Step 9.1 Doing the same thing for test data 
summary(marketing_data_test_4)
str(marketing_data_test_4)

#Step 10.1
marketing_data_test_4$Rev_Code <- as.factor(as.character(marketing_data_test_4$Rev_Code))
marketing_data_test_4$Job_Code <- as.factor(as.character(marketing_data_test_4$Job_Code))
marketing_data_test_4$diff_call_complete <- as.numeric(marketing_data_test_4$diff_call_complete)
marketing_data_test_4$cust_age <- as.numeric(marketing_data_test_4$cust_age)
marketing_data_test_4$diff <- as.numeric(as.character(marketing_data_test_4$diff))
marketing_data_test_4$month <- as.factor(as.character(marketing_data_test_4$month))

#sanity check
str(marketing_data_test_4)
class(marketing_data_test_4)
marketing_data_test_4 <- as.data.frame(marketing_data_test_4)
class(marketing_data_test_4) # sanity check

######################### Preparing Test data set Part 2/2 ##########

#Step 11
########################## Buidling a model #################
# M1 : Simple model - Linear Regression
linear_model <- lm(diff~., data = marketing_data_7 )
summary(linear_model)
# Lets prdict on train data using the above model
predictions_on_train <- predict(linear_model, newdata = marketing_data_7, type = "response")
regr.eval(marketing_data_7$diff, predictions_on_train)
# Looks like the model is shot 
# M2 : Best model using StepAIC
model2 <- stepAIC(linear_model, direction = "both")
summary(model2$anova)

#Step 11.1 
########################## Testing on test data #################
predictions_on_test <- predict(linear_model, newdata = marketing_data_test_4[,-1], type = "response")
regr.eval(marketing_data_test_4$diff, predictions_on_test)
View(marketing_data_test_2)

test_data_with_preds <- data.frame(marketing_data_test_4, predictions_on_test)
View(test_data_with_preds)
