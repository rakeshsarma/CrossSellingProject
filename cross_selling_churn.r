# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject") 


library(readxl)
library(XLConnect)
library(caret)
library(dplyr)
library(reshape2)
marketing_data<- read_excel("~/Data Science/CrossSellingProject/Target Marketing and cross selling - Data.xls")
summary(marketing_data)
str(marketing_data)
View(marketing_data)
#replace space with underscore
names(marketing_data) <- gsub(" ", "_", names(marketing_data))
names(marketing_data) 
unique(marketing_data$Job_Code)
#############Preprocessing
#Step 0.1 : Remove unnecessary variables
marketing_data <- select(marketing_data, -Bill_To,-Customer_Name, -Address, -Contact, -Ticket_Number,-Current_Email)
#converting dates to subtracting from an epochdate= '2008-01-01'
epochdate <- as.Date('2008-01-01')
marketing_data$epochdate <- epochdate
View(marketing_data)
#converting setup date
marketing_data$setupday<- as.Date(marketing_data$Setup_Date)- marketing_data$epochdate
marketing_data$LastServiceDay<- as.Date(marketing_data$Last_Service_Date)- marketing_data$epochdate
marketing_data$CallDay<- as.Date(marketing_data$Call_Date)- marketing_data$epochdate
marketing_data$CompleteDay<- as.Date(marketing_data$Complete_Date)- marketing_data$epochdate
marketing_data$ScheduleDay<- as.Date(marketing_data$Schedule_Date)- marketing_data$epochdate
marketing_data$DispatchDay<- as.Date(marketing_data$Dispatch_Date)- marketing_data$epochdate
####################

#Step 1 : Order the data based on Customer ID and Call Duration
marketing_data_1 <- marketing_data[order(marketing_data$Customer_ID,marketing_data$Call_Date),]
#Sanity Check
View(marketing_data_1)
#Step 2 : generating new features
# F1: Customer age = last service day -first callday
# sanity checking for eah customer has a single last_service_day
check_last_service_day<-marketing_data_1 %>% group_by(Customer_ID, Last_Service_Date) %>% summarise(count=n())
View(check_last_service_day) # 1059 customer numbers
#seems there's one customer who changed branches between services. Lets flush out that customer
check_last_service_day %>% group_by(Customer_ID) %>% summarise(count=n()) %>% arrange(-count) %>% View
# The customer is C000282
#lets investigate this guy C000282
View(marketing_data_1)
# The customer moved from NY to CA. 
#Assuming the customer's behaviour changes when the customer moves from east coast to west coast
#The row is of no use for evaluaiton, so, lets delete the CHEEKTOWAGA (NY) Customer
marketing_data_churn_2 <- as.data.frame(setdiff(marketing_data_1, as.data.frame(subset(marketing_data_1, Customer_ID=="C000282" & Branch_ID =="B0004"))))
View(marketing_data_churn_2)
# Now that sanity check is done. Lets generate new features
# customer_age <- max(last_service_day)- min(call_day)
merge.df<-marketing_data_churn_2 %>% group_by(Customer_ID) %>% summarise(Cust_age=max(Last_Service_Date)-min(Call_Date)) %>% arrange(Customer_ID)
View(merge.df)

# merging data frames
marketing_data_churn_3<-merge(marketing_data_churn_2, merge.df, by = "Customer_ID", all.x = T)
rm(merge.df)
View(marketing_data_churn_3)

# Calculating the mean of the call_difference dates
mean_call_diff<- marketing_data_churn_3 %>% arrange(Customer_ID, Call_Date) %>% group_by(Customer_ID) %>% summarise(mean_call_diff= mean(CallDay))
View(mean_call_diff)  
marketing_data_churn_4<-merge(marketing_data_churn_3, mean_call_diff, by = "Customer_ID", all.x = T)

View(marketing_data_churn_4)
# calculate the average time taken to resolve a complaint
marketing_data_churn_4$time_taken_to_resolve <- as.Date(marketing_data_churn_4$Complete_Date)-as.Date(marketing_data_churn_4$Call_Date)
View(marketing_data_churn_4)
# calculate the mean call diff for each customer type and use them as cutoff
lookup_mean_call_day_basedOnCustType<-marketing_data_churn_4 %>% group_by(Customer_Type) %>% summarise(mean_call_day_cust_type =mean(CallDay))
# merging the dataframe (lookup_mean_call_day_basedOnCustType) to with marketing_data_churn_4
marketing_data_churn_5<-merge(marketing_data_churn_4, lookup_mean_call_day_basedOnCustType, by = "Customer_Type", all.x = T)
View(marketing_data_churn_5)
# if the mean_call_day_cust_type > mean_call_diff for each customer then the customer will churn, else no
marketing_data_churn_5$churned<- ifelse(marketing_data_churn_5$mean_call_day_cust_type<marketing_data_churn_5$mean_call_diff, 1, 0)
length(unique(marketing_data_churn_5$Job_Code))

# F3 : Inocrporating how many times a customer called for each rev code and job code
attach(marketing_data_churn_5)
rev_job_cust <- select(marketing_data_churn_5, Customer_ID, Rev_Code,Job_Code)
detach(marketing_data_churn_5)
jobCode_cast <- dcast(rev_job_cust,rev_job_cust$Customer_ID~rev_job_cust$Job_Code )
colnames(jobCode_cast)<-paste("Job_Code", colnames(jobCode_cast), sep = "_")
rev_code_cast <- dcast(rev_job_cust,rev_job_cust$Customer_ID~rev_job_cust$Rev_Code )
colnames(rev_code_cast)<-paste("Rev_Code",colnames(rev_code_cast), sep = "_")
colnames(rev_code_cast)[1]<-"Customer_ID"
colnames(jobCode_cast)[1]<-"Customer_ID"
View(jobCode_cast)
View(rev_code_cast)
# merging both rev_code and job_code 
job_rev <- merge(jobCode_cast, rev_code_cast, by = "Customer_ID", all.x = T)
View(job_rev)
rm(jobCode_cast)
rm(rev_code_cast)
# merging jeb_rev with marketing_data_churn_5
marketing_data_churn_6 <- merge(marketing_data_churn_5, job_rev, by = "Customer_ID", all.x = T)
View(marketing_data_churn_6)
#F4 : Avg_time for addressing the concern(TAC) : Some records have "2000-01-01", these records are assumed to be resolved 
#on the dispatch day

Dispatch_Date_1<-as.Date(ifelse(as.Date(marketing_data_churn_6$Dispatch_Date)==as.Date("2000-01-01"), as.Date(marketing_data_churn_6$Schedule_Date),as.Date(marketing_data_churn_6$Dispatch_Date)), origin = "1970-01-01")
marketing_data_churn_7 <- data.frame(Dispatch_Date_1, marketing_data_churn_6)
View(marketing_data_churn_7)
marketing_data_churn_7$DispatchDay_1<- as.Date(marketing_data_churn_7$Dispatch_Date_1)- marketing_data_churn_7$epochdate
#F5 : No of times SLA was breached for each customer
SLAbreach_df<-marketing_data_churn_7 %>% group_by(Customer_Type) %>% summarise(variance=var(time_taken_to_resolve), mean= mean(time_taken_to_resolve))
SLAbreach_df$slaBreach<-SLAbreach_df$mean+1.644*(SLAbreach_df$variance)
View(SLAbreach_df)
SLAbreach_df$variance <-NULL
SLAbreach_df$mean <- NULL
# Merging this with data frame
marketing_data_churn_8 <- merge(marketing_data_churn_7, SLAbreach_df, by = "Customer_Type", all.x = T)
View(marketing_data_churn_8)
marketing_data_churn_8$slaBreachFlag<- ifelse(marketing_data_churn_8$time_taken_to_resolve>marketing_data_churn_8$slaBreach, 1,0)
#slabreach for each customer
Count_SLAbreach<-marketing_data_churn_8 %>% group_by(Customer_ID) %>% summarise(SLABreach_number= sum(slaBreachFlag))
View(Count_SLAbreach)
marketing_data_churn_9 <- merge(marketing_data_churn_8, Count_SLAbreach, by = "Customer_ID", all.x = T)
View(marketing_data_churn_9)
slaBreach_df<-marketing_data_churn_9 %>% group_by(Customer_ID) %>% summarise(SLABreachCount= sum(slaBreachFlag))
marketing_data_churn_10 <- merge(marketing_data_churn_9, slaBreach_df, by = "Customer_ID", all.x = T)
View(marketing_data_churn_10)
#F5 Average time taken to resolve a complaint
avg_time_to_resolve <- marketing_data_churn_10 %>%group_by(Customer_ID) %>% summarise(mean_time_to_resolve= mean(time_taken_to_resolve)) 
View(avg_time_to_resolve)
marketing_data_churn_11 <- merge(marketing_data_churn_10, avg_time_to_resolve, by = "Customer_ID", all.x = T)
View(marketing_data_churn_11)
#Lets start building model
#Cleaning data for building model
#removing useless columns
write.csv(marketing_data_churn_11, "marketing_data_churn.csv")
colnames(marketing_data_churn_11)
useless_cols<- c("Customer_ID", "Dispatch_Date_1","Branch_ID", "City", "State", "Zip_Code", "Zip_5", "Setup_Date","Last_Service_Date", "Area_2", "Call_Date", "Complete_Date","Year_Month", "Year", "Week_Ending_Date"  )
marketing_data_churn_12 <- select(marketing_data_churn_11, -contains("ate"))
View(marketing_data_churn_12)

#importing data from Excel

marketing_data_churn_13<- read_excel("/Users/raki/Data Science/CrossSellingProject/marketing_data_churn_2.xls", sheet = 1, col_names = T)
str(marketing_data_churn_13)
marketing_data_churn_13$churned<- as.factor(as.character(marketing_data_churn_13$churned))
marketing_data_churn_13$Cust_Type<- as.factor(as.character(marketing_data_churn_13$Cust_Type))
marketing_data_churn_13$Area_1<- as.factor(as.character(marketing_data_churn_13$Area_1))
str(marketing_data_churn_13)
View(marketing_data_churn_13)
# Building a model
#split_train and Test
train_rows<- createDataPartition(marketing_data_churn_13$churned, p=0.7, times = 1, list = F)
train_data <- marketing_data_churn_13[train_rows,] 
test_data <- marketing_data_churn_13[-train_rows,] 
#building a naive bayes model
library(e1071)
NBModel<-naiveBayes(churned~., data = train_data)
# Accuracy on train data
#predict on train
NB_train<-predict(NBModel, newdata = train_data)
conf.mat = table(train_data$churned,NB_train)
accuracy_NB_train = sum(diag(conf.mat))/sum(conf.mat)
precision_NB_train = conf.mat[2,2]/sum(conf.mat[,2])
recall_NB_Train = conf.mat[2,2]/sum(conf.mat[2,])
predict(NBModel, newdata = test_data)

# Predict on test
NB_test<-predict(NBModel, newdata = test_data[,-1])
conf.mat_test = table(test_data$churned,NB_test)
accuracy_NB_test = sum(diag(conf.mat_test))/sum(conf.mat_test)
precision_NB_test = conf.mat_test[2,2]/sum(conf.mat_test[,2])
recall_NB_Test = conf.mat_test[2,2]/sum(conf.mat_test[2,])

########Using random forest model#################
library(randomForest)
model = randomForest(churned ~ ., data=train_data,keep.forest=TRUE, ntree=50)
# As model is not able to handle lets take out Area_1 and see
train_data <- train_data[, c(-3)]
test_data <- test_data[, c(-3)]
# Building the model again
model = randomForest(churned ~ ., data=train_data,keep.forest=TRUE, ntree=50)
varImpPlot(model)
# Lets take top 5 variables
train_data_2<- select(train_data, churned,mean_call_diff, DispatchDay_1, Cust_Type, Cust_age, Total_Call_revenue, mean_time_to_resolve)
model = randomForest(churned ~ ., data=train_data_2,keep.forest=TRUE, ntree=50)
##############Function for NB MOdel#######

NBModel<-naiveBayes(churned~., data = train_data_2)
  # Accuracy on train data
  #predict on train
  NB_train_2<-predict(NBModel, newdata = train_data_2)
  conf.mat_2 = table(train_data_2$churned,NB_train_2)
  accuracy_NB_train_2 = sum(diag(conf.mat_2))/sum(conf.mat_2)
  precision_NB_train_2 = conf.mat_2[2,2]/sum(conf.mat_2[,2])
  recall_NB_Train_2 = conf.mat_2[2,2]/sum(conf.mat_2[2,])
  print("accuracy is ",accuracy_NB_train_2)
  print("precision is ", precision_NB_train_2)
  print("recall is ", recall_NB_Train_2)




# Predict on test
  test_data_2<- select(test_data, churned,mean_call_diff, DispatchDay_1, Cust_Type, Cust_age, Total_Call_revenue, mean_time_to_resolve)
NB_test_2<-predict(NBModel, newdata = test_data[,-1])
conf.mat_test_2 = table(test_data_2$churned,NB_test_2)
accuracy_NB_test_2 = sum(diag(conf.mat_test_2))/sum(conf.mat_test_2)
precision_NB_test_2 = conf.mat_test_2[2,2]/sum(conf.mat_test_2[,2])
recall_NB_Test_2 = conf.mat_test_2[2,2]/sum(conf.mat_test_2[2,])

# using random forest on First and second data
rf_model_1 <- randomForest(churned ~ ., data=train_data,keep.forest=TRUE, ntree=500)
RF_model_train_1<-predict(rf_model_1, newdata = train_data)
rf_model_2 <- randomForest(churned ~ ., data=train_data_2,keep.forest=TRUE, ntree=500)
RF_model_train_2<-predict(rf_model_2, newdata = train_data_2)
########using random forest on train confusion matrix
# COnf matrix for RF Model 1
conf.mat_train_RF_1 = table(train_data$churned,RF_model_train_1)
accuracy_RF_train_1 = sum(diag(conf.mat_train_RF_1))/sum(conf.mat_train_RF_1)
precision_RF_train_1 = conf.mat_train_RF_1[2,2]/sum(conf.mat_train_RF_1[,2])
recall_RF_Train_1 = conf.mat_train_RF_1[2,2]/sum(conf.mat_train_RF_1[2,])

########using random forest on test confusion matrix
# COnf matrix for RF Model 1
RF_model_test_1 <- predict(rf_model_1, newdata = test_data)
conf.mat_test_RF_1 = table(test_data$churned,RF_model_test_1)
accuracy_RF_test_1 = sum(diag(conf.mat_test_RF_1))/sum(conf.mat_test_RF_1)
precision_RF_test_1 = conf.mat_test_RF_1[2,2]/sum(conf.mat_test_RF_1[,2])
recall_RF_Test_1 = conf.mat_test_RF_1[2,2]/sum(conf.mat_test_RF_1[2,])

########using random forest on train confusion matrix on model 2
# COnf matrix for RF Model 2

conf.mat_train_RF_2 = table(train_data_2$churned,RF_model_train_2)
accuracy_RF_train_2 = sum(diag(conf.mat_train_RF_2))/sum(conf.mat_train_RF_2)
precision_RF_train_2 = conf.mat_train_RF_2[2,2]/sum(conf.mat_train_RF_2[,2])
recall_RF_Train_2 = conf.mat_train_RF_2[2,2]/sum(conf.mat_train_RF_2[2,])

########using random forest on test confusion matrix on model 2
# COnf matrix for RF Model 2
RF_model_test_2 <- predict(rf_model_2, newdata = test_data_2)

conf.mat_test_RF_2 = table(test_data_2$churned,RF_model_test_2)
accuracy_RF_test_2 = sum(diag(conf.mat_test_RF_2))/sum(conf.mat_test_RF_2)
precision_RF_test_2 = conf.mat_test_RF_2[2,2]/sum(conf.mat_test_RF_2[,2])
recall_RF_Test_2 = conf.mat_test_RF_2[2,2]/sum(conf.mat_test_RF_2[2,])
