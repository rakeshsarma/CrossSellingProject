# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject")

library(readxl)
library(dplyr)
library(XLConnect)
library(caret)

#importing data
marketing_data<- read_excel("~/Data Science/CrossSellingProject/Target Marketing and cross selling - Data.xls")

#replace space with underscore
names(marketing_data) <- gsub(" ", "_", names(marketing_data))
names(marketing_data) 

# Roll the data to Customer Level
summary(marketing_data$Customer_Type)
unique(marketing_data$Customer_Type)
marketing_data_1 <- marketing_data[order(marketing_data$Customer_ID),]
View(marketing_data_1)
verify <-marketing_data %>% group_by(Customer_Type) %>% count(Customer_Type)

attach(marketing_data_1)
class(verify)
sum(verify$n)
summary(marketing_data$Customer_Type)
summary(marketing_data$Setup_Date)
#remove unwanted columns

marketing_data_2 <- select(marketing_data_1, -c(Customer_Name:Contact, Ticket_Number,Current_Email, Week_Ending_Date, Call_Time, Schedule_Time, Dispatch_Time, Complete_Time))
marketing_data_2 <- marketing_data_2[order(marketing_data_2$Customer_ID,marketing_data_2$Call_Date),]
View(marketing_data_2)
# Checking if schedule date is same as dispatch date 

sum(marketing_data_2$Schedule_Date!=marketing_data_2$Dispatch_Date)
# NO it is not same
min(marketing_data_2$Setup_Date)
marketing_data_2$epoch_date <- as.Date("2008-09-30")
marketing_data_2$days_from_epoch <- as.Date(marketing_data_2$Call_Date)-epoch_date
days_bw_calls <- diff(marketing_data_2$Call_Date[1:length(marketing_data_2$Call_Date)-1],marketing_data_2$Call_Date[2:length(marketing_data_2$Call_Date)], units = "days")
days_bw_calls <- diff(marketing_data_2$days_from_epoch)
length(days_bw_calls)
# FInding the average of days between calls
marketing_data_3<-marketing_data_2 %>% arrange(Customer_ID, Call_Date) %>% group_by(Customer_ID) %>% mutate(diff =  as.Date(Call_Date)- lag(as.Date(Call_Date)))

View(marketing_data_3)
sum(marketing_data_3$churned, na.rm = T)
# assumption that if a customer didn't call in 365 days, the person is a chusrned customer
marketing_data_3$churned <- ifelse(marketing_data_3$diff>366,1,0)
marketing_data_1$avg_cust_call_days <- marketing_data_1 %>% group_by(Customer_ID) %>% mean(Customer_Type)
marketing_data_1$days_to_resolve_problem <-  as.Date(marketing_data_1$Complete_Date)-as.Date(marketing_data_1$Call_Date)
View(marketing_data_1)
marketing_data_1$problem_occur_days_fromSetup_date <- as.Date(marketing_data_1$Call_Date, "%Y-%m-%d")-as.Date(marketing_data_1$Setup_Date, "%Y-%m-%d")
View(marketing_data_1[marketing_data_1$Call_Date<marketing_data_1$Setup_Date,])
mean(marketing_data_3$diff[marketing_data_3$Customer_Type=="CustType01"], na.rm=T)
mean(marketing_data_3$diff[marketing_data_3$Customer_Type=="CustType02"], na.rm=T)
mean(marketing_data_3$diff[marketing_data_3$Customer_Type=="CustType0"], na.rm=T)
