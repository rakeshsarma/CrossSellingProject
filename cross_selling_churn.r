# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject") 


library(readxl)
library(XLConnect)
library(caret)
library(dplyr)
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
#Assuming the customer's behavious changes when the customer moves from east coast to west coast
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
# calculate the mean call diff for each customer type and use them as cutoff
lookup_mean_call_day_basedOnCustType<-marketing_data_churn_4 %>% group_by(Customer_Type) %>% summarise(mean_call_day_cust_type =mean(CallDay))
# merging the dataframe to 