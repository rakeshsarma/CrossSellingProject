# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject") 

library(readxl)
library(dplyr)
library(XLConnect)
library(caret)

marketing_data<- read_excel("~/Data Science/CrossSellingProject/Target Marketing and cross selling - Data.xls")

#replace space with underscore
names(marketing_data) <- gsub(" ", "_", names(marketing_data))
names(marketing_data) 

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
#Step 4: Getting Rid of the first and the last colums

marketing_data_4<- marketing_data_3 %>% arrange(Customer_ID, Call_Date) %>%
  slice(c(3: n()-1)) %>%
  ungroup()
nrow(marketing_data_4)
View(marketing_data_4)

#Step 4: Getting Rid of the special cases
View(marketing_data_5[which(Customer_ID=='C000197'),])
marketing_data_5<-marketing_data_4[which((!is.na(marketing_data_4$diff))),]

(nrow(marketing_data_5)-nrow(marketing_data_3))/2
# Find mean call duration  for each customer type
cust_type_call_duration <- marketing_data_5 %>% group_by(Customer_Type) %>% summarise(mean_call_duration = mean(diff))

mean(marketing_data_5$diff)
