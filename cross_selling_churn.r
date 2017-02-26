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
