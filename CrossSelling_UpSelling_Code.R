# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject")

#libraries
library(readxl)
library(dplyr)
library(XLConnect)
#install.packages("lubridate")
#install.packages("XLConnect")
#install.packages("rJava")



#importig data
#marketing_data <- read_excel(file.choose())
marketing_data<- read_excel("~/Data Science/CrossSellingProject/Target Marketing and cross selling - Data.xls")


View(marketing_data)
summary(marketing_data)
str(marketing_data)
dim(marketing_data) # There are 5284 rows and 31 columns
names(marketing_data)
unique(marketing_data$`Customer ID`)
# Replacing space with underscore
names(marketing_data) <- gsub(" ", "_", names(marketing_data))
names(marketing_data) # sanity check
str(marketing_data)
boxplot(marketing_data$Ticket_Revenue)
hist(marketing_data$Ticket_Revenue)
max(marketing_data$Ticket_Revenue)
mean(marketing_data$Ticket_Revenue)
# playing with data
b<-filter(marketing_data, Ticket_Revenue<2000 )
hist(marketing_data$Ticket_Revenue, breaks = 6000)
mean(marketing_data$Ticket_Revenue)

uniqueNos <- data.frame(apply(marketing_data, 2,function(x){length(unique(x))}))
table(uniqueNos)
View(uniqueNos)
str(marketing_data)
chisq.test(marketing_data$Address, marketing_data$Zip_Code)
marketing_data$addrNo<-as.integer(substr(marketing_data$Address, 1, nchar(marketing_data$Address)-8))
cor(marketing_data$addrNo, marketing_data$Zip_Code)
sum(marketing_data$Dispatch_Date- marketing_data$Schedule_Date)
sum(is.na(marketing_data$Customer_Name))
NAs<-data.frame(apply(marketing_data, 2,function(x){sum(is.na(x))}))

hist(marketing_data$Call_Date, breaks = 120)
epoch <- as.Date("2010-12-21")
marketing_data$days <- as.integer(as.Date(marketing_data$Call_Date)-epoch)
min(marketing_data$Call_Date)
View(marketing_data)

# dataset division train 
library(caret)
train_rows<- createDataPartition(marketing_data$days, p=0.5,  list = F)
train_data <- marketing_data[train_rows,]
test_data <- marketing_data[-train_rows,]

