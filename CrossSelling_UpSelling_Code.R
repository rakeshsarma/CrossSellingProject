# Remove Environment variables
rm(list = ls(all.names = T))

#set workspace
setwd("~/Data Science/CrossSellingProject")

#libraries
library(readxl)



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

