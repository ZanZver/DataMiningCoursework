#==============================================================================================================
# Project name: Data Mining on hotel dataset 
#
# Team:
#      Daniel Rimaru, student ID: 19134702
#      Mihai Nastase, student ID: 19112421
#      Zan Zver, student ID: 18133498
#
# Available on GitHub https://github.com/ZanZver/DataMiningCoursework
#==============================================================================================================

# Our dataset paths
# Daniels data path
dataFolderDaniel <- "~/Daniel/Sleep/" # DANIEL change that to whatever your path is
# Mihais data path
dataFolderMihai <- "~/Uni/CMP726_Data_Mining/DataMiningCoursework/"
# Zans data path
dataFolderZan <- "~/Documents/BCU2/Masters/CMP7206-A-S1-2022:3_Data_Mining/Coursework/DataMiningCoursework/"

if(dir.exists(dataFolderDaniel)){
  print("Welcome Deniel")
  usersDataFolder <- dataFolderDaniel
}else if(dir.exists(dataFolderMihai)){
  print("Welcome Mihai")
  usersDataFolder <- dataFolderMihai
}else if(dir.exists(dataFolderZan)){
  print("Welcome Zan")
  usersDataFolder <- dataFolderZan
}else{
  stop("No path has been found")
}


#==============================================================================================================
# Library downloads 
#==============================================================================================================
#install.packages("dplyr")
#install.packages("caret")

#==============================================================================================================
# Library imports
#==============================================================================================================
library(dplyr)
library(explore)
library(ggcorrplot)
library(ggplot2)
library(cleandata)
library(caret)

source(paste(usersDataFolder, "Code/function.R", sep = ""))
#==============================================================================================================
# Data import
#==============================================================================================================

# Decide which file to use, lite dataset is used by default
if(FALSE){
  # Lite dataset, used for dev (only 10k rows)
  fileName <- "hotel_bookings_lite.csv"
}else{
  # Original dataset, used for test & prod
  fileName <- "hotel_bookings.csv"
}

hotel_bookings <- dataLoader(paste(usersDataFolder, "Data/", sep = ""), fileName)
if(!is.null(hotel_bookings)){
  # Prints first 6 rows so you can see the data
  print(head(hotel_bookings))
}else{
  stop("No data has been loaded") 
}
hotel_bookings_original <- hotel_bookings

ISO_Codes <- dataLoader(paste(usersDataFolder, "Data/SupportingData/", sep = ""), "ISO_Code.csv", FALSE)
if(!is.null(ISO_Codes)){
  # Prints first 6 rows so you can see the data
  print(head(ISO_Codes))
}else{
  stop("No data has been loaded") 
}

# Remove agent and company
hotel_bookings <- hotel_bookings[,!(names(hotel_bookings) %in% c("agent","company", "reservation_status", "reservation_status_date"))]

# Remove na columns
hotel_bookings <- na.omit(hotel_bookings) 

# Remove items that are out of bounds
hotel_bookings <- checkData(hotel_bookings, ISO_Codes)

# Change the data types
hotel_bookings <- transformDataTypes(hotel_bookings)

hotel_bookings_clean <- hotel_bookings

print("=============================================================")

# Encode the data
hotel_bookings <- encodeTheData(hotel_bookings)
str(hotel_bookings)

hotel_bookings_clean_encoded <- hotel_bookings

write.csv(hotel_bookings, 
          paste(paste(usersDataFolder, "Data/", sep = ""), "hotel_bookings_clean.csv", sep = ""), 
          row.names = TRUE)


pdf(paste(paste(usersDataFolder, "Data/", sep = ""), "my_plot.pdf", sep = ""))
ggcorrplot(cor(hotel_bookings)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1, size = 7),
          axis.text.y = element_text(size = 7))
dev.off() 

#==============================================================================================================
# Drop columns
#==============================================================================================================
# Columns that are being dropped: agent,company
#hotel_bookings <- subset(hotel_bookings, select = -c(agent,company) )

print("=============================================================")

#==============================================================================================================
# Data exploration
#==============================================================================================================
str(hotel_bookings)

# Get the col names
hotelColNames <- colnames(hotel_bookings)

# Explore hotel bookings
#explore(hotel_bookings)

#==============================================================================================================
# Data preparation / Data pre-processing”
#==============================================================================================================
# Inspect the data
str(hotel_bookings)
# Check the column names
colnames(hotel_bookings)

# Split the data
#specify the cross-validation method
#install.packages("Rfast")
library(Rfast)

x <- as.matrix(hotel_bookings%>% select(-is_canceled))
y <- hotel_bookings$is_canceled
mod <- knn.cv(#folds = NULL, 
              nfolds = 10, 
              stratified = TRUE,
              seed = FALSE, 
              y = y, 
              x = x, 
              k = 6, 
              dist.type = "euclidean",
              type = "C",
              method = "average", 
              freq.option = 0,
              pred.ret = FALSE
              #mem.eff = FALSE
              )
mod

# Create a dataframe to simplify charting
plot.df = data.frame(test, predicted = fit)


# Satges based on this article: https://monkeylearn.com/blog/data-cleaning-steps/
# 1) Remove irrelevant data

# 2) Deduplicate your data

# 3) Fix structural errors

# 4) Deal with missing data

# 5) Filter out data outliers

# 6) Validate your data

