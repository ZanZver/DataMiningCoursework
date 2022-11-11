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


#==============================================================================================================
# Library imports
#==============================================================================================================
library(dplyr)

source(paste(usersDataFolder, "Code/function.R", sep = ""))
#==============================================================================================================
# Data import
#==============================================================================================================

# Decide which file to use, lite dataset is used by default
if(TRUE){
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

print("=============================================================")

#==============================================================================================================
# Drop columns
#==============================================================================================================
# Columns that are being dropped: agent,company
#hotel_bookings <- subset(hotel_bookings, select = -c(agent,company) )

print("=============================================================")

#==============================================================================================================
# Data exploration
#==============================================================================================================
summary(hotel_bookings)

# Get the col names
hotelColNames <- colnames(hotel_bookings)

# Loop across hotelColNames
for (i in 1:length(hotelColNames)){
  print("=============================================================")
  print(hotelColNames[i]) # Print col name
  tempTable <- table(hotel_bookings%>%dplyr::pull(i)) # Get num of occurrences
  print(tempTable) # Print num of occurrences
  
  # To save the files, works for Zan atm
  tempBarFile <- paste(hotelColNames[i], ".pdf", sep = "")
  tempTotalPath <- paste(paste(usersDataFolder, "Data/", sep = ""), "Graphs/", sep = "")
  tempFinalPath <- paste(tempTotalPath, tempBarFile, sep = "")
  pdf(tempFinalPath) 
  #jpeg(file=tempFinalPath)
  barplot(tempTable, main = hotelColNames[i], xlab = "Attributes", ylab = "Num. of occurrences") # Visualize num of occurrences
  dev.off()
  
  print("=============================================================")
}

#==============================================================================================================
# Data preparation / Data pre-processing”
#==============================================================================================================
# Inspect the data
str(hotel_bookings)
# Check the column names
colnames(hotel_bookings)


# Satges based on this article: https://monkeylearn.com/blog/data-cleaning-steps/
# 1) Remove irrelevant data

# 2) Deduplicate your data
duplicated(hotel_bookings)
sum(duplicated(hotel_bookings))

# 3) Fix structural errors

# 4) Deal with missing data

# 5) Filter out data outliers

# 6) Validate your data

