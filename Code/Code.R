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


dev <- FALSE #if(TRUE) uses litedataset

# Our dataset paths
# Daniels data path
dataFolderDaniel <- "D:/UNI/CMP7206-DM/DataMiningCoursework/"
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

packages <- c("dplyr",
              "explore",
              "ggcorrplot",
              "ggplot2",
              "cleandata",
              "caret",
              "KODAMA",
              "pROC",
              "mlbench",
              "Rfast")

for(p in packages)
{
  tryCatch(test <- require(p,character.only=T), 
           warning=function(w) return())
  if(!test)
  {
    print(paste("Package", p, "not found. Installing Package!"))
    install.packages(p)
    require(p)
  }
}

source(paste(usersDataFolder, "Code/function.R", sep = ""))
#==============================================================================================================
# Data import
#==============================================================================================================

# Decide which file to use, lite dataset is used by default
if(dev){
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

# Remove agent, company, reservation_status and reservation_status_date
hotel_bookings <- hotel_bookings[,!(names(hotel_bookings) %in% c("agent","company", "reservation_status", "reservation_status_date","X"))]

# Remove na columns
hotel_bookings <- na.omit(hotel_bookings) 

# Remove items that are out of bounds
hotel_bookings <- checkData(hotel_bookings, ISO_Codes)

# Change the data types
hotel_bookings <- transformDataTypes(hotel_bookings)

hotel_bookings_clean <- hotel_bookings

print("=============================================================")

# Encode the data
hotel_bookings <- encodeTheData(hotel_bookings,ISO_Codes)
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
# Data exploration
#==============================================================================================================
#explore(hotel_bookings)

#==============================================================================================================
# Data preparation / Data pre-processing???
#==============================================================================================================
# Inspect the data
#str(hotel_bookings)
# Check the column names
#colnames(hotel_bookings)

hotel_bookings_lite <- head(hotel_bookings,50000)

dataSplit <- dataSpliter(hotel_bookings_lite, 1234)
globalTraining <- dataSplit[[1]]
globalTest <- dataSplit[[2]]
globalMtry <- dataSplit[[3]]
globalTunegrid <- dataSplit[[4]]


source(paste(usersDataFolder, "Code/function.R", sep = ""))

knnModel <- knnFunction(paste(usersDataFolder, "Data/", sep = ""), 
             knnNumber = 3, 
             training = globalTraining, 
             test = globalTest, 
             tunegrid = globalTunegrid)

knnModel

rfModel <- rfFunction(paste(usersDataFolder, "Data/", sep = ""),
           training = globalTraining,
           test = globalTest,
           mtry = globalMtry,
           tunegrid = globalTunegrid
           )

lrModel <- lrFunction(paste(usersDataFolder, "Data/", sep = ""),
           training = globalTraining,
           test = globalTest,
           mtry = globalMtry,
           tunegrid = globalTunegrid)

#plot.df = data.frame(test, predicted = fit)


# Satges based on this article: https://monkeylearn.com/blog/data-cleaning-steps/
# 1) Remove irrelevant data

# 2) Deduplicate your data

# 3) Fix structural errors

# 4) Deal with missing data

# 5) Filter out data outliers

# 6) Validate your data

