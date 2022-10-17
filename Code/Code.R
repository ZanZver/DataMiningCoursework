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



#==============================================================================================================
# Library downloads 
#==============================================================================================================



#==============================================================================================================
# Library imports
#==============================================================================================================



#==============================================================================================================
# Data import
#==============================================================================================================

# Our dataset paths
# Daniels data path
dataFolderDaniel <- "~/Documents/BCU2/Masters/" # DANIEL change that to whatever your path is
# Mihais data path
dataFolderMihai <- "~/Documents/BCU2/Masters/" # MIHAI change that to whatever your path is
# Zans data path
dataFolderZan <- "~/Documents/BCU2/Masters/CMP7206-A-S1-2022:3_Data_Mining/Coursework/DataMiningCoursework/Data/"

# Decide which file to use, lite dataset is used by default
if(TRUE){
  # Lite dataset, used for dev (only 10k rows)
  fileName <- "hotel_bookings_lite.csv"
}else{
  # Original dataset, used for test & prod
  fileName <- "hotel_bookings.csv"
}

# Function to load all of the data
dataLoader <- function(dataFolder, fileName, user) {
  tryCatch(               
    expr = {
      # Join persons data path with lite data path
      dataLite <- paste(dataFolder, fileName, sep = "")
      # Read CSV from dataLite path
      hotel_bookings <- read.csv(dataLite, stringsAsFactors=TRUE)
      # Return CSV
      return(hotel_bookings)
    },
    error = function(e){
      # Inform the user on whose profile the error occurred
      print(sprintf("Error for user: %s", user))
      # Print out the error
      print(e)
      # Return nothing
      return(NULL)
    },
    warning = function(w){
      # Inform the user on whose profile the warning occurred
      print(sprintf("Warning for user: %s", user))
      # Print out the warning
      print(w)
      # Return nothing
      return(NULL)
    },
    finally = {
      # Let the user know once the function has finished with execution
      print(sprintf("Dataloader for %s finished.", user))
    }
  )
}

# Var for switching the users
userNum <- 1
while(userNum < 5){
  print("=============================================================")
  # Go across the user profiles, save the data or throw an error if no data is saved at the end
  switch( userNum,  
          hotel_bookings <- dataLoader(dataFolderDaniel, fileName, "Daniel"),  
          hotel_bookings <- dataLoader(dataFolderMihai, fileName, "Mihai"),  
          hotel_bookings <- dataLoader(dataFolderZan, fileName, "Zan"),  
          stop("No data has been loaded") # Stops the code HERE if no data has been loaded
  ) 
  userNum <- userNum + 1 
  
  # Once the data is found (from the user), break the loop
  if(!is.null(hotel_bookings)){
    # Prints first 6 rows so you can see the data
    print(head(hotel_bookings))
    break
  }
}
print("=============================================================")


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

