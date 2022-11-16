# Function to load all of the data
dataLoader <- function(dataFolder, fileName) {
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
      # Print out the error
      print(e)
      # Return nothing
      return(NULL)
    },
    warning = function(w){
      # Inform the user on whose profile the warning occurred
      # Print out the warning
      print(w)
      # Return nothing
      return(NULL)
    },
    finally = {
      # Let the user know once the function has finished with execution
      print(sprintf("Dataloader finished."))
    }
  )
}

labelEncoder <- function(df){
  df <- as.numeric(factor(df))
  return (df)
}

oneHotEncoder <- function(df){
  df <- myData[, df, drop = FALSE]
  df0 <- encode_onehot(df)
  df0 <- cbind(df, df0)
  df0 <- encode_onehot(df, colname.sep = '-', drop1st = TRUE)
  df0 <- cbind(df, df0)
  #rm(df)
  #print(df0)
}

binaryEncoder <- function(df, varString){
  df <- ifelse(df == "varString",1,0)
  return (df)
}

ordinalEncoder <- function(df, name, orderList){
  cat.df <- df[, name, drop = FALSE]
  df <- encode_ordinal(cat.df, order = orderList)
  df <- as.numeric(unlist(df %>% select(name)))
  return (df)
}

encodeTheData <-function(df){
  df$hotel <- binaryEncoder(df$hotel,"City Hotel")
  
  orderList <- c("January", "February", "March", "April", "May", "June", "July", 
                 "August", "September", "October", "November", "December")
  df$arrival_date_month <- ordinalEncoder(df, "arrival_date_month", orderList)
  
  orderList <- c("Undefined", "SC", "BB", "HB", "FB")
  df$meal <- ordinalEncoder(df, "meal", orderList)
  
  df$country <- labelEncoder(df$country)
  
  df$market_segment <- labelEncoder(df$market_segment)
  
  df$distribution_channel <- labelEncoder(df$distribution_channel)
  
  df$deposit_type <- labelEncoder(df$deposit_type)
  
  df$customer_type <- labelEncoder(df$customer_type)
  
  df$reservation_status <- labelEncoder(df$reservation_status)
  
  df$reservation_status_date <- labelEncoder(df$reservation_status_date)
  
  orderList <- c("A","B","C","D","E","F","G","H","L","P")
  df$reserved_room_type <- ordinalEncoder(df, "reserved_room_type", orderList)
  
  orderList <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "P")
  df$assigned_room_type <- ordinalEncoder(df, "assigned_room_type", orderList)
}















