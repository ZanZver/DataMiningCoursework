# Function to load all of the data
dataLoader <- function(dataFolder, fileName, strAsFac=TRUE) {
  tryCatch(               
    expr = {
      # Join persons data path with lite data path
      fullDataPath <- paste(dataFolder, fileName, sep = "")
      # Read CSV from dataLite path
      readFile <- read.csv(fullDataPath, stringsAsFactors=strAsFac)
      # Return CSV
      return(readFile)
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
  df <- as.numeric(unlist(df %>% select(all_of(name))))
  return (df)
}

encodeTheData <-function(df,ISO_Codes){
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
  
  #df$reservation_status <- labelEncoder(df$reservation_status)
  
  #df$reservation_status_date <- labelEncoder(df$reservation_status_date)
  
  orderList <- c("A","B","C","D","E","F","G","H","L","P")
  df$reserved_room_type <- ordinalEncoder(df, "reserved_room_type", orderList)
  
  orderList <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "P")
  df$assigned_room_type <- ordinalEncoder(df, "assigned_room_type", orderList)
  
  # Create new dataframe with just GPD data and Country
  justGDP <- data.frame(GDP = ISO_Codes$GDP, Country = ISO_Codes$Alpha3Code)
  # Arrange GPD ASC
  justGDP <- arrange(justGDP, GDP)
  # Create a new list based on the Country
  justGDPList <- as.character(justGDP$Country)
  
  df$country <- ordinalEncoder(df, "country", justGDPList)

  return (df)
}

transformDataTypes <- function(df){
  df <- transform(df,
                  hotel <- as.character(hotel),
                  is_canceled <- as.integer(is_canceled),
                  lead_time <- as.integer(is_canceled),
                  arrival_date_year <- as.integer(arrival_date_year),
                  arrival_date_month <- as.character(arrival_date_month),
                  arrival_date_week_number <- as.integer(arrival_date_week_number),
                  arrival_date_day_of_month <- as.integer(arrival_date_day_of_month),
                  stays_in_weekend_nights <- as.integer(stays_in_weekend_nights),
                  stays_in_week_nights <- as.integer(stays_in_week_nights),
                  adults <- as.integer(stays_in_week_nights),
                  children <- as.integer(children),
                  babies <- as.integer(babies),
                  meal <- as.character(meal),
                  country <- as.character(country),
                  # market_segment,
                  # distribution_channel,
                  is_repeated_guest <- as.integer(is_repeated_guest),
                  previous_cancellations <- as.integer(previous_cancellations),
                  previous_bookings_not_canceled <- as.integer(previous_bookings_not_canceled),
                  reserved_room_type <- as.character(reserved_room_type),
                  assigned_room_type <- as.character(assigned_room_type),
                  booking_changes <- as.integer(booking_changes),
                  deposit_type <- as.character(deposit_type),
                  days_in_waiting_list <- as.integer(days_in_waiting_list),
                  customer_type <- as.character(customer_type),
                  adr <- as.integer(adr),
                  required_car_parking_spaces <- as.integer(required_car_parking_spaces),
                  total_of_special_requests <- as.integer(total_of_special_requests)
                  #reservation_status <- as.character(reservation_status),
                  #reservation_status_date <- as.POSIXct(df$reservation_status_date, format="%Y-%m-%d")
                  )
  return(df)
}

checkData <- function(df,isoDF){
  # "hotel" - Resort Hotel or City Hotel
  mystr <- sapply(list(c("Resort Hotel", "City Hotel")), paste, collapse = "|")
  df <- df[grepl(mystr, df$hotel),]
  
  # "is_canceled" - 0 or 1
  mystr <- sapply(list(c(0, 1)), paste, collapse = "|")
  df <- df[grepl(mystr, df$is_canceled),]


  # "lead_time" - int
  # "arrival_date_year" - int - maybe scope?
  # "arrival_date_week_number" - int - maybe scope?
  # "arrival_date_day_of_month" - int - maybe scope?
  # "stays_in_weekend_nights" - int
  # "stays_in_week_nights" - int
  # "adults" - int
  # "children" - int
  # "babies" - int
  # "previous_cancellations" - int
  # "previous_bookings_not_canceled" - int
  # "booking_changes" - int
  # "days_in_waiting_list" - int
  # "adr" - numeric
  # "required_car_parking_spaces" - int
  # "total_of_special_requests" - int
  #df[df < 0] <- NA
  #df <- na.omit(df)

  # "arrival_date_month" - 1-12
  mystr <- sapply(list(c(month.name)), paste, collapse = "|")
  df <- df[grepl(mystr, df$arrival_date_month),]

  # "meal" - Undefined/SC, BB, HB, FB
  mystr <- sapply(list(c("Undefined","SC", "BB", "HB", "FB")), paste, collapse = "|")
  df <- df[grepl(mystr, df$meal),]


  # "is_repeated_guest" - 0 or 1
  mystr <- sapply(list(c(0, 1)), paste, collapse = "|")
  df <- df[grepl(mystr, df$is_repeated_guest),]

  # "reserved_room_type" - "A","B","C","D","E","F","G","H","L","P"
  mystr <- sapply(list(c("A","B","C","D","E","F","G","H","L","P")), paste, collapse = "|")
  df <- df[grepl(mystr, df$reserved_room_type),]

  # "assigned_room_type" - "A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "P"
  mystr <- sapply(list(c("A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "P")), paste, collapse = "|")
  df <- df[grepl(mystr, df$assigned_room_type),]

  # "deposit_type" - No Deposit, Non Refund, Refundable
  mystr <- sapply(list(c("No Deposit", "Non Refund", "Refundable")), paste, collapse = "|")
  df <- df[grepl(mystr, df$deposit_type),]

  # "customer_type" - Contract, Group, Transient, Transient-party
  mystr <- sapply(list(c("Contract","Group", "Transient", "Transient-party")), paste, collapse = "|")
  df <- df[grepl(mystr, df$customer_type),]

  # "reservation_status" - Canceled, Check-Out, No-Show
  #mystr <- sapply(list(c("Canceled", "Check-Out", "No-Show")), paste, collapse = "|")
  #df <- df[grepl(mystr, df$reservation_status),]

  # "country" - list of Alpha-3 code ISO standard can be found here: https://www.iso.org/obp/ui/#search
  mystr <- sapply(list(levels(isoDF$Alpha3Code)), paste, collapse = "|")
  df <- df[grepl(mystr, df$country),]
  
  return (df)
}

dataSpliter <- function(data, mySeed=1234){
  data$is_canceled[data$is_canceled == 0] <- 'No'
  data$is_canceled[data$is_canceled == 1] <- 'Yes'
  data$is_canceled <- factor(data$is_canceled)
  
  set.seed(mySeed)
  ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
  training <- data[ind == 1,]
  test <- data[ind == 2,]
  
  set.seed(mySeed)
  mtry <- sqrt(ncol(data)) #the number of variables to be randomly sampled for each split
  tunegrid <- expand.grid(.mtry=mtry) #put it in a list for tuneGrid (cuz it only takes in lists)
  
  return(list(training, test, mtry, tunegrid))
}

createConfusionMatrix <- function(pred, test, cfSavePath, cfFileName, cfTitle){
  confMatrix <- confusionMatrix(pred, test$is_canceled )[["table"]]
  
  # Start "drawing" confusion matrix
  TClass <- factor(c("T", "T", "F", "F"))
  PClass <- factor(c("T", "F", "T", "F"))
  Y      <- c(confMatrix[1:4])
  df <- data.frame(TClass, PClass, Y)
  
  pdf(paste(cfSavePath, cfFileName, sep = ""))
  p <- ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
    scale_fill_gradient(low = "lightgreen", high = "darkgreen") +
    labs(x = "Actual values", y = "Expected values", title = cfTitle) +
    theme_bw() +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
  print(p)
  dev.off()
}

knnFunction <- function(cfSavePath, knnNumber, kfoldNumber, training, test, tunegrid){
  trControl <- trainControl(method = "repeatedcv", 
                         number = 3, 
                         repeats = 3,
                         classProbs = TRUE, 
                         summaryFunction = twoClassSummary)
  #set.seed(3333)
  model <- train(is_canceled ~., 
                   data = training, 
                   method = "knn",
                   tuneLength = 18,
                   trControl=trControl,
                   metric = "ROC")
  
  pred <- predict(model, newdata = test)
  createConfusionMatrix(pred, test, cfSavePath, cfFileName = "KNNConfusionMatrix.pdf", cfTitle = "KNN Confusion Matrix")
  
  return(model)
}

rfFunction <- function(cfSavePath, training, test, mtry, tunegrid){
  trControl <- trainControl(method = "repeatedcv",
                            number = 3,
                            repeats = 3,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)
  
  model <- train(is_canceled ~ .,
                 data = training,
                 method = 'rf',
                 tuneLength = 18,
                 trControl = trControl,
                 metric = "ROC",
                 tuneGrid = tunegrid)

  # Create confusion matrix
  pred <- predict(model, newdata = test)
  createConfusionMatrix(pred, test, cfSavePath, cfFileName = "RFConfusionMatrix.pdf", cfTitle = "Random Forest Confusion Matrix")
  
  return(model)
}

lrFunction <- function(cfSavePath, training, test, mtry, tunegrid){
  trControl <- trainControl(method = "repeatedcv",
                            number = 3,
                            repeats = 3,
                            classProbs = TRUE,
                            summaryFunction = twoClassSummary)

  #the number of iterations is decided automatically using ROC. so there is no need to specify nIter :)
  model <- train(is_canceled ~ .,
               data = training,
               method = 'LogitBoost',
               tuneLength = 18,
               trControl = trControl,
               metric = "ROC")
  
  # Create confusion matrix
  pred <- predict(model, newdata = test)
  createConfusionMatrix(pred, test, cfSavePath, cfFileName = "LRConfusionMatrix.pdf", cfTitle = "Linear regression Confusion Matrix")
  
  return(model)
}








