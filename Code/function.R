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