# Original file path
CSVfilepath <- "~/Uni/CMP726_Data_Mining/DataMiningCoursework/Data/hotel_bookings.csv"
# Save CSV to data-frame
wholeData <- read.csv(CSVfilepath, stringsAsFactors = TRUE, header = TRUE, sep = ",")

chart_missing_values <- function(data_frame, path=NULL) {
  missing_count <- vector(mode = "numeric", length = length(colnames(wholeData)))
  for(i in 1:ncol(data_frame)) {
    missing_count[i] <- sum(is.na(data_frame[ , i]))
  }
  par(mar=c(3, 15, 3, 1))
  barplot(missing_count, main="Null values", horiz=TRUE, las=1, names.arg=colnames(data_frame))
  print("Barplot generated")
}


chart_missing_values(wholeData)

?par