# Original file path
CSVfilepath <- "/Data/hotel_bookings.csv"
# Save CSV to dataframe
wholeData <- read.csv(CSVfilepath,stringsAsFactors = TRUE, header = TRUE, sep = ",")

# Get first 10000 rows as lite dataframe
liteData <- head(wholeData, 10000)
# Declare lite data path
CSVLitePath <- "/Data/hotel_bookings_lite.csv"
# Save lite dataset to the folder
write.csv(liteData, CSVLitePath)