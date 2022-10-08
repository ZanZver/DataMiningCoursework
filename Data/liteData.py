import pandas as pd

CSVfilepath = "/Users/zanzver/Documents/BCU2/Masters/CMP7206-A-S1-2022:3_Data_Mining/Coursework/DataMiningCoursework/Data/hotel_bookings.csv"
dataset = pd.read_csv(CSVfilepath)

liteDataset = dataset.head(10000)
CSVLitePath = "/Users/zanzver/Documents/BCU2/Masters/CMP7206-A-S1-2022:3_Data_Mining/Coursework/DataMiningCoursework/Data/hotel_bookings_lite.csv"
liteDataset.to_csv(CSVLitePath)
