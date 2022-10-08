# DataMiningCoursework
Group project for Data Mining module.

## Table of Contents
- [DataMiningCoursework](#dataminingcoursework)
  - [Table of Contents](#table-of-contents)
  - [About](#about)
    - [Dataset](#dataset)
    - [Team](#team)
  - [Structure](#structure)

## About
In this project, our team is going to use data mining concepts to explore hotel data. At the moment we are not sure what we are going to get from the data. This is going to be updated!

### Dataset

Size of the dataset is 16.9 MB. It contains 32 columns and 119391 rows. In the table bellow, you can see all the variables and their descriptions.

Variable | Type | Description | Source/Engineering
---|---|---|---
ADR | Numeric | Average Daily Rate as defined by American Hotel & Lodging Association Uniform System of Accounts for the Lodging Industry | BO, BL and TR / Calculated by dividing the sum of all lodging transactions by the total number of staying nights
Adults | Integer | Number of adults | BO and BL
Agent | Categorical | ID of the travel agency that made the booking | BO and BL
ArrivalDateDayOfMonth | Integer | Day of the month of the arrival date | BO and BL
ArrivalDateMonth | Categorical | Month of arrival date with 12 categories: “January” to “December” | BO and BL
ArrivalDateWeekNumber | Integer | Week number of the arrival date | BO and BL
ArrivalDateYear | Integer | Year of arrival date | BO and BL
AssignedRoomType | Categorical | Code for the type of room assigned to the booking. Sometimes the assigned room type differs from the reserved room type due to hotel operation reasons (e.g. overbooking) or by customer request. Code is presented instead of designation for anonymity reasons | BO and BL
Babies | Integer | Number of babies | BO and BL
BookingChanges | Integer | Number of changes/amendments made to the booking from the moment the booking was entered on the PMS until the moment of check-in or cancellation | BO and BL/Calculated by adding the number of unique iterations that change some of the booking attributes, namely: persons, arrival date, nights, reserved room type or meal
Children | Integer | Number of children | BO and BL/Sum of both payable and non-payable children
Company | Categorical | ID of the company/entity that made the booking or responsible for paying the booking. ID is presented instead of designation for anonymity reasons | BO and BL.
Country | Categorical | Country of origin. Categories are represented in the ISO 3155–3:2013 format (https://www.iso.org/obp/ui/#) | BO, BL and NT
CustomerType | Categorical | Type of booking, assuming one of four categories:  <ul><li>Contract - when the booking has an allotment or other type of contract associated to it</li> <li>Group – when the booking is associated to a group</li> <li>Transient – when the booking is not part of a group or contract, and is not associated to other transient booking</li> <li>Transient-party – when the booking is transient, but is associated to at least other transient booking</li> </ul> | BO and BL
DaysInWaitingList | Integer | Number of days the booking was in the waiting list before it was confirmed to the customer | BO/Calculated by subtracting the date the booking was confirmed to the customer from the date the booking entered on the PMS
DepositType | Categorical | Indication on if the customer made a deposit to guarantee the booking. This variable can assume three categories: <ul> <li>No Deposit – no deposit was made</li> <li>Non Refund – a deposit was made in the value of the total stay cost</li> <li>Refundable – a deposit was made with a value under the total cost of stay</li></ul> | BO and TR/Value calculated based on the payments identified for the booking in the transaction (TR) table before the booking׳s arrival or cancellation date. <br> In case no payments were found the value is “No Deposit”. <br> If the payment was equal or exceeded the total cost of stay, the value is set as “Non Refund”. <br> Otherwise the value is set as “Refundable”.
DistributionChannel | Categorical | Booking distribution channel. The term “TA” means “Travel Agents” and “TO” means “Tour Operators” | BO, BL and DC
IsCanceled | Categorical | Value indicating if the booking was canceled (1) or not (0) | BO
IsRepeatedGuest | Categorical | Value indicating if the booking name was from a repeated guest (1) or not (0) | BO, BL and C/ Variable created by verifying if a profile was associated with the booking customer. If so, and if the customer profile creation date was prior to the creation date for the booking on the PMS database it was assumed the booking was from a repeated guest
LeadTime | Integer | Number of days that elapsed between the entering date of the booking into the PMS and the arrival date | BO and BL/ Subtraction of the entering date from the arrival date
MarketSegment | Categorical | Market segment designation. In categories, the term “TA” means “Travel Agents” and “TO” means “Tour Operators” | BO, BL and MS
Meal | Categorical | Type of meal booked. Categories are presented in standard hospitality meal packages: <ul> <li>Undefined/SC – no meal package;</li> <li>BB – Bed & Breakfast;</li> <li>HB – Half board (breakfast and one other meal – usually dinner);</li> <li>FB – Full board (breakfast, lunch and dinner)</li> </ul> | BO, BL and ML
PreviousBookingsNotCanceled | Integer | Number of previous bookings not cancelled by the customer prior to the current booking | BO and BL / In case there was no customer profile associated with the booking, the value is set to 0. Otherwise, the value is the number of bookings with the same customer profile created before the current booking and not canceled.
PreviousCancellations | Integer | Number of previous bookings that were cancelled by the customer prior to the current booking | BO and BL/ In case there was no customer profile associated with the booking, the value is set to 0. Otherwise, the value is the number of bookings with the same customer profile created before the current booking and canceled.
RequiredCardParkingSpaces | Integer | Number of car parking spaces required by the customer | BO and BL
ReservationStatus | Categorical | Reservation last status, assuming one of three categories: <ul> <li>Canceled – booking was canceled by the customer</li> <li>Check-Out – customer has checked in but already departed</li> <li>No-Show – customer did not check-in and did inform the hotel of the reason why</li> </ul> | BO
ReservationStatusDate | Date | Date at which the last status was set. This variable can be used in conjunction with the ReservationStatus to understand when was the booking canceled or when did the customer checked-out of the hotel | BO
ReservedRoomType | Categorical | Code of room type reserved. Code is presented instead of designation for anonymity reasons | BO and BL
StaysInWeekendNights | Integer | Number of weekend nights (Saturday or Sunday) the guest stayed or booked to stay at the hotel | BO and BL/ Calculated by counting the number of weekend nights from the total number of nights
StaysInWeekNights | Integer | Number of week nights (Monday to Friday) the guest stayed or booked to stay at the hotel | BO and BL/Calculated by counting the number of week nights from the total number of nights
TotalOfSpecialRequests | Integer | Number of special requests made by the customer (e.g. twin bed or high floor) | BO and BL/Sum of all special requests

Dataset that is in use for this project originated from this source:
https://www.sciencedirect.com/science/article/pii/S2352340918315191

Dataset can be downloaded from here:
https://www.kaggle.com/datasets/jessemostipak/hotel-booking-demand

After you have downloaded the file (filename: hotel_bookings.csv), paste it in the Data folder.


### Team
- Daniel Rimaru
  - Student ID: 19134702
  - Responsible for: ABC
- Mihai Nastase
   - Student ID: 19112421
   - Responsible for: XYZ
- Zan Zver
   - Student ID: 18133498
   - Responsible for: 123

## Structure
GitHub structure can be found bellow. This will be updated as the project develops.
```
DataMiningCoursework
│   README.md
│
└─── Code
│   │   code.R
│   │
│   └───subfolder1
│       │   otherCode.R
│       │   ...
│   
└─── Data
│   │   originalData.csv
│   │   channgedData.csv
│    
└─── Documents
    │   doc1.txt
    │   doc2.txt
```