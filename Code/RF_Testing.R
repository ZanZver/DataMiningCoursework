library(caret)
library(pROC)
library(mlbench)

source("function.R")
source("Code.R")

str(hotel_bookings)

hotel_bookings$is_canceled[hotel_bookings$is_canceled == 0] <- 'No'
hotel_bookings$is_canceled[hotel_bookings$is_canceled == 1] <- 'Yes'
hotel_bookings$is_canceled <- factor(hotel_bookings$is_canceled)

set.seed(1234)
ind <- sample(2, nrow(hotel_bookings), replace = T, prob = c(0.7, 0.3))
ind
training <- hotel_bookings[ind == 1,]
head(training)
test <- hotel_bookings[ind == 2,]
head(test)

trControl <- trainControl(method = "repeatedcv",
                          number = 3,
                          repeats = 3,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(69)


mtry <- sqrt(ncol(hotel_bookings)) #the number of variables to be randomly sampled for each split
tunegrid <- expand.grid(.mtry=mtry) #put it in a list for tuneGrid (cuz it only takes in lists)

fit <- train(is_canceled ~ .,
             data = training,
             method = 'rf',
             tuneLength = 18,
             trControl = trControl,
             metric = "ROC",
             tuneGrid = tunegrid)

fit

varImp(fit)

pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$is_canceled)

