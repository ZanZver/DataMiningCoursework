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

#the number of iterations is decided automatically using ROC. so there is no need to specify nIter :)
fit <- train(is_canceled ~ .,
             data = training,
             method = 'LogitBoost',
             tuneLength = 18,
             trControl = trControl,
             metric = "ROC")

fit

varImp(fit)

pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$is_canceled)

