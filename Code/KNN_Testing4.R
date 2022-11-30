install.packages("caret")
install.packages("pROC")
install.packages("mlbench")

library(caret)
library(pROC)
library(mlbench)

head(hotel_bookings,2)

#data <- read.csv("https://raw.githubusercontent.com/finnstats/finnstats/main/binary.csv", header = T)
data <- head(hotel_bookings,20000)
str(data)

data$is_canceled[data$is_canceled == 0] <- 'No'
data$is_canceled[data$is_canceled == 1] <- 'Yes'
data$is_canceled <- factor(data$is_canceled)

#data$hotel[data$hotel == 0] <- 'A'
#data$hotel[data$hotel == 1] <- 'B'
#data$hotel <- factor(data$hotel)

set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
ind
training <- data[ind == 1,]
head(training)
test <- data[ind == 2,]
head(test)

trControl <- trainControl(method = "repeatedcv",
                          number = 3,
                          repeats = 3,
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

set.seed(222)
fit <- train(is_canceled ~ .,
             data = training,
             method = 'knn',
             tuneLength = 18,
             trControl = trControl,
             preProc = c("center", "scale"),
             metric = "ROC",
             tuneGrid = expand.grid(k = 1:90))

fit

plot(fit)

varImp(fit)

pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$is_canceled)

