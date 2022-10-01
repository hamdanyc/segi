# fbmod.R

library(caret)

# make training set ----
training <- createDataPartition(y = event$cat, p = 0.7, list = FALSE)
trainData <- event[training, ]
dim(trainData)

# test sample
test <- event[-training, ]

# train control ----
trControl <- trainControl(method = "cv", number = 4)

# build model ----
modelFit.rf <- train(trainData$cat ~ ., method = "rf", trControl = trControl, 
                     event)

summary(modelFit.rf)

