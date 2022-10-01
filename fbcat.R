# fbcat.R

library(caret)
library(tm)
library(dplyr)

# Training data.
data <- event[,"event"] 
data <- data %>% unlist()
corpus <- VCorpus(VectorSource(data))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE,
                                       stemming = TRUE, removeNumbers = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train.comb <- cbind(train, event$cat)
colnames(train.comb)[ncol(train.comb)] <- 'y'
train.comb <- as.data.frame(train.comb)
train.comb$y <- as.factor(event$cat)

# Train.
fit <- train(y ~ ., data = train.comb, method = 'bayesglm')

# Check accuracy on training.
predict(fit, newdata = train)

# Test data.
data2 <- c("BIRTHDAY BASH FOR JULY,AUG& SEPT ANDBEST EMPLOYEE OF THE MONTH AWARD FOR JULY & SEPTEMBER")
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), 
                                                 removePunctuation = TRUE, stopwords = TRUE, 
                                                 stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
predict(fit, newdata = test)
