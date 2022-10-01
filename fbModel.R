# fbModel.R

# init ----
library(tidyverse)
library(tidytext)
library(caret)

load("stopwordMalay.RData")
event <- read_csv("event.csv")

# tokenise to unigrams & bigrams ----
data <- event %>% 
  mutate("id" = row_number())

data_counts <- map_df(1:2,
                      ~ unnest_tokens(data, word, event, 
                                      token = "ngrams", n = .x)) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(stopword_malay, by = "word") %>%
  filter(str_detect(word,"[:alpha:]")) %>% 
  count(id, word, sort = TRUE)

# words with freq > 10
wgp <- data_counts %>%
  group_by(word) %>%
  summarise(n = n()) %>% 
  filter(n >= 1) %>%
  select(word)

# calculate the tf_idf and cast it to a document term matrix ----
data_dtm <- data_counts %>%
  right_join(wgp, by = "word") %>%
  bind_tf_idf(word, id, n) %>%
  cast_dtm(id, word, tf_idf)

# create meta data.frame
meta <- tibble(id = as.numeric(dimnames(data_dtm)[[1]])) %>%
  left_join(data[!duplicated(data$event), ], by = "id") %>% 
  filter(!is.na(event))

# separate the data into a training and test set
set.seed(1234)
trainIndex <- createDataPartition(meta$cat, p = 0.8, list = FALSE, times = 1)

data_train <- data_dtm[trainIndex, ] %>% as.matrix() %>% as.data.frame()
data_test <- data_dtm[-trainIndex, ] %>% as.matrix() %>% as.data.frame()

response_train <- meta$cat[trainIndex]

# Model random forest
# will not use a resampling method
trctrl <- trainControl(method = "none")
rf_mod <- train(x = data_train, 
                y = as.factor(response_train), 
                method = "ranger",
                trControl = trctrl,
                tuneGrid = data.frame(mtry = floor(sqrt(dim(data_train)[2])),
                                      splitrule = "gini",
                                      min.node.size = 1))

rf_pred <- predict(rf_mod, newdata = data_test)

# calculate the confusion matrix
# rf_cm <- confusionMatrix(rf_pred, meta[-trainIndex, ]$cat)
# rf_cm

# test data ----
dummy <- c("mesyuarat 3 khidmat","majlis ramah mesra pvtm","mmr hari ulang tahun kpa",
               "taklimat hrmis","taklimat kajian tahunan") %>% 
  data_frame()
names(dummy)[1] <- "event"

tdm_cat <- function(data){
  
  data <- data %>% 
    mutate("id" = row_number())
  
  data_counts <- map_df(1:2,
                        ~ unnest_tokens(data, word, event, 
                                        token = "ngrams", n = .x)) %>%
    anti_join(stop_words, by = "word") %>%
    anti_join(stopword_malay, by = "word") %>%
    filter(str_detect(word,"[:alpha:]")) %>% 
    count(id, word, sort = TRUE)
  
  # words with freq > 10
  wgp <- data_counts %>%
    group_by(word) %>%
    summarise(n = n()) %>% 
    filter(n >= 1) %>%
    select(word)
  
  # calculate the tf_idf and cast it to a document term matrix ----
  data_dtm <- data_counts %>%
    right_join(wgp, by = "word") %>%
    bind_tf_idf(word, id, n) %>%
    cast_dtm(id, word, tf_idf)
  
  return(data_dtm)
}


cat_data <- tdm_cat(dummy)[,] %>% as.matrix() %>% as.data.frame()

rf_pred <- predict(rf_mod, newdata = cat_data)







