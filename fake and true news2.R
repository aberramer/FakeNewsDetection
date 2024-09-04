# Load libraries
library(tidyverse)
library(ggplot2)
library(caret)

# Load data
# Replace 'your_data.csv' with your actual dataset file name
data <- Fake 2.csv('your_data.csv')
data <- True 2.csv('your_data.csv')

# Explore data
head(data)
summary(data)

# Data preprocessing
# (Perform any data cleaning, preprocessing steps here)

# Split data into training and testing sets
set.seed(123) # For reproducibility
train_indices <- createDataPartition(data$target_variable, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Model training
# (Train your machine learning model here)

# Model evaluation
# (Evaluate your model's performance here)
head(df_fake)
head(df_true, 5)

# Assuming df_fake and df_true are existing data frames
# Create a new column named "class" and assign value 0 to df_fake
df_fake$class <- 0

# Create a new column named "class" and assign value 1 to df_true
df_true$class <- 1

# Check the dimensions of the data frames
# In R, we use 'dim()' to get the dimensions of a data frame
dim(df_fake)
dim(df_true)

# For df_fake
df_fake_manual_testing <- tail(df_fake, 10)
df_fake <- df_fake[-(23480:23471), , drop = FALSE]

# For df_true
df_true_manual_testing <- tail(df_true, 10)
df_true <- df_true[-(21416:21407), , drop = FALSE]

# For df_fake
dim(df_fake)

# For df_true
dim(df_true)

# For df_fake_manual_testing
df_fake_manual_testing$class <- 0

# For df_true_manual_testing
df_true_manual_testing$class <- 1

head(df_fake_manual_testing, 10)

library(dplyr)

head(df_true_manual_testing, 10)

# Concatenate df_fake_manual_testing and df_true_manual_testing row-wise
df_manual_testing <- rbind(df_fake_manual_testing, df_true_manual_testing)

# Write the resulting data frame to a CSV file
write.csv(df_manual_testing, "manual_testing.csv", row.names = FALSE)

# Merge df_fake and df_true row-wise
df_merge <- rbind(df_fake, df_true)

# View the first 10 rows of the merged data frame
head(df_merge, 10)

colnames(df_merge)

# Remove columns "title", "subject", and "date" from df_merge
df <- df_merge[, !(names(df_merge) %in% c("title", "subject", "date"))]

# Check for missing values
colSums(is.na(df))

# Shuffle the rows of the df data frame
df <- df[sample(nrow(df)), ]

# View the first few rows of the shuffled data frame
head(df)

# Remove row names
row.names(df) <- NULL

# Drop the "index" column
df <- df[, !(names(df) %in% c("index"))]

# Get column names
colnames(df)

# View the first few rows of the data frame
head(df)

wordopt <- function(text) {
  # Convert text to lowercase
  text <- tolower(text)
  
  # Remove square brackets and their contents
  text <- gsub("\\[.*?\\]", "", text)
  
  # Replace non-word characters with spaces
  text <- gsub("\\W", " ", text)
  
  # Remove URLs
  text <- gsub("https?://\\S+|www\\.\\S+", "", text)
  
  # Remove HTML tags
  text <- gsub("<.*?>+", "", text)
  
  # Remove punctuation
  text <- gsub("[[:punct:]]", "", text)
  
  # Remove newline characters
  text <- gsub("\n", "", text)
  
  # Remove alphanumeric characters containing digits
  text <- gsub("\\w*\\d\\w*", "", text)
  
  return(text)
}
# Assuming df is your data frame and wordopt is the preprocessing function

# Apply the wordopt function to each element of the "text" column
df$text <- sapply(df$text, wordopt)

# Assuming you have already loaded your data and performed text preprocessing

# Splitting the data into training and testing sets
set.seed(123)  # for reproducibility
split_index <- sample(1:nrow(df), size = round(0.75 * nrow(df)), replace = FALSE)
x_train <- df$text[split_index]
y_train <- df$class[split_index]
x_test <- df$text[-split_index]
y_test <- df$class[-split_index]

# Performing TF-IDF vectorization
library(tm)
vectorization <- function(x_train, x_test) {
  corpus <- Corpus(VectorSource(x_train))
  dtm_train <- DocumentTermMatrix(corpus)
  dtm_test <- DocumentTermMatrix(Corpus(VectorSource(x_test)), control = list(dictionary = Terms(dtm_train)))
  tfidf_transformer <- function(dtm) {
    weighting <- weightTfIdf(dtm)
    return(as.data.frame(as.matrix(weighting)))
  }
  return(list(tfidf_transformer(dtm_train), tfidf_transformer(dtm_test)))
}
tfidf_vectors <- vectorization(x_train, x_test)
xv_train <- tfidf_vectors[[1]]
xv_test <- tfidf_vectors[[2]]

# Training logistic regression model
library(glmnet)
LR <- glmnet(xv_train, y_train, family = "binomial")

# Making predictions
pred_lr <- as.numeric(predict(LR, newx = as.matrix(xv_test), type = "response") > 0.5)

# Evaluating model
accuracy <- mean(pred_lr == y_test)
print(paste("Accuracy:", accuracy))

# Generating classification report
library(caret)
classification_report <- function(y_true, y_pred) {
  confusion_matrix <- confusionMatrix(factor(y_pred), factor(y_true))
  return(confusion_matrix)
}
report <- classification_report(y_test, pred_lr)
print(report)

# Training decision tree classifier
library(rpart)
DT <- rpart(y_train ~ ., data = as.data.frame(xv_train), method = "class")

# Making predictions with decision tree classifier
pred_dt <- predict(DT, as.data.frame(xv_test), type = "class")

# Evaluating decision tree classifier
accuracy_dt <- mean(pred_dt == y_test)
print(paste("Decision Tree Classifier Accuracy:", accuracy_dt))

# Generating classification report for decision tree classifier
report_dt <- confusionMatrix(pred_dt, y_test)
print(report_dt)

# Training gradient boosting classifier
library(gbm)
GBC <- gbm(xv_train, as.factor(y_train), distribution = "bernoulli", n.trees = 100, interaction.depth = 4, shrinkage = 0.1)

# Making predictions with gradient boosting classifier
pred_gbc <- predict.gbm(GBC, newdata = as.matrix(xv_test), type = "response", n.trees = 100)

# Converting predicted probabilities to class labels
pred_gbc <- ifelse(pred_gbc > 0.5, 1, 0)

# Evaluating gradient boosting classifier
accuracy_gbc <- mean(pred_gbc == y_test)
print(paste("Gradient Boosting Classifier Accuracy:", accuracy_gbc))

# Generating classification report for gradient boosting classifier
report_gbc <- confusionMatrix(pred_gbc, y_test)
print(report_gbc)

# Training random forest classifier
library(randomForest)
RFC <- randomForest(xv_train, as.factor(y_train), ntree = 100, importance = TRUE)

# Making predictions with random forest classifier
pred_rfc <- predict(RFC, as.data.frame(xv_test))

# Evaluating random forest classifier
accuracy_rfc <- mean(pred_rfc == y_test)
print(paste("Random Forest Classifier Accuracy:", accuracy_rfc))

# Generating classification report for random forest classifier
report_rfc <- confusionMatrix(pred_rfc, y_test)
print(report_rfc)

# Function to output label
output_label <- function(n) {
  if (n == 0) {
    return("Fake News")
  } else if (n == 1) {
    return("Not A Fake News")
  }
}

# Function for manual testing
manual_testing <- function(news) {
  testing_news <- list(text = news)
  new_def_test <- as.data.frame(testing_news)
  new_def_test$text <- sapply(new_def_test$text, wordopt)
  new_x_test <- new_def_test$text
  new_xv_test <- predict(vectorization, newdata = new_x_test)
  pred_LR <- predict(LR, newdata = new_xv_test)
  pred_DT <- predict(DT, newdata = new_xv_test, type = "class")
  pred_GBC <- ifelse(predict(GBC, newdata = as.matrix(new_xv_test), n.trees = 100, type = "response") > 0.5, 1, 0)
  pred_RFC <- predict(RFC, newdata = as.data.frame(new_xv_test))
  cat("\n\nLR Prediction:", output_label(pred_LR), 
      "\nDT Prediction:", output_label(pred_DT),
      "\nGBC Prediction:", output_label(pred_GBC),
      "\nRFC Prediction:", output_label(pred_RFC))
}

manual_testing <- function() {
  cat("Enter news text:")
  news <- readline()
  testing_news <- list(text = news)
  new_def_test <- as.data.frame(testing_news)
  new_def_test$text <- sapply(new_def_test$text, wordopt)
  new_x_test <- new_def_test$text
  new_xv_test <- predict(vectorization, newdata = new_x_test)
  pred_LR <- predict(LR, newdata = new_xv_test)
  pred_DT <- predict(DT, newdata = new_xv_test, type = "class")
  pred_GBC <- ifelse(predict(GBC, newdata = as.matrix(new_xv_test), n.trees = 100, type = "response") > 0.5, 1, 0)
  pred_RFC <- predict(RFC, newdata = as.data.frame(new_xv_test))
  cat("\n\nLR Prediction:", output_label(pred_LR), 
      "\nDT Prediction:", output_label(pred_DT),
      "\nGBC Prediction:", output_label(pred_GBC),
      "\nRFC Prediction:", output_label(pred_RFC))
}

manual_testing()



