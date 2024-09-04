library(tidyverse)
library(tidytext)
library(syuzhet)
library(reshape2)
library(ggplot2)

news <- read_csv("fakenews.csv")

news$type <- gsub("bs|conspiracy", "fake", news$type)
news$type <- gsub("bias|satire|hate|junksci|state", "real", news$type)

sentiment <- get_nrc_sentiment(news$text)
news <- cbind(news, sentiment)

binary_news <- news %>%
  mutate(class = ifelse(positive > negative, "real", "fake"))

TP <- sum(binary_news$type == "real" & binary_news$class == "real")
FP <- sum(binary_news$type == "fake" & binary_news$class == "real")
TN <- sum(binary_news$type == "fake" & binary_news$class == "fake")
FN <- sum(binary_news$type == "real" & binary_news$class == "fake")

confusion_matrix <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                           dimnames = list("Actual" = c("real", "fake"), "Predicted" = c("real", "fake")))
print(confusion_matrix)

news_summary <- news %>%
  group_by(type) %>%
  summarise(count = n())
print(news_summary)

news$exc <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\!+"))))
news$que <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\?+"))))

exc_summary <- news %>% 
  group_by(type) %>% 
  summarise(exclamations = sum(exc))
print(exc_summary)

que_summary <- news %>% 
  group_by(type) %>% 
  summarise(QuestionMarks = sum(que))
print(que_summary)

par(mar = c(4, 4, 4, 4)) 
boxplot(exc ~ type, data = news, ylim = c(0, 20), ylab = "Exclamations", col = c("red", "orange"))

par(mar = c(4, 4, 4, 4)) 
boxplot(que ~ type, data = news, ylim = c(0, 20), col = c("red", "orange"))

terms <- function(fake, text_column, group_column) {
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  words <- fake %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>%
    ungroup()
  
  return(words)
}

df <- terms(news, text, type)

boxplot(n ~ type, data = df, log = "y", xlab = "Type", ylab = "Number of words", col = c("green", "pink"))

neg_sd <- news %>% group_by(type) %>% summarise(neg_sd = sd(negative))
pos_sd <- news %>% group_by(type) %>% summarise(pos_sd = sd(positive))
neg_med <- news %>% group_by(type) %>% summarise(neg_med = median(negative))
pos_med <- news %>% group_by(type) %>% summarise(pos_med = median(positive))

dfr1 <- data.frame(pos_sd)
dfr2 <- data.frame(neg_sd)
dfr3 <- data.frame(pos_med)
dfr4 <- data.frame(neg_med)

t1 <- merge(dfr1, dfr2)
t2 <- t(t1)
print(t2)

t3 <- merge(dfr4, dfr3)
t4 <- t(t3)
print(t4)
