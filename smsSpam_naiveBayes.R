
sms <- read.csv("E:/ser_downloads_manish/mlwithr/sms_spam.csv", stringsAsFactors = FALSE)
str(sms)
sms$type <- factor(sms$type)
str(sms)
table(sms$type)

#install.packages("tm")
library("tm")
sms_corpus <- VCorpus(VectorSource(sms$text))
#print(sms_corpus)
#inspect(sms_corpus[1:3])
lapply(sms_corpus[1:2],as.character)
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
lapply(sms_corpus_clean[1:2],as.character)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
#install.packages("SnowballC")
library("SnowballC")
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
lapply(sms_corpus_clean[1:2],as.character)


#document erms
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)


#train and test data
sms_dtm_train <- sms_dtm[1:4181,]
sms_dtm_test <- sms_dtm[4182:5574,]
sms_train_labels <- sms$type[1:4181]
sms_test_labels <- sms$type[4182:5574]
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))
#install.packages("wordcloud")
library("wordcloud")
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)


#spam and ham visualization
spam <- subset(sms, type == "spam")
ham <- subset(sms, type == "ham")
par(mfrow=c(2,1))
wordcloud(spam$text, max.words = 40, scale = c(3,0.5), random.order = FALSE)
wordcloud(ham$text, max.words = 40, scale = c(3,0.5), random.order = FALSE)

#indicator features for frequent words
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]

#function to convert numeric data to categorical
convert_counts <- function(x)
{
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

#training
#install.packages("e1071")
library("e1071")
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#predict
sms_test_pred <- predict(sms_classifier, sms_test)

#evaluation
library("gmodels")
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = F, prop.t = F, dnn = c('predicted','actual'))
#97.92%

#Trying for imrovement with laplace
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = F, prop.t = F, dnn = c('predicted','actual'))
#97.48%