#Author - Gowtham Kumaresh
# Text Mining
#install.packages("readr") # you only need to do this one time on your system
#install.packages("wordcloud") # you only need to do this one time on your system
#install.packages("writexl") # you only need to do this one time on your system
library(writexl)
library(readr)
library(tokenizers)
library(tm)
library(stringr)
library(wordcloud)
library(readxl)
library(ISLR)
library(caret)


##Setting up work directory
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")

####################################################################
#####################  DATASET EXPLORATION    ######################
####################################################################

#Cleaning of text using tm library
corpus_explore <- function(clean_corp){
  clean_corp = tm_map(clean_corp, stripWhitespace)
  return(clean_corp)
}

DataExploration <- function() {
  #Creating a string of all the 400 files in 4 folders
  #and applying text preprocessing techniques
  print("/** Takes approximately 6 mins for word cloud creation **/")
  newstring <- ""
  folder_list = list.dirs("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")
  for (i in (folder_list)) {
    setwd(file.path(i))
    files_list = list.files(pattern = "*.*")
    for (j in files_list) {
      mystring <- read_file(j)
      newstring <- paste(newstring, mystring, sep = " ")
      documents = gsub("\\n", " ", newstring)
      clean_corpus <- Corpus(VectorSource(documents))
      clean_corp <- corpus_explore(clean_corpus)
      dtm <- DocumentTermMatrix(clean_corp)
      dtm2 <- as.matrix(dtm)
    }
  }
  freq <- colSums(dtm2)
  sorted_freq <- sort(freq, decreasing = T)
  
  #Task 1 to create top 200 words and visualize using word cloud
  words <- names(sorted_freq)
  #Set seed to replicate the word cloud on different execution
  set.seed(1)
  wordcloud(
    words[1:200],
    sorted_freq[1:200],
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.15,
    colors = brewer.pal(8, "Dark2")
  )
  
  #Task 2 Repeat the previous step, but now filter tokens by
  #length (min. size = 4 and max. size = 20).
  #Please indicate the total occurrences of the words
  filtered_words = data.frame(nrows = 1)
  for (i in names(sorted_freq)) {
    if (str_length(i) >= 4  & str_length(i) <= 20) {
      filtered_words[i] <- sorted_freq[[i]]
    }
  }
  filtered_words = filtered_words[-1]
  #Set seed to replicate the word cloud on different execution
  set.seed(1)
  wordcloud(
    names(filtered_words[1, 1:200]),
    filtered_words[1, 1:200],
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.05,
    colors = brewer.pal(8, "Dark2")
  )
  print("/* Word cloud created */")
  return(sorted_freq)
}

####################################################################
#####################        BAG OF WORDS     ######################
####################################################################

##Function to create bag of words
##Function to create bag of words
BagOfWords <- function(words) {
  print("/** Takes approximately 4 mins for bag of words**/")
  bag_of_words = data.frame(matrix(ncol = length(words), nrow = 400))
  colnames(bag_of_words) = names(words)
  # Last column corresponds to folders, There are 4 folders and each folder has 100 files
  #Thus, the last column corresponds to folders (ie) 1, 2, 3, 4
  bag_of_words$news_category = rep(1:4, each = 100)
  #Creating bag of words
  count = 0
  folder_list = list.dirs("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")
  for (i in (folder_list)) {
    setwd(i)
    files_list = setdiff(list.files(),
                         list.dirs(recursive = FALSE, full.names = FALSE))
    for (j in files_list) {
      mystring <- read_file(j)
      documents = gsub("\\n", " ", mystring)
      documet_clean_corpus <- Corpus(VectorSource(mystring))
      documet_clean_corpus <- corpus_explore(documet_clean_corpus)
      dtm_bagofwords <- DocumentTermMatrix(documet_clean_corpus)
      dtm2_bagofwords <- as.matrix(dtm_bagofwords)
      freq <- colSums(dtm2_bagofwords)
      for (f in names(freq)) {
        if (f %in% colnames(bag_of_words)) {
          bag_of_words[[f]][count] = freq[[f]]
        }
      }
      count = count + 1
    }
  }
  bag_of_words[is.na(bag_of_words)] = 0
  return (bag_of_words)
}


words = DataExploration()
BoW = BagOfWords(words)
write.csv(BoW,"C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/BoW.csv", row.names = F)
print("/* Bag of words being created and CSV is stored locally*/")


####################################################################
#####################     BASIC EVALUATION    ######################
####################################################################

##Setting up work directory
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")
#Reading the bag of words
print("/* Creating train and test data (run time - aproximately 1 min) */")
my_data <- read.csv("BoW.csv", header = T)
my_data$news_category = factor(my_data$news_category)
## set the seed to make your partition reproducible
set.seed(1)
index <- sample(nrow(my_data), size = nrow(my_data))
#Split 70% of the dataset from training and 30% for testing
n <- floor(0.70 * nrow(my_data))

train_index <- index[1:n]
test_index <- index[n+1:120]


#Train and test data creation
train <- my_data[train_index,]
test  <- my_data[test_index,]
#Verify the number of rows
nrow(train)
ncol(train)
nrow(test)
ncol(test)

## The main objective is to classify the data
##As we know we have created an entire last column for maintaing the folders
##  The folders will be classified (ie) the last column is the response variable
## Store the response variable

#Extract news_category column for to train
x_train = train[-24790]
y_train = train$news_category
#Extract news_category column for to measure accuracy
x_test = test[-24790]
y_test = test$news_category

#To find accuracy of classification models
accuracy_value <- function(x) {
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

############################################################
###################   KNN ALGORITHM   ######################
############################################################

library(class)##Implementing KNN algorithm
set.seed(1)

###K value also can be checked by elbow method
###manual implementation of elbow method is as follows
k.optimal = 1
i = 1
for (i in  1:30) {
  knn_fit <- knn(x_train, x_test, cl = y_train, k = i)
  ##create confusion matrix
  confusionmat_knn <- table(knn_fit, y_test)
  
  accuracy_value <- function(x) {
    sum(diag(x) / (sum(rowSums(x)))) * 100
  }
  accuracy <- accuracy_value(confusionmat_knn)
  k.optimal[i] = accuracy
}
k.optimal

#Accuracy plot
plot(k.optimal,
     type = "b",
     xlab = "K- Value",
     ylab = "Accuracy level")
##Even the graph indicates that the optimal value will be around 16 or 17.
##Choosing K = 17 will be the best choice for this dataset
set.seed(1)
knn_fit <-
  knn(x_train, x_test, cl = y_train, k = 25)
##create confusion matrix
confusionmat_knn <- table(knn_fit, y_test)
accuracy_knn <- accuracy_value(confusionmat_knn)
print(accuracy_knn)

############################################################
########## ACCURACY OF KNN is  50 ########################
############################################################

############################################################

############################################################
###################   RANDOM FOREST   ######################
############################################################
set.seed(1)
library(randomForest)
cat("Random Forest Processing...\nLoading time is 3 minutes")
#Mtry is set to 17 because for classification (sqrt(p) where p is 
#number of variables in x)
randomforest_fit <- randomForest(x_train, y_train, mtry = 17)
y_pred_rf <- predict(randomforest_fit,x_test)
confusionmat_rf_acc <- table(y_pred_rf, y_test)
accuracy_value(confusionmat_rf_acc)
confusionmat_rf <- confusionMatrix(y_pred_rf, y_test)

#Performance Metrics
#Sensitivity
confusionmat_rf[["byClass"]][ , "Sensitivity"]
#Precision 
confusionmat_rf[["byClass"]][ , "Pos Pred Value"]

############################################################
############## ACCURACY OF RF is  66.666667 ################
############################################################

############################################################

############################################################
###################   Naive Bayes's   ######################
############################################################

library(e1071) ##Implementing Naive Bayes

?naiveBayes
nv_fit <- naiveBayes(x_train, y_train)
Y_pred_nb <- predict(nv_fit, x_test)
confusionmat_nb_acc <- table(Y_pred_nb, y_test)
accuracy_value <- function(x) {
  sum(diag(x) / (sum(rowSums(x)))) * 100
}
accuracy_value(confusionmat_nb_acc)
confusionmat_nb <- confusionMatrix(Y_pred_nb, y_test)

#Performance Metrics
#Sensitivity
confusionmat_nb[["byClass"]][ , "Sensitivity"]
#Precision 
confusionmat_nb[["byClass"]][ , "Pos Pred Value"]

############################################################
############## ACCURACY OF RF is  27.5  ####################
############################################################

############################################################
################  Robust Evaluation #######################
############################################################

############################################################
###################### Preprocessing #######################
############################################################

#Setting up Working directory
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/")
#Cleaning of text using tm library
cleaned_corpus <- function(clean_corp){
  clean_corp = tm_map(clean_corp, stripWhitespace)
  clean_corp = tm_map(clean_corp, content_transformer(tolower))
  replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
  clean_corp = tm_map(clean_corp, replacePunctuation)
  clean_corp = tm_map(clean_corp, removeNumbers)
  clean_corp = tm_map(clean_corp, removeWords, stopwords("english"))
  clean_corp = tm_map(clean_corp, removeWords, c(stopwords("english"),"else", "ifelse", "for", "in")) 
  return(clean_corp)
}

preprocessing <- function() {
  #Creating a string of all the 400 files in 4 folders
  #and applying text preprocessing techniques
  print("/** Takes approximately 5 mins for word cloud creation **/")
  newstring <- ""
  folder_list = list.dirs("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")
  for (i in (folder_list)) {
    setwd(file.path(i))
    files_list = list.files(pattern = "*.*")
    for (j in files_list) {
      mystring <- read_file(j)
      newstring <- paste(newstring, mystring, sep = " ")
      documents = gsub("\\n", " ", newstring)
      clean_corpus <- Corpus(VectorSource(documents))
      clean_corp <- cleaned_corpus(clean_corpus)
      dtm <- DocumentTermMatrix(clean_corp)
      dtm2 <- as.matrix(dtm)
    }
  }
  freq <- colSums(dtm2)
  sorted_freq <- sort(freq, decreasing = T)
  
  #Task 1 to create top 200 words and visualize using word cloud
  words <- names(sorted_freq)
  #Set seed to replicate the word cloud on different execution
  set.seed(1)
  wordcloud(
    words[1:200],
    sorted_freq[1:200],
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.15,
    colors = brewer.pal(8, "Dark2")
  )
  
  #Task 2 Repeat the previous step, but now filter tokens by
  #length (min. size = 4 and max. size = 20).
  #Please indicate the total occurrences of the words
  filtered_words = data.frame(nrows = 1)
  for (i in names(sorted_freq)) {
    if (str_length(i) >= 4  & str_length(i) <= 20) {
      filtered_words[i] <- sorted_freq[[i]]
    }
  }
  filtered_words = filtered_words[-1]
  #Set seed to replicate the word cloud on different execution
  set.seed(1)
  wordcloud(
    names(filtered_words[1, 1:200]),
    filtered_words[1, 1:200],
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    rot.per = 0.05,
    colors = brewer.pal(8, "Dark2")
  )
  print("/* Word cloud created */")
  return(sorted_freq)
}

####################################################################
#####################        BAG OF WORDS     ######################
####################################################################

##Function to create bag of words
PreprocessedBagOfWords <- function(words) {
  print("/** Takes approximately 4 mins for bag of words**/")
  bag_of_words = data.frame(matrix(ncol = length(words), nrow = 400))
  colnames(bag_of_words) = names(words)
  # Last column corresponds to folders, There are 4 folders and each folder has 100 files
  #Thus, the last column corresponds to folders (ie) 1, 2, 3, 4
  bag_of_words$news_category = rep(1:4, each = 100)
  #Creating bag of words
  count = 1
  folder_list = list.dirs("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")
  for (i in (folder_list)) {
    setwd(i)
    files_list = setdiff(list.files(),
                         list.dirs(recursive = FALSE, full.names = FALSE))
    for (j in files_list) {
      mystring <- read_file(j)
      documents = gsub("\\n", " ", mystring)
      documet_clean_corpus <- Corpus(VectorSource(mystring))
      documet_clean_corpus <- cleaned_corpus(documet_clean_corpus)
      dtm_bagofwords <- DocumentTermMatrix(documet_clean_corpus)
      dtm2_bagofwords <- as.matrix(dtm_bagofwords)
      freq <- colSums(dtm2_bagofwords)
      for (f in names(freq)) {
        if (f %in% colnames(bag_of_words)) {
          bag_of_words[[f]][count] = freq[[f]]
        }
      }
      count = count + 1
    }
  }
  bag_of_words[is.na(bag_of_words)] = 0
  write_xlsx(bag_of_words,"C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/PreProcessBagOfWords.xlsx")
  print("/* Bag of words being created and XLSX is stored locally*/")
}

preprocessedwords = preprocessing()
PreprocessedBagOfWords(preprocessedwords)

############################################################
############## Hyper Paramerter Tuning ####################
############################################################
