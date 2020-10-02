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
    files_list = setdiff(list.files(), list.dirs(recursive = FALSE, full.names = FALSE))
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
write.csv(BoW,"C:/Users/Gowtham/OneDrive/Documents/Text Mining/BoW.csv", row.names = F)
print("/* Bag of words being created and CSV is stored locally*/")


####################################################################
#####################     BASIC EVALUATION    ######################
####################################################################

##Setting up work directory
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining")
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
knn_fit <-
  knn(x_train, x_test, cl = y_train, k = 1)
##create confusion matrix
confusionmat_knn <- table(knn_fit, y_test)
accuracy_knn <- accuracy_value(confusionmat_knn)
print(accuracy_knn)

############################################################
########## ACCURACY OF KNN is  45.8333 #####################
############################################################

############################################################

############################################################
###################   RANDOM FOREST   ######################
############################################################
set.seed(1)
library(randomForest)
library(caret)
cat("Random Forest Processing...\nLoading time is 3 minutes")
#Mtry is set to 17 because for classification (sqrt(p) where p is 
#number of variables in x)
randomforest_fit <- randomForest(x_train, y_train)
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
################## ACCURACY OF RF is  90 ###################
############################################################

############################################################

############################################################
###################   Naive Bayes's   ######################
############################################################

library(e1071) ##Implementing Naive Bayes

?naiveBayes
nv_fit <- naiveBayes(x_train, y_train)
y_pred_nb <- predict(nv_fit, x_test)
confusionmat_nb_acc <- table(y_pred_nb, y_test)
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
  clean_corp = tm_map(clean_corp, removeWords, c(stopwords("english"),"else", "ifelse", "for", "in", "shadowmask")) 
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
    files_list = setdiff(list.files(),
                         list.dirs(recursive = FALSE, full.names = FALSE))
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
  write_xlsx(bag_of_words,"C:/Users/Gowtham/OneDrive/Documents/Text Mining/PreProcessBagOfWords.xlsx")
  print("/* Bag of words being created and XLSX is stored locally*/")
}

preprocessedwords = preprocessing()
PreprocessedBagOfWords(preprocessedwords)

############################################################
############## Hyper Paramerter Tuning ####################
############################################################

##Setting up work directory
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/")
#Reading the bag of words
print("/* Creating train and test data (run time - aproximately 1 min) */")
my_data <- read_excel("PreProcessBagOfWords.xlsx")
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
x_train = train[-12790]
y_train = train$news_category
#Extract news_category column for to measure accuracy
x_test = test[-12790]
y_test = test$news_category

#To find accuracy of classification models
accuracy_value <- function(x) {
  sum(diag(x) / (sum(rowSums(x)))) * 100
}

#Feature selection using Boruta

library(Boruta)
library(randomForest)
library(caret)
str(my_data)


set.seed(1)
boruta <- Boruta(news_category ~ .,data = my_data, doTrace = 2)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)

bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

set.seed(1)
getNonRejectedFormula(boruta)
getConfirmedFormula(boruta)

###########################################################
#################   Feature Selection  ####################
###########################################################

#New dataset 
fs_data <- my_data[c("cmu", "srv", "cantaloupe", "talk", "politics", "misc", "writes", "article", "references", "people" ,"guns", "alt", "ibm",
  "comp", "gun", "hardware", "sys", "xref", "electronics", "sci",   "government", "president", "usa", "soc", "clinton", "fbi", "culture", 
  "optilink", "legal", "card", "circuit", "batf", "cramer", "men",
  "abortion", "religion", "scsi", "port", "sex", "weapons", "atf",
  "gay", "bios", "ide", "clayton", "firearms", "motherboard", "firearm",
  "burns", "ranch", "six", "liberties", "dividian", "qvfik")]

fs_data$new_category = rep(c(1:4), each = 100)
fs_data$new_category = factor(fs_data$new_category)
## set the seed to make your partition reproducible
set.seed(1)
index <- sample(nrow(fs_data), size = nrow(fs_data))

noofrow <- floor(0.70 * nrow(fs_data))

train_ind <- index[1:noofrow]
test_ind <- index[noofrow+1:120]


#Train and test data creation
train <- fs_data[train_ind,]
test  <- fs_data[test_ind,]

ncol(train)
#Extract news_category column for to train
x_train = train[-55]
y_train = train$new_category
#Extract news_category column for to measure accuracy
x_test = test[-55]
y_test = test$new_category



############### Hyperparameter Tuning ######################
install.packages("mlr")
library(mlr)

hyp_tree <- makeLearner("classif.rpart")
hyp_rf <- makeLearner("classif.randomForest")
hyp_svm <- makeLearner("classif.ksvm")
hyp_knn <- makeLearner("classif.knn")

param_tree <- makeParamSet(
  makeIntegerParam("minsplit",lower = 1, upper = 100),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2)
)

param_rf <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)

param_svm <-  makeParamSet(
  makeNumericParam("C", lower=-10, upper=10, trafo = function(x)2^x),
  makeNumericParam("sigma",lower=-10,upper=10,trafo = function(x)2^x))

param_knn = makeParamSet(makeIntegerParam("k",lower=1,upper=30))


ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 3L)
task = makeClassifTask(data=train,target="new_category")

set.seed(1)
tune_trees = tuneParams(hyp_tree,task, resampling = rdesc, par.set=param_tree, 
                       control=ctrl, measures = acc)
# minsplit=1; minbucket=5; cp=0.001 : acc.test.mean=0.9643484
set.seed(1)
tune_knn = tuneParams(hyp_knn, task, resampling = rdesc, par.set=param_knn, 
                     control=ctrl, measures = acc) 
#k=4 : acc.test.mean=0.9323191
set.seed(1)
tune_rf = tuneParams(hyp_rf, task, resampling = rdesc, par.set=param_rf, 
                    control=ctrl, measures = acc) 
# ntree=50; mtry=7; nodesize=10 : acc.test.mean=0.9822695
set.seed(1)
tune_svm = tuneParams(hyp_svm, task, resampling = rdesc, par.set=param_svm, 
                     control=ctrl, measures = acc) 
# C=47; sigma=0.00456 : acc.test.mean=0.9784946
# Optimal results from hyperparameter tuning
print(tune_trees)

print(tune_knn)

print(tune_rf)

print(tune_svm)

par(mfrow=c(2,2))
plot(tune_trees$x$minsplit,tune_trees$x$maxdepth,xlab="Min. split",ylab="Max. Depth",col="black",
     pch=16,main="Decision Trees")
plot(tune_knn$x,tune_knn$y,xlab="K value",ylab="Accuracy",col="black",pch=16,
     main="Knn")
plot(tune_rf$x$ntree, tune_rf$x$mtry, xlab="No. of trees",ylab="mtry",main="Random Forest",
     pch=16, col="black")
plot(tune_svm$x$C, tune_svm$x$sigma, xlab="Cost", ylab="Sigma", main="SVM",
     col="black",pch=16)

############################## Cross validation ####################################
library(class) #Contains KNN
library(randomForest)
library(e1071) #Contains SVM
library(rpart) #Contains Decision Tree
k = 10

folds <- cut(seq(1,nrow(fs_data)),breaks=10,labels=FALSE)
acc.knn = acc.rf = acc.svm = acc.tree = numeric(0)
#Perform 10 fold cross validation
for(i in 1:10){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- fs_data[testIndexes, ]
  trainData <- fs_data[-testIndexes, ]
  x_train = train[-55]
  y_train = train$new_category
  #Extract news_category column for to measure accuracy
  x_test = test[-55]
  y_test = test$new_category
  #KNN
  knn_cv <- knn(x_train, x_test, cl = y_train, k = tune_knn$x)
  cm_cv_knn <- table(knn_cv, y_test)
  acc.knn <- sum(diag(cm_cv_knn) / (sum(rowSums(cm_cv_knn)))) * 100
  #Random Forest
  rf_cv <-randomForest(x_train, y_train, ntree = tune_rf$x$ntree, mtry = tune_rf$x$mtry)
  y_pred_rf_cv <- predict(rf_cv,x_test)
  cm_cv_rf <- table(y_pred_rf_cv, y_test)
  acc.rf <- sum(diag(cm_cv_rf) / (sum(rowSums(cm_cv_rf)))) * 100
  #SVM
  svm_cv <- svm(x_train, y_train, cost = tune_svm$x$C, sigma = tune_svm$x$sigma)
  y_pred_svm_cv <- predict(svm_cv,x_test)
  cm_cv_svm <- table(y_pred_svm_cv, y_test)
  acc.svm <- sum(diag(cm_cv_svm) / (sum(rowSums(cm_cv_svm)))) * 100
  #Decision Tree
  dtree_cv <-  rpart(new_category~.,data = train,minsplit=tune_trees$x$minsplit,
                     cp=tune_trees$x$cp)
  y_pred_dtree_cv <- predict(dtree_cv,x_test, type = "class")
  cm_cv_dtree <- table(y_pred_dtree_cv, y_test)
  acc.tree <- sum(diag(cm_cv_dtree) / (sum(rowSums(cm_cv_dtree)))) * 100
}
mean(acc.knn)
mean(acc.rf)
mean(acc.svm)
mean(acc.tree)


plot(acc.knn,t="b",main="Cross Validated KNN Accuracy")
plot(acc.rf,t="b",main="Cross Validated RF Accuracy")
plot(acc.svm,t="b",main="Cross Validated SVM Accuracy")
plot(acc.tree,t="b",main="Cross Validated DT Accuracy")

boxplot(acc.knn, acc.rf, acc.svm, acc.tree,
        main="Overall CV prediction accuracy",
        names=c("KNN","RF","SVM","TREE"))
