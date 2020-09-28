#Author - Gowtham Kumaresh
# Text Mining
#install.packages("readr") # you only need to do this one time on your system
#install.packages("wordcloud") # you only need to do this one time on your system
install.packages("writexl") # you only need to do this one time on your system
library(writexl)
library(readr)
library(tokenizers)
library(tm)
library(stringr)
library(wordcloud)
library(readxl)


##Setting up work directory
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups")

####################################################################
#####################  DATASET EXPLORATION    ######################
####################################################################

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
  return(clean_corp)
}

####################################################################
#####################     TASK 1 AND TASK 2   ######################
####################################################################

DataExploration <- function() {
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
BagOfWords <- function(words) {
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
      #each_word <- tokenize_words(documet_clean_corpus$content)
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
  write_xlsx(bag_of_words,"C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/BagOfWords.xlsx")
  print("/* Bag of words being created and XLSX is stored locally*/")
}

words = DataExploration()
BagOfWords(words)


####################################################################
#####################     BASIC EVALUATION    ######################
####################################################################

#Reading the bag of words
print("/* Creating train and test data (run time - aproximately 1 min) */")
my_data <- read_excel("BagOfWords.xlsx")
#Split 70% of the dataset from training and 30% for testing
n <- floor(0.70 * nrow(my_data))
## set the seed to make your partition reproducible
set.seed(1)
train_index <- sample(seq_len(nrow(my_data)), size = n)

#Train and test data creation
train <- my_data[train_index,]
test  <- my_data[-train_index,]
#Verify the number of rows
nrow(train)
nrow(test)





