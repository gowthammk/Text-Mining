#Author - Gowtham Kumaresh
# Text Mining Data Exploration
#Exploration of the dataset
#Task 1 and 2
#install.packages("readr") # you only need to do this one time on your system
library(readr)
library(tokenizers)
library(tm)
library(stringr)

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

#Creating a string of all the 400 files in 4 folders
#and applying text preprocessing techniques
newstring <- ""
folder_list = list.files(pattern = "*.*")
for (i in 1:length(folder_list)) {
  setwd(file.path("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/", folder_list[i]))
  files_list = list.files(pattern = "*.*")
  for (j in files_list){
    mystring <- read_file(j)
    newstring <- paste(newstring,mystring, sep = " ")
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
#install.packages("wordcloud")
library(wordcloud)
words <- names(sorted_freq)
head(words)
wordcloud(words[1:200], sorted_freq[1:200])

#Task 2 Repeat the previous step, but now filter tokens by 
#length (min. size = 4 and max. size = 20). 
#Please indicate the total occurrences of the words
df1 = data.frame(nrows = 1)
for (i in names(sorted_freq)){
  if (str_length(i) >= 4  & str_length(i) <= 20){
    df1[i] <- sorted_freq[[i]]
  }
}
df1 = df1[-1]

