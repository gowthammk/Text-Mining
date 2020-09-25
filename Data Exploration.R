# Text Mining Data Exploration
library(tokenizers)
library(tm)
library(stringr)
#install.packages("readr") # you only need to do this one time on your system
library(readr)
df = data.frame(matrix(nrow = 400))
colnames(df)
count = 1
setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/comp.sys.ibm.pc.hardware")
files_list = list.files(pattern = "*.*")
replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
for (i in files_list){
    mystring <- read_file(i)
    mystring <- DocumentTermMatrix(mystring)
    clean_corp <- clean_corpus(mystring)
}

clean_corpus <-  function(corpus){
  documents = gsub("\\n", " ", mystring)
  documents <- Corpus(VectorSource(documents))
  documents = tm_map(documents, stripWhitespace)
  documents = tm_map(documents, content_transformer(tolower))
  documents = tm_map(documents, replacePunctuation)
  documents = tm_map(documents, removeNumbers)
  documents = tm_map(documents, removeWords, stopwords("english"))
  documents = str_squish(documents$content)
  return(documents)
  
}

coffee_dtm <- DocumentTermMatrix(documents)

