# Text Mining Data Exploration
library(tokenizers)
library(tm)
library(stringr)
#install.packages("readr") # you only need to do this one time on your system
library(readr)

setwd("C:/Users/Gowtham/OneDrive/Documents/Text Mining/Newsgroups/comp.sys.ibm.pc.hardware")
files_list = list.files(pattern = "*.*")
replacePunctuation <- content_transformer(function(x) {return (gsub("[[:punct:]]"," ", x))})
for (i in files_list){
    mystring <- read_file(i)
    documents = gsub("\\n", " ", mystring)
    documents <- Corpus(VectorSource(documents))
    documents = tm_map(documents, content_transformer(tolower))
    documents = tm_map(documents, replacePunctuation)
    documents = tm_map(documents, removeNumbers)
    documents = tm_map(documents, stripWhitespace)
    documents = tm_map(documents, removeWords, stopwords("english"))
    
}

dc = strsplit(documents$content, " ")
data.frame(table(unlist(strsplit(tolower(dc), " "))))


