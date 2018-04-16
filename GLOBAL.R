if (!file.exists("Coursera-SwiftKey.zip")) {
  download.file(url = "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile = "C://Users//u182335//Documents//DataScience//CAPSTONE//Week 2//Coursera-SwiftKey.zip")
  unzip("C://Users//u182335//Documents//DataScience//CAPSTONE//Week 2//Coursera-SwiftKey.zip")
}

blogs <- readLines("final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

library(styler)
library(stringi)
library(devtools)
library(ngram)

blogs.size <- file.info("final/en_US/en_US.blogs.txt")$size / 1024 ^ 2
news.size <- file.info("final/en_US/en_US.news.txt")$size / 1024 ^ 2
twitter.size <- file.info("final/en_US/en_US.twitter.txt")$size / 1024 ^ 2

blogs.words <- stri_count_words(blogs)
news.words <- stri_count_words(news)
twitter.words <- stri_count_words(twitter)

data.frame(source = c("blogs", "news", "twitter"),
           file.size.MB = c(blogs.size, news.size, twitter.size),
           num.lines = c(length(blogs), length(news), length(twitter)),
           num.words = c(sum(blogs.words), sum(news.words), sum(twitter.words)),
           mean.num.words = c(mean(blogs.words), mean(news.words), mean(twitter.words)))

set.seed(679)
data.sample <- c(sample(blogs, length(blogs) * 0.01),
                 sample(news, length(news) * 0.01),
                 sample(twitter, length(twitter) * 0.01))

library(shiny)
library(dplyr)
library(ggplot2)

freq.table <- data.sample

####### CONSTANTs ######### 
MAX_NGRAM <- 4
####### functions #########
clean_input <- function(txt_input) {
  sens <- unlist(stringr::str_split(txt_input, "[.!?]"))
  # choose the last sentence
  sens <- sens[length(sens)]
  
  # preprocess the text input the same way as with training text
  sens <- stringr::str_trim(sens)
  sens <- tolower(sens)
  sens <- gsub("'", "'", sens)
  sens <- gsub("[^[:alnum:][:space:]\']", " ", sens)
  sens <- iconv(sens, "latin1", "ASCII", sub = "_0_")
  sens <- gsub("\\w*[0-9]\\w*"," ", sens)
  sens <- gsub(" www(.+) ", " ", sens)
  sens <- gsub("\\s+[b-hj-z]\\s+", " ", sens)
  sens <- gsub("\\s+", " ", sens)
  sens <- stringr::str_trim(sens)
  
  # if "", return the tag indicating the end of the sentence
  # otherwise, return the last incomplete sentence
  if(sens == "") 
    return("0EOS0")
  else 
    return(sens)
}

# predict function, return all predicted words
predict_sbf <- function(freq.table, typed_context) {
  require(dplyr)
  
  capitalize_prediction <- FALSE
  typed_context <- clean_input(typed_context)
  
  typed_context <- get_last_n_words(typed_context, n = MAX_NGRAM-1)
  # treat empty string and space as end of sentence tag 0EOS0
  if (typed_context == "" || typed_context == " ") {
    typed_context <- "0EOS0"
  }
  nGramToStartSearch <- nWords(typed_context) + 1
  # the predicted words, based on contexts ending with end of sentence 
  # tag 0EOS0, will be capitalized.
  if (get_last_word(typed_context) == "0EOS0")
    capitalize_prediction <- TRUE
  
  # find all possible search context terms, including "" empty string
  searchTerms <- sapply((nGramToStartSearch-1):1, function(i) {
    get_last_n_words(typed_context, i)
  })
  searchTerms <- c(searchTerms, "") # seach 1-gram
  
  finalResult <- freq.table %>% 
    filter(as.character(context) %in% searchTerms)
  
  finalResult <-
    finalResult %>% 
    select(predicted, freq, ngram, everything()) %>% 
    mutate(predicted = as.character(predicted)) %>%
    mutate(freq = as.numeric(as.character(freq))) %>%
    mutate(ngram = as.integer(as.character(ngram))) %>%
    mutate(freq = freq * ((0.40) ^ (nGramToStartSearch - ngram)) ) %>%
    arrange(desc(ngram), desc(freq) ) %>%
    distinct(predicted)
  
  # if the prediction is 0EOS0, then change it to "." 
  finalResult$predicted[finalResult$predicted == "0EOS0"] <- "."
  finalResult$predicted[finalResult$predicted == "i"] <- "I"
  
  # if the context ends with 0EOS0, then captalize all predictions
  if(capitalize_prediction) {
    finalResult$predicted <- stringi::stri_trans_totitle(finalResult$predicted)
  }
  
  finalResult
}

random_from <- function(words) {
  words[sample(length(words), 1)]
}


get_last_word <- function(s, sep = " ") {
  #stringr::word(s, -1)
  get_last_n_words(s, n = 1L, sep = sep)
}

get_last_n_words <- function(s, n, sep = " ") {
  #stringr::word(s, (-1)*n, -1)
  stopifnot(n >= 1)
  words <- unlist(strsplit(s, split = sep))
  len <- length(words)
  if (len <= n)
    return(paste(words, collapse = sep))
  paste(words[-(1:(len - n))], collapse = sep)
}

get_first_n_words <- function(s, n, sep = " ") {
  #stringr::word(s, 1, n)
  stopifnot(n >= 1)
  words <- unlist(strsplit(s, split = sep))
  if(length(words) < n) 
    return(paste(words, collapse = sep))
  paste(words[1:n], collapse = sep)
}

nWords <- function(s, sep = " ") {
  #qdap::wc(s)
  #stringr::str_count(s, "\\S+")
  #stringr::str_count(s,"[[:alpha:]]+") 
  s <- as.character(s)
  if (nchar(s) == 0)
    return(0)
  length(unlist( strsplit(s, split = sep) ))
}

