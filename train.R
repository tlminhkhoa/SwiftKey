library(tidyverse)
library(tidytext)
library(tm)
library(dplyr)

trainNgram <- function(){
        
        if(!file.exists("Data")){dir.create("Data")}
        fileName <- "Coursera-SwiftKey.zip"
        path <- getwd()
        if(!file.exists(paste(path,"/Data","/",fileName,sep = ""))){
                url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
                download.file(url,destfile = paste(path,"/Data","/",fileName,sep = "")) 
                unzip(paste(path,"/Data","/",fileName,sep = ""),exdir = paste(path,"/Data", sep =""))
        }
        
        
        
        filepath <- list.files(path = paste(getwd(),"/Data/final/en_US",sep = "") , pattern="*.txt", full.names=TRUE, recursive=FALSE)
        
        lines = c()
        
        lines<- lapply(filepath, function(x) {
                
                con <- file(x, "r")
                line <- readLines(con)
                set.seed(123)
                close(con)
                line <- sample(line,length(line)*0.1)
                text_df <- tibble(id = 1:length(line), text = line)
                text_df <- mutate(text_df, text = text_df$text)
                return(text_df)
        })
        
        lines <- Preprocess(lines)
        
        one_gram_token <- tokenize_ngrams(lines_corpus$content,n=1)
        one_gram_token <- data.frame(id = 1:length(unlist(one_gram_token)),token = unlist(one_gram_token)) %>% count(token,sort =TRUE)
        
        two_gram_token <- tokenize_ngrams(lines_corpus$content,n=2)
        two_gram_token <- data.frame(id = 1:length(unlist(two_gram_token)),token = unlist(two_gram_token)) %>% count(token,sort =TRUE)
        
        three_gram_token <- tokenize_ngrams(lines_corpus$content,n=3)
        three_gram_token <- data.frame(id = 1:length(unlist(three_gram_token)),token = unlist(three_gram_token)) %>% count(token,sort =TRUE)
        
        write.csv(one_gram_token,"one_gram_token.csv")
        write.csv(two_gram_token,"two_gram_token.csv")
        write.csv(three_gram_token,"three_gram_token.csv")
        
        return(1)
}
