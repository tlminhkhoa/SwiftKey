library(stringr)
library(dplyr)
library(tm)
library(SnowballC)



Preprocess <- function(bigPre){
        lines_corpus <- Corpus(VectorSource(bigPre))
        lines_corpus <- tm_map(lines_corpus,tolower)
        lines_corpus <- tm_map(lines_corpus, removePunctuation)
        removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
        lines_corpus <- tm_map(lines_corpus, content_transformer(removeURL))
        removeNumPunct <- function(x) gsub('[^a-zA-Z|[:blank:]]', "", x)
        lines_corpus <- tm_map(lines_corpus, content_transformer(removeNumPunct))
        lines_corpus <- tm_map(lines_corpus, removeWords,stopwords("english"))
        lines_corpus <- tm_map(lines_corpus, stripWhitespace)
        
        lines_corpus <- tm_map(lines_corpus, stemDocument)
        profanity <- read.csv("profanityWords.txt")$V1
        lines_corpus <- tm_map(lines_corpus, removeWords, profanity)
        rm(profanity)
        
        return(lines_corpus$content)
}

getObsTrigs <- function(bigPre, trigrams) {
        trigs.winA <- data.frame(tokens=vector(mode = 'character', length = 0),
                                 freq=vector(mode = 'integer', length = 0))
        
        trigram_indices <- grep(paste("^",bigPre,sep =""), trigrams$token)
        if(length(trigram_indices) > 0) {
                trigs.winA <- trigrams[trigram_indices, ]
        }
        
        return(trigs.winA)
}

getObsTriProbs <- function(obsTrigs, bigrs, bigPre, triDisc=0.5) {
        if(nrow(obsTrigs) < 1) return(NULL)
        obsCount <- subset(bigrs, token==bigPre)$n
        obsTrigProbs <- mutate(obsTrigs$token, prob=((n - triDisc) / obsCount))
        # colnames(obsTrigProbs) <- c("token", "prob")
        
        return(obsTrigProbs)
}

getUnobsTrigTails <- function(obsTrigs, unigs) {
        # obs_trig_tails <- str_split_fixed(obsTrigs, "_", 3)[, 3]
        obs_trig_tails <- str_split_fixed(obsTrigs, " ", 3)[, 3]
        unobs_trig_tails <- unigs[!(unigs$token %in% obs_trig_tails), ]$token
        return(unobs_trig_tails)
}

getAlphaBigram <- function(unigram, bigrams, bigDisc=0.5) {
        # get all bigrams that start with unigram
        # regex <- sprintf("%s%s%s", "^", unigram$token[1], "_")
        # bigsThatStartWithUnig <- bigrams[grep(regex, bigrams$token),]
        bigsThatStartWithUnig <- bigrams[grep(paste("^",unigram$token,sep =""), bigrams$token),]
        if(nrow(bigsThatStartWithUnig) < 1) return(0)
        alphaBi <- 1 - (sum(bigsThatStartWithUnig$n - bigDisc) / unigram$n)
        
        return(alphaBi)
}

getBoBigrams <- function(bigPre, unobsTrigTails) {
        w_i_minus1 <- str_split(bigPre, " ")[[1]][2]
        boBigrams <- paste(w_i_minus1, unobsTrigTails, sep = " ")
        return(boBigrams)
}

getObsBoBigrams <- function(bigPre, unobsTrigTails, bigrs) {
        boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
        obs_bo_bigrams <- bigrs[bigrs$token %in% boBigrams, ]
        return(obs_bo_bigrams)
}

getUnobsBoBigrams <- function(bigPre, unobsTrigTails, obsBoBigram) {
        boBigrams <- getBoBigrams(bigPre, unobsTrigTails)
        unobs_bigs <- boBigrams[!(boBigrams %in% obsBoBigram$token)]
        return(unobs_bigs)
}

getObsBigProbs <- function(obsBoBigrams, unigs, bigDisc=0.5) {
        first_words <- str_split_fixed(obsBoBigrams$token, " ", 2)[, 1]
        first_word_freqs <- unigs[unigs$token %in% first_words, ]
        obsBigProbs <- (obsBoBigrams$n - bigDisc) / first_word_freqs$n
        obsBigProbs <- data.frame(token=obsBoBigrams$token, prob=obsBigProbs)
        
        return(obsBigProbs)
}

getQboUnobsBigrams <- function(unobsBoBigrams, unigs, alphaBig) {
        # get the unobserved bigram tails
        qboUnobsBigs <- str_split_fixed(unobsBoBigrams, " ", 2)[, 2]
        w_in_Aw_iminus1 <- unigs[!(unigs$token %in% qboUnobsBigs), ]
        # convert to data.frame with counts
        qboUnobsBigs <- unigs[unigs$token %in% qboUnobsBigs, ]
        denom <- sum(qboUnobsBigs$n)
        # converts counts to probabilities
        qboUnobsBigs <- data.frame(token=unobsBoBigrams,
                                   prob=(alphaBig * qboUnobsBigs$n / denom))
        
        return(qboUnobsBigs)
}

getAlphaTrigram <- function(obsTrigs, bigram, triDisc=0.5) {
        if(nrow(obsTrigs) < 1) return(1)
        alphaTri <- 1 - sum((obsTrigs$n - triDisc) / bigram$n[1])
        
        return(alphaTri)
}

getUnobsTriProbs <- function(bigPre, qboObsBigrams,
                             qboUnobsBigrams, alphaTrig) {
        qboBigrams <- rbind(qboObsBigrams, qboUnobsBigrams)
        qboBigrams <- qboBigrams[order(-qboBigrams$prob), ]
        sumQboBigs <- sum(qboBigrams$prob)
        first_bigPre_word <- str_split(bigPre, " ")[[1]][1]
        unobsTrigNgrams <- paste(first_bigPre_word, qboBigrams$token, sep=" ")
        unobsTrigProbs <- alphaTrig * qboBigrams$prob / sumQboBigs
        unobsTrigDf <- data.frame(token=unobsTrigNgrams, prob=unobsTrigProbs)
        
        return(unobsTrigDf)
}