predictNextWord <- function(words){
        
        one_gram_token <- read.csv("one_gram_token.csv")[2:4]
        two_gram_token <- read.csv("two_gram_token.csv")[2:4]
        three_gram_token <- read.csv("three_gram_token.csv")[2:4]
        
        
        sentence <- Preprocess(words)
        words <- unlist(str_split(sentence, " "))
        if (length(words) < 2){
                return(0)
        }
        
        
        
        bigPre <- tail(words,2)
        bigPre <- paste(bigPre[1],bigPre[2],sep = " ")
        gamma2 <- 0.5  # bigram discount
        gamma3 <- 0.5  # trigram discount
        
        obs_trigs <- getObsTrigs(bigPre,three_gram_token)
        qbo_obs_trigrams <- getObsTriProbs(obs_trigs, two_gram_token, bigPre, gamma3)
        unobs_trig_tails <- getUnobsTrigTails(obs_trigs$token, one_gram_token)
        
        
        unig <- str_split(bigPre, " ")[[1]][2]
        unig <- one_gram_token[one_gram_token$token == unig,]
        alpha_big <- getAlphaBigram(unig, two_gram_token, gamma2)
        
        
        bo_bigrams <- getBoBigrams(bigPre, unobs_trig_tails)
        obs_bo_bigrams <- getObsBoBigrams(bigPre, unobs_trig_tails,two_gram_token)
        unobs_bo_bigrams <- getUnobsBoBigrams(bigPre, unobs_trig_tails, obs_bo_bigrams)
        
        qbo_obs_bigrams <- getObsBigProbs(obs_bo_bigrams, one_gram_token, gamma2)
        qbo_unobs_bigrams <- getQboUnobsBigrams(unobs_bo_bigrams, one_gram_token, alpha_big)
        
        qbo_bigrams <- rbind(qbo_obs_bigrams, qbo_unobs_bigrams)
        
        bigram <- two_gram_token[two_gram_token$token %in% bigPre, ]
        alpha_trig <- getAlphaTrigram(obs_trigs, two_gram_token, gamma3)
        
        
        
        qbo_unobs_trigrams <- getUnobsTriProbs(bigPre, qbo_obs_bigrams,
                                               qbo_unobs_bigrams, alpha_trig)
        
        
        qbo_trigrams <- rbind(qbo_obs_trigrams, qbo_unobs_trigrams)
        qbo_trigrams <- qbo_trigrams[order(-qbo_trigrams$prob), ]
        
        return(qbo_trigrams[1,])
        
}