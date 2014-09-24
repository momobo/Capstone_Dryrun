library(tm)
library(RWeka)
library(stringi)
library(ggplot2)
library(scales)
#library(filehash)
library(slam)
library(plyr)
lang="en_US"

test_corpus =c("<s> This is a list containing the tallest buildings in San Francisco <pp> </s>",  # <pp> = :
               "<s> The Transamerica Pyramid is the tallest building in San Francisco <p> </s>",  # <p> = .
               "<s> 555 California Street is the 2nd-tallest building in San Francisco <p> </s>") # <p> = .
# 
# test_corpus
c <- Corpus(VectorSource(test_corpus), list(language = lang))
inspect(c)

SAVEHX <- NOHAPAX
SAVED <- D
SAVEBBG <- BBG

NOHAPAX <- F
BBG <- "<s>"
D1 <- 21/(21+2*5)
D2 <- 25/(25+2*2)
D3 <- 25/(25+2*3)
D1 <- D2 <- D3 <- 0.8

mono  <- loadNGram(c, 1)
bigr  <- loadNGram(c, 2)
trigr <- loadNGram(c, 3)
# 
bigr <- addIndexN(bigr ,2)
trigr <- addIndexN(trigr ,3)

#
mono  <- removeStartOfPhrase(mono, 1)
bigr  <- removeStartOfPhrase(bigr, 2)
trigr <- removeStartOfPhrase(trigr,3)

nmono  <- addKNUnigram(mono,  bigr, trigr, D1)
nbigr  <- addKNBigram(nmono, bigr, trigr, D2, D1)
ntrigr <- addKNTrigram(nbigr, trigr, D3)

D <- SAVED
BBG <- SAVEBBG
NOHAPAX <- SAVEHX

nmono
nbigr
ntrigr

tables()

nrow(mono)
nrow(mono[mono$cnt==2,])
nrow(bigr)
nrow(bigr[bigr$cnt==2])
nrow(trigr)
nrow(trigr[trigr$cnt==2])
trigr
sum(mono$cnt)

D <- n1 / (n1 + 2n2)

rm(c)
cw("francisco")
cw("<s>")
word <- "francisco"

# -------------------
PKN1 <- function(word){
    if(word=="<s>") 0
    cw(word) / (sum(mono$cnt) - sum(mono[mono$term=="<s>","cnt"]))
}

PKN1("francisco")
PKN1("")
cw2("san francisco")
# D is n1 / (n1 + 2n2)
D <- 0.75
w1 <-"san"
w2 <-"francisco"
cw2("the tallest")

PKN2 <- <- function(w1, w2){
    bb <- paste(w1,w2)
    bigrams <- nrow(bigr)
    max(cw2(bb)-D, 0)
}