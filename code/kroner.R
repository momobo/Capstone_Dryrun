#-----------------------------------------------------------------------------------------------
library(tm)
library(RWeka)
library(stringi)
library(ggplot2)
library(scales)
#library(filehash)
library(slam)
library(plyr)
lang<-"en_US"
library(RWeka)

#-----------------------------------------------------------------------------------------------
loadNGram <- function(corpus, N){
  ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
  tdm <-   TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=N),wordLengths=c(1,Inf)))
  df <- tdm2df(tdm)
  return(df)
}

tdm2df <- function(tdm){
  #------------------------------------------------------------------------------
  # function. Create an ordered data frame from a tdm (BEWARE, cannot be too big)
  #------------------------------------------------------------------------------
  ft <- rowapply_simple_triplet_matrix(tdm, sum)
  dfft <- data.frame(term = Terms(tdm), cnt = ft)
  
  # order and i at the end, not now
  #  dfft <- dfft[order(-dfft$cnt),] 
  #  dfft$i <- 1:nrow(dfft)
  
  return(dfft)
}
#-----------------------------------------------------------------------------------------------
test_corpus =c("<s> This is a list containing the tallest buildings in San Francisco <pp> </s>",  # <pp> = :
               "<s> The Transamerica Pyramid is the tallest building in San Francisco <p> </s>",  # <p> = .
               "<s> 555 California Street is the 2nd-tallest building in San Francisco <p> </s>") # <p> = .
# 
test_corpus
c <- Corpus(VectorSource(test_corpus), list(language = lang))

inspect(c)
mono  <- loadNGram(c, 1)
bigr  <- loadNGram(c, 2)
trigr  <- loadNGram(c, 3)
mono
bigr
trigr
nrow(mono)
sum(mono$cnt)
# -------------------
cw <- function(word){
  if(word=="<s>") 0
  else sum(mono[mono$term == word ,"cnt"])
}
cw2 <- function(biword){
  sum(bigr[bigr$term == biword ,"cnt"])
}
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
#-----------------------------------------------------------------
sessionInfo()



if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)

System.getProperty("sun.arch.data.model") 
java -d64 -version
