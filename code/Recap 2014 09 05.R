# 2014 09 05
# --- recap project capstone

setwd("C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone")
data <- ".\\data\\final\\en_US"
fileb <- "en_US.blogs.txt"

# load badwords
urlbad <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
download.file(urlbad, ".\\badWords", method = "auto", quiet=FALSE)
badwords <- readLines(".\\badWords")

# custom filter is very slow. In testing is better to disable
# toks is the N in N-gram

loadAndTokenize <- function(file, bad=NULL, N=10000, toks=1, lang="en_US"){   
    #---------------------------------------------------------------------------
    # function that return a tokenized item
    #---------------------------------------------------------------------------
    #----------  CUSTOM UNICODE FILTERS ----------------------------------------
    # --------- manual punctuation and strange chars
    myRemPunct <- function(x) UseMethod("myRemPunct", x)
    myRemPunct.PlainTextDocument <- myRemPunct.character  <- function(x) {
        x <- gsub("[^\\PP]", "", x, perl=T)
        x <- gsub("[$~`£¨\u20AC\u00B0=+½]", "", x, perl=T)
        x <- gsub("-", "", x, perl=T)
        return(x)
    }
    
    tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
    myTolower <- function(x) stri_trans_tolower(x, lang)
    #---------------------------------------------------------------------------
    raw <- readLines(file, encoding="UTF-8", n=N)
    
    # collapse all row in one
    raw <- paste(raw, collapse=" ")
    
    #createCorpus
    myCorpus <- Corpus(VectorSource(raw), list(language = lang))
    # tolower 
    myCorpus <- tm_map(myCorpus, content_transformer(myTolower))
    # remove punctuation
    myCorpus <- tm_map(myCorpus, content_transformer(myRemPunct))
    # remove numbers
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
    
    # remove stopwords (no stopwords but bad words)
    # myStopwords <- c(stopwords('english'))
    if(!is.null(bad)){
        myCorpus <- tm_map(myCorpus, removeWords, bad)    
    }
    
    # Term document matrix
    if(toks == 1){
        tdm <- TermDocumentMatrix(myCorpus)
    }else{
        tdm <- TermDocumentMatrix(myCorpus, control = list(tokenize = tokenizer))
    }
    return(tdm)
}

tdm  <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=10000)
tdm2 <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=10000, toks=2)
tdm3 <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=10000, toks=3)
tdm4 <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=10000, toks=4)

#  -------------------- experiment with N-gram
# ------------------------------------- FUNCTIONS --------------------------------
tdm2df <- function(tdm){
    #------------------------------------------------------------------------------
    # function. Create an ordered data frame from a tdm (BEWARE, cannot be too big)
    #------------------------------------------------------------------------------
    ft <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
    dfft <- data.frame(term = names(ft), cnt = ft)
    dfft$prob <- dfft$cnt/sum(dfft$cnt)
    dfft$i <- 1:nrow(dfft)
    return(dfft)
}

df2plotdf <- function(dfft){
    #------------------------------------------------------------------------------
    # function. Create a plottable (zipf) structure from a data frame dfft style
    #------------------------------------------------------------------------------
    
    terms <- nrow(dfft)
    gr <- data.frame(i=NULL, prob=NULL) 
    for(i in 1:100){
        t <- floor((terms/100)*i)
        gr[i,"i"] <- i
        gr[i,"prob"]    <- sum(dfft[1:t,"prob"])
    }
    return(gr)
}
# ----------------------------------END  FUNCTIONS --------------------------------

#------------------------ FREQUENCY ANALYSIS

# 1 gram   --------------------------------------------------
Zipf_plot(tdm)

dfft <- tdm2df(tdm)

# prepare to plot
gr <- df2plotdf(dfft) 

library(ggplot2)
library(scales)

# graph (preliminary)
ggplot(as.data.frame(x=gr), aes(x=i, y=prob*100)) + scale_x_log10()  + geom_point() 
# Zipf!
ggplot(dfft, aes(x=i, y=cnt)) + scale_x_log10() + scale_y_log10()  + geom_point() 

# 2 gram --------------------------------------------------

dfft2 <- tdm2df(tdm2)

gr2 <-df2plotdf(dfft2)

# graph (preliminary)
ggplot(as.data.frame(x=gr2), aes(x=i, y=prob*100)) + scale_x_log10()  + geom_point() 
ggplot(dfft2, aes(x=i, y=cnt)) + scale_x_log10() + scale_y_log10()  + geom_point() 

