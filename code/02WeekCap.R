# week 02 capstone
#install.packages("tm")
library(tm)
library(RWeka)
library(stringi)
library(ggplot2)
library(scales)


getwd()
setwd("C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone")
#setwd("C:\\Documents and Settings\\Massimo\\Documenti\\Google Drive\\Data Science\\10_Capstone")
data <- ".\\data\\final\\en_US"
fileb <- "en_US.blogs.txt"
filen <- "en_US.news.txt"
filet <- "en_US.twitter.txt"
# --- internal var for debug
toks <- 2
lang <- "en_US"
N <- 50000
badwords <- readLines(".\\badWords")

bad <- badwords
file <- paste(data,fileb, sep="\\")
-----
loadAndTokenize <- function(file, bad=NULL, N=50000, toks=1, lang="en_US"){   
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
#    myCorpus <- tm_map(myCorpus, content_transformer(myRemPunct))
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
myCnt <- function(x) {
    length(unlist(strsplit(x, split = "[[:space:][:punct:]]+"),
                  use.names = FALSE, recursive = FALSE))
}



# 1318667

# 2751816
#------------------------------------------------------------------------------
# save the week 1 questions with comments -------------------------------------
# Question 1: The en_US_blogs.txt file is how many megabytes?
# 205

# Question 2: The en_US.twitter.txt has how many lines of text?
# 2360148
# ?readLines
bcorp <- readLines(paste(data,fileb, sep="\\"), encoding="UTF-8", n=4000)
-- 899288 lines
myCnt(bcorp[1:100000])
# 4216665 x 100K row
#N <- 4216665*8.99288
#bwords <- 37.919.962

tcorp <- readLines(paste(data,filet, sep="\\"), encoding="UTF-8", skipNul=T)
-- 2360148 lines
myCnt(tcorp[1:1000000])
    twords <-  13209492 * 2.360148 # 31.176.356

ncorp <- readLines(paste(data,filen, sep="\\"), encoding="UTF-8")
-- 77259 lines
myCnt(ncorp)
# nwords 2.751.816

# Question 3: What is the length of the longest line seen in any of the three en_US data sets?
# > 40 in bolgs
#max(sapply(bcorp, nchar))
#-- 40835 (40833 with utf-8 encoding)
#max(sapply(tcorp, nchar))
#-- 140
#max(sapply(ncorp, nchar))
#-- 5760

# love.hate <- sum(grepl("love", tcorp)) / sum(grepl("hate", tcorp))
# warning: not correct, find all the occurrence also the embedded ones (hateful, gloved)

# Question 5: The one tweet in the en_US twitter data set that matches the word "biostats" says what?
#tcorp[grep("biostats", tcorp)]
# "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

# Question 6
# How many tweets have the exact characters 
# "A computer once beat me at chess, but it was no match for me at kickboxing"
# . (I.e. the line matches those characters exactly.)
#grep("A computer once beat me at chess, but it was no match for me at kickboxing", tcorp)
# [1]  519059  835824 2283423
#----------------------------------------------------------------------
#------------------------------   WEEK 2       ------------------------
#------------------------------------------------------------------------------
# Task 1
#------------------------------------------------------------------------------

#install.packages("tm")

# ?Corpus
# ?VectorSource
# ?TermDocumentMatrix
# ?enc2utf8
# ?tm_map

# reduced blog


#head(blogmini, 100)

myCorpus <- Corpus(VectorSource(bcorp), list(lanuage = "en_US"))



# str(myCorpus)
# 
# termFreq(myCorpus)
# class(myCorpus)
inspect(myCorpus[1:10])

# tolower 
myCorpus <- tm_map(myCorpus, tolower)
 # remove punctuation
#myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
#myStopwords <- c(stopwords('english'))
# tm_map: Interface to apply transformation functions (also denoted as mappings) to corpora.
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# check
#inspect(myCorpus[1:200])

# convert non convertible bytes (but this destroy unicode?)
#myCorpus <- tm_map(myCorpus, function(x) iconv(enc2utf8(as.character(x)), sub = "byte"))
#myCorpus[1:10]$content

# convert to plaintext (see stack overflow) to avoid error in tdm building
myCorpus <- tm_map(myCorpus, PlainTextDocument)

date()
tdm <- TermDocumentMatrix(myCorpus)
date() # 8 sec for 10K row 45 sec for 50k row

Docs(tdm)

nDocs(tdm)

# inspecting terms
a <- inspect(tdm[16,1])
# findFreqTerms(tdm, lowfreq=1)
# dimnames(tdm)[[1]]
# lhtdm <- tdm[c("love", "hate"), ]

#------------------------------------------------------------------------------
# load tdm with routine
#------------------------------------------------------------------------------
# list of bad words
urlbad <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
download.file(urlbad, ".\\badWords", method = "auto", quiet=FALSE)
badwords <- readLines(".\\badWords")

system.time(tdm <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=8000))
system.time(tdm2 <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=8000, toks=2))
system.time(tdm3 <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=8000, toks=3))
tdm4 <- loadAndTokenize(paste(data,fileb, sep="\\"), badwords,N=8000, toks=4)
# check

Docs(tdm)

inspect(tdm[1:30,1])
inspect(tdm2[1:30,1])
inspect(tdm3[1:30,1])
#------------------------------------------------------------------------------
# Task 2
#------------------------------------------------------------------------------
#Esploratory analysis (poor man version, with findFreqTerms)
f <- findFreqTerms(tdm, 1000, Inf)
NN <- (1:1000)*10
NN[1001]<-Inf

NN[2]
#long tail
for(i in 1:1000){
    f[i] <- length(findFreqTerms(tdm, NN[i], NN[i+1]))
}
# manually found
findFreqTerms(tdm, 81664, 81664)

# head 
g <- NULL
for(i in 1:10) g[i] <- length(findFreqTerms(tdm, i, i))
plot(g)
# ----------- end of poor man version
# useful function 

# not possible if the tdm is too big
ft <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
ft2 <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
ft3 <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
plot(ft[1:20])
#-------------------------------------------------------------------------
#   TOKENIZATION
#-------------------------------------------------------------------------


library(RWeka)
raw <- readLines(paste(data,fileb, sep="\\"), encoding="UTF-8", n=100000)
myCorpus <- Corpus(VectorSource(raw), list(language = "en_US"))
myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))
myCorpus <- tm_map(myCorpus, content_transformer(myRemPunct))
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
summary(myCorpus)
removeNumbers
myRemPunct
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
BigramTokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))

date()
tdm <- TermDocumentMatrix(myCorpus)
tdm3 <- TermDocumentMatrix(myCorpus, control = list(tokenize = TrigramTokenizer))
date()
#tdm3 <- removeSparseTerms(tdm, 0.75)
tdm3
inspect(tdm3[1:1000,1:5])
inspect(tdm[1:100,1:5])
date()
stri_trans_tolower
loc_stri_trans_tolower <- stri_trans_tolower()
#---------------------------------------
# not very useful linee of reasoning
# useful: removeSparseTerms()
TDM.common = removeSparseTerms(tdm, 0.99)
dim(tdm)
dim(TDM.common)
inspect(TDM.common[1:10,1:10])
library(slam)
TDM.dense <- as.matrix(TDM.common)

library(reshape2)
TDM.d <- melt(TDM.dense, value.name = "count")
head(TDM.d)
getTransformations()
#----------------- need frequency table
nDocs(tdm)
nTerms(tdm)
getFilters()

#--------------------------------------

#----------  CUSTOM UNICODE FILTERS ----------------------------------------
# --------- manual punctuation and strange chars
myRemPunct <- function(x) UseMethod("myRemPunct", x)
myRemPunct.PlainTextDocument <- myRemPunct.character  <- function(x) {
    x <- gsub("[^\\PP]", "", x, perl=T)
    x <- gsub("[$~`£¨\u20AC=+½]", "", x, perl=T)
    x <- gsub("-", "", x, perl=T)
    return(x)
}

stri_trans_toupper(ss, "de_DE")
#---------------------------------------------------------------------
# ----------------   use of stringi -----------------------------
install.packages("stringi")
library(stringi)
grep("grease",raw, fixed=T) 
raw[grep("\u00BA",raw, fixed=T) ]
raw[grep("\u02DA",raw, fixed=T) ]

#-------------------------------
#----- filter playground

x <- gsub("[^\\PP]", "", x, perl=T)
gsub("[]", "", x, perl=T)
x
x <- removeNumbers(x)
myRemPunct(x)
myRemPunct.PlainTextDocument
ss <- "\u00DF"
gr <- 
x <- gsub("[^\\PP~$]", "", x, perl=T)
ss <- "\u00B0"
ss
stri_trans_tolower(ss, "de_DE")
ll <- "de_DE"
ll <- "en_US"
my_tolower <- function(x) stri_trans_tolower(x, ll)
my_tolower(ss)
stri_trans_toupper(ss, "de_DE")
#-------------------------------------------------------------------------------
#------------------------------- experiment on frequency -----------------------
# ----- *** tbd: consider corpus.p <-tm_map(corpus.p, stripWhitespace)  #removes stopwords

# raw <- readLines(paste(data,fileb, sep="\\"), encoding="UTF-8", n=100000)



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
raw <- readLines(paste(data,fileb, sep="\\"), encoding="UTF-8", 100000) # bigram cannot 100000
# TBD useful function findFreqTerms
# create a collapsed corpus (just 1 doc)
rawColl <- paste(raw, collapse=" ")

# create corpus
system.time(Coll <- Corpus(VectorSource(rawColl), list(language = lang)))

# polish corpus
Coll <- tm_map(Coll, stripWhitespace)
Coll <- tm_map(Coll, content_transformer(myTolower))
#Coll <- tm_map(Coll, content_transformer(myRemPunct)) # too slow
Coll <- tm_map(Coll, content_transformer(removePunctuation))
Coll <- tm_map(Coll, content_transformer(removeNumbers))
Coll <- tm_map(Coll, removeWords, bad)    

tdm <- TermDocumentMatrix(Coll, list(bounds=list(local=c(2,Inf))))
system.time(tdm <- TermDocumentMatrix(Coll))
nDocs(tdm)
# 1
nTerms(tdm)
# 125672

Zipf_plot(tdm)
# Heaps_plot(tdm2) # does not work

dfft <- tdm2df(tdm)
nrow(dfft)
# 43788
# how many to do 50%, 90%
sum(dfft[dfft$i<=390,"prob"]) # 50%
sum(dfft[dfft$i<=18100,"prob"]) # 90%

# prepare to plot
gr <- df2plotdf(dfft) 

library(ggplot2)
library(scales)

# graph (preliminary)
ggplot(as.data.frame(x=gr), aes(x=i, y=prob*100)) + scale_x_log10()  + geom_point() 
# Zipf!
ggplot(dfft, aes(x=i, y=cnt)) + scale_x_log10() + scale_y_log10() + geom_point() + xlab("N. of unigrams") + ylab("Frequency")+ ggtitle("Zipf plot of unigrams - sample (8000 rows)")

#------ 2 gram -----------------------------------------------------------------

# raw <- readLines(paste(data,fileb, sep="\\"), encoding="UTF-8", 8000) # bigram hat serious problems
# 
# # create a collapsed corpus (just 1 doc)
# rawColl <- paste(raw, collapse=" ")
# 
# # create corpus
# system.time(Coll <- Corpus(VectorSource(rawColl), list(language = lang)))
# # options(mc.cores=1) -- ineffective
# 
# # I do not polish the corpus
# 
# BigramTokenizer  <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# tdm <- TermDocumentMatrix(Coll, list(bounds=list(local=c(2,Inf))))
# 
# # no appreciable gain in prestation skipping hapax
# #system.time(tdm2 <- TermDocumentMatrix(Coll, control=list(tokenize=BigramTokenizer, bounds=list(local=c(2,Inf)))))
# 
# system.time(tdm2 <- TermDocumentMatrix(Coll, control=list(tokenize=BigramTokenizer)))

nDocs(tdm2)
nTerms(tdm2)
Terms(tdm2)

dfft2 <- tdm2df(tdm2)

gr2 <-df2plotdf(dfft2)

# graph (preliminary)
options(scipen=10)
ggplot(as.data.frame(x=gr2), aes(x=i, y=prob*100)) + scale_x_log10()  + geom_point() 
ggplot(dfft2, aes(x=i, y=cnt)) + scale_x_log10() + scale_y_log10()  + geom_point() + xlab("N. of bigrams") + ylab("Frequency")+ ggtitle("Zipf plot of bigrams - sample (8000 rows)")

# --------------------------
?write.table
write.table(raw, file="raw.txt", quote=F, row.names=F, col.names=F)
