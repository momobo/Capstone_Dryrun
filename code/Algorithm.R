#------------------------------------------------------------------------------------------------
# Algorithm
# Note:
#   Use intermediate files.
# 
# Train and write ARPA
#   1 divide train, validation, test.
#   (possibly in a single pass, polish corpus)
#     On Train parse and get vocabulary (eliminate letter, )
#     substitute out of vocabulary with <UNK>, end of phrase with <\s> eventually 
#   load frequency for each word, bigram, trigram, tetragram
#   From frequency calculate probability (smoothed katz backoff)
#   Write an ARPA file
# 
# Prevision next word from stream (4 word)
#   Load arpa file
#   for each word in stream
#     calculate max probability and emit more probabile next word
# 
# Calculate perplexity
#   Use text stream and previde with prevision routine
#-----------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------
library(tm)
library(RWeka)
library(stringi)
library(ggplot2)
library(scales)
#library(filehash)
library(slam)
#-----------------------------------------------------------------------------------------------

# getwd()

#############    initialize variables ########################
setwd("C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone")
#setwd("C:\\Documents and Settings\\Massimo\\Documenti\\Google Drive\\Data Science\\10_Capstone")

datadir <- ".\\data\\final\\en_US"
fileb <- "en_US.blogs.txt"
# list of bad words
if(F){ # modify to reload
  urlbad <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  download.file(urlbad, ".\\badWords", method = "auto", quiet=FALSE)
}
badwords <- readLines(".\\badWords")
file <- paste(datadir, fileb, sep="\\")
trainfile <- paste(datadir, "train.txt", sep="\\")
BBG <- "__s__"
EEN <- "__es__"
APO <- "__AP__"
#-----------------------------------------------------------------

#############    train, test validation   ########################
# sequential cut (statistical cut would be better)

partitionData <- function(datadir, fileb, probcut=c(0.6, 0.8, 1) ){
  file <- paste(datadir, fileb, sep="\\")
  dat <- readLines(file, encoding="UTF-8")
  len <- length(dat)
  
  cutt <- ceiling(probcut*len)
  
  train <- dat[1:cutt[1]]
  valid <- dat[(cutt[1]+1):(cutt[2])]
  test  <- dat[(cutt[2]+1):(cutt[3])]

  # do this to preserve encoding
  trainfile <- file(paste(datadir, "train.txt", sep="\\"), encoding="utf8")
  validfile <- file(paste(datadir, "valid.txt", sep="\\"), encoding="utf8")
  testfile  <- file(paste(datadir, "test.txt", sep="\\"), encoding="utf8")
  
  write(train, file = trainfile) # verify encoding
  write(train, file = validfile) # verify encoding
  write(train, file = testfile ) # verify encoding
  
  close(trainfile)
  close(validfile)
  close(testfile)
}

#-----------------------------------------------------------------
partitionData(datadir, fileb)
limit <- 5000

#############    create and prefilter corpus   ###################


brokeInPhrases <- function(vector, regex="[.!?]", beg=BBG, end=EEN){
# Broke the input in phrases
    BBGEEN <- paste0(BBG,EEN)
    EENBBG <- paste0(EEN,BBG)
    # introduce separator
    vector <- gsub(regex, EENBBG, vector)
    # remove empty phrases
    vector <- gsub(BBGEEN, "", vector, fixed=T)
    # split vector
    return(unlist(strsplit(vector, EENBBG)))
}
# v <- head(corp)
# brokeInPhrases(v)
createCorpus <- function(docs, limit=5000, name="traincorpus.save", lang="en_US", use="LOAD"){
#     mapLen <- 4000
     if(use=="LOAD"){
        corp <- readLines(docs, encoding="UTF-8", n=limit)
     }else{
        corp <- docs
     }
     
     # special function
     #--------------------------------------------------------------------------
     substApostrophe <- function(x) UseMethod("substApostrophe", x)
     substApostrophe <- substApostrophe  <- function(x) {
         x <- stri_replace_all_regex(x, "['`´\u2018\u2019\u2027\u201B]", "__AP__")
         return(x)
     }
     #--------------------------------------------------------------------------
     
#     
#     # calculate number of chunk
#     l <- length(corp)
#     num <- l %/% mapLen +1
#     cc <- cut(1:l, num)
#     
#     chunk <- split(corp, cc)
#     print("TBD Divide in chunk")
#     
#     for(i in  1:length(chunk)){
#         cat(sprintf("%02d_%s\n", i, name))
#         # ok here TBD
#     }
    
    # here prefilter:
    #    broke phrase with stopgap, 
    #    maybe subst start and stop with <s> and </s> or maybe later
    
    x <- brokeInPhrases(corp)
    x <- paste(BBG, x, EEN)

    myCorpus <- Corpus(VectorSource(x), list(language = lang))
    # here apply filtering (pay attention at the <)
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
    myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace))

    # treatment of apostrophe
    myCorpus <- tm_map(myCorpus, content_transformer(substApostrophe) )
    
    # here cut in smaller sentences, using stop-sentences.
    return(myCorpus)    
}

#-----------------------------------------------------------------

Cor <- createCorpus(trainfile)
save(Cor, file= paste(datadir, name, sep="\\"))
rm(Cor)

# load saved corpus
load(file=paste(datadir, "traincorpus.save", sep="\\"))

# inspect(myCorpus[1:100])


#############    tokenize  ###############
# NB: it is possible to parallelize the tokenization. Just divide the corpus in slices
# then put togheter the resultant data frame. Then group by over the ngram (with ddply)
# TBD parametrize.  
tdm2df <- function(tdm){
  #------------------------------------------------------------------------------
  # function. Create an ordered data frame from a tdm (BEWARE, cannot be too big)
  #------------------------------------------------------------------------------
  ft <- rowapply_simple_triplet_matrix(tdm, sum)
  dfft <- data.frame(term = Terms(tdm), cnt = ft)
  
  dfft <- dfft[order(-dfft$cnt),] 

  dfft$i <- 1:nrow(dfft)
  return(dfft)
}
load1Gram <- function(corpus, name="tddf"){
  tdm <- TermDocumentMatrix(corpus)
  tddf <- tdm2df(tdm)
  nam1 <- paste0(name, ".save")
  save(tddf, file= paste(datadir, nam1, sep="\\"))
}

load2Gram <- function(corpus, name="tddf"){
  ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
  
  tdm2 <-  TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=2)))
  tddf2 <- tdm2df(tdm2)
  
  nam2 <- paste0(name, "2.save")

  save(tddf2, file= paste(datadir, nam2, sep="\\"))
}
load3Gram <- function(corpus, name="tddf"){
  ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
  
  tdm3 <-  TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=3)))
  tddf3 <- tdm2df(tdm3)
  
  nam3 <- paste0(name, "3.save")
  
  save(tddf3, file= paste(datadir, nam3, sep="\\"))
}
load4Gram <- function(corpus, name="tddf"){
  ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
  tdm4 <-  TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=4)))
  tddf4 <- tdm2df(tdm4)
  nam4 <- paste0(name, "4.save")
  save(tddf4, file= paste(datadir, nam4, sep="\\"))
}
#-----------------------------------------------------------------
# create td data frame
system.time(load1Gram(myCorpus))
system.time(load2Gram(myCorpus))
system.time(load3Gram(myCorpus))
system.time(load4Gram(myCorpus))

load(file=paste(datadir, "tddf.save",  sep="\\"))
load(file=paste(datadir, "tddf2.save", sep="\\"))
load(file=paste(datadir, "tddf3.save", sep="\\"))
load(file=paste(datadir, "tddf4.save", sep="\\"))

#head(tddf, 100)
#############    Calculate log prob with discounting       ########################
# ---- cstar
# TBD: replace with Kneser-Ney

getc <- function(i, df, cntZero){
    # poor man algorithm.
    # c0 = v**N, c1-c4 simple good turing, then subtract 0.75
    if(i == 1){
# assign to the hapax the 0 count
        cstar <- cntZero
    }else if(i <= 6){
        Ni  <- sum(df$cnt ==i)
        Ni1 <- sum(df$cnt == (i+1))
        cstar<-(df[i,"cnt"] +1)*(Ni1/Ni)
    }else{
        cstar <- i - 0.75
    }
    return(cstar)
}
addcstar <-function(df, bigN){
    # bigN denominator for N0 calc. Types for unigram, power of V for multigram
    hapax <- sum(df$cnt == 1) * 1.0
    df$cstar <- sapply(df[,"cnt"], function(x) getc(x,df, hapax/bigN) )
    return(df)
}

addLogProb <- function(df){
    base <- sum(df$cstar)
    df$logprob = log(df$cstar/base)
    return(df)
}

#-----------------------------------------------------------------
# -- total probability of unseen = Hapax / type
Types <- sum(tddf$cnt)
V <- nrow(tddf)

# 1-gram
system.time(ntddf <- addcstar(tddf, Types))
ntddf <- addLogProb(ntddf)

# 2,3,4-gram
system.time(ntddf2 <- addcstar(tddf2, V**2))
system.time(ntddf3 <- addcstar(tddf3, V**3))
system.time(ntddf4 <- addcstar(tddf4, V**4))

ntddf2 <- addLogProb(ntddf2)
ntddf3 <- addLogProb(ntddf3)
ntddf4 <- addLogProb(ntddf4)

save(ntddf,  file=paste(datadir, "ntddf.save",  sep="\\"))
save(ntddf2, file=paste(datadir, "ntddf2.save", sep="\\"))
save(ntddf3, file=paste(datadir, "ntddf3.save", sep="\\"))
save(ntddf4, file=paste(datadir, "ntddf4.save", sep="\\"))



#############    set prediction from a prhase     ########################
test = "Baby. relax. it`s not time to make a cange. at the same"
test = "want to break"
rm(rc)
teststring <- function(test){
    ctest <- createCorpus(test, use="NOLOAD")
    ltest <- as.list(ctest)
    # last phrase (no prediction after a ".")
    last <- ltest[[length(ltest)]]
    # convert to char
    charlast <- as.character(last)
    # split in words
    a <- strsplit(charlast, " " )[[1]]
    numw <- length(a)
    rc <- "no match"
    if(numw >= 4){
        # string long enough
        seed4 <- a[(numw-3):(numw-1)]
        x4 <- paste(seed4, collapse=" ")
        regex4 <- sprintf("^%s", x4)
        print(regex4)
        best4 <- ntddf4[grep(regex4, ntddf4$term), ]
#        print(best4$term)
        
        seed3 <- a[(numw-2):(numw-1)]
        x3 <- paste(seed3, collapse=" ")
        regex3 <- sprintf("^%s", x3)
        print(regex3)
        best3 <- ntddf3[grep(regex3, ntddf3$term), ]
#        print(best3$term)
        
        seed2 <- a[(numw-1)]
        x2 <- paste(seed2, collapse=" ")
        regex2 <- sprintf("^%s", x2)
        print(regex2)
        best2 <- ntddf2[grep(regex2, ntddf4$term), ]
#        print(best2$term)
        
        if(!is.na(best4[1, "term"])){
            rc <- paste("Prediction: ", as.character(best4[1, "term"]), " with prob ", as.character(exp(best4[1, "logprob"])))
        }else if(!is.na(best3[1, "term"])){
            rc <- paste("Prediction: ", as.character(best3[1, "term"]))
        }else if(!is.na(best2[1, "term"])){
            rc <- paste("Prediction: ", as.character(best2[1, "term"]))
        }else{
            "no match"
        }
    }else{
        rc <- "Not yet implemented for string shorter than 4"
    }
    return(rc)
}
head(ntddf4, 100)
teststring("when it comes")
teststring("i want")


#













#-----------------------------------------------------------------
#############    bla bla     ########################
#-----------------------------------------------------------------


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------

head(tddf)
tddf[grep("don", tddf$term),]

head(ntddf4,100)
grep("don", myCorpus)

# debug
z <- tm_filter(myCorpus, FUN=function(x) any(grep("__s__ I don", content(x))))

inspect(z)
a <-z[c(127, 123, 117, 114)]

tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tt <-  TermDocumentMatrix(z, control = list(tokenize = tokenizer))
x <- Docs(tt[7,])
inspect(z)

# load4Gram(z) # break it
class(tt)
ft <- rowapply_simple_triplet_matrix(tt, sum)


t <- Terms(tt)
inspect(tt[7,])
df <- tdm2df(tt)
head(df)


ft <- rowapply_simple_triplet_matrix(tt, sum)
dfft <- data.frame(term = t, cnt = ft)
head(dfft)

dfft <- dfft[order(-dfft$cnt),] 
head(dfft)
?order

dfft$i <- 1:nrow(dfft)

#----------------------------------------------------------------
# replace apostrophe that are so important in english
head(x)
cc <- x
# filter
cc <- x[grep("__s__ I don",x)]
y <- stri_replace_all_regex(cc, "['\u2018\u2019\u2027\u201B]", APO)
y
y[-grep("__AP__",y)] 
U+2018 U+2019 U+2027

head(y)


head(t,100)

q <- inspect(tt[,1])
a <-dimnames(inspect(tt[,1]))$Term
a <- dimnames(q)$Terms
a[grep("don",a)]
rm(a)
head(q,100)
str(q)
r <- as.data.frame(q)
head(r)
str(r,100)
q[,1]
tt$dimnames[[1]] # break it

str(ft)
head(ft)
ft <- sort(ft, decreasing=T)

dfft <- data.frame(term = names(ft), cnt = ft)
dfft$i <- 1:nrow(dfft)
return(dfft)

?simple_triplet_matrix


tddf4 <- tdm2df(tdm4)
head(tddf4)
nam4 <- paste0(name, "4.save")
save(tddf4, file= paste(datadir, nam4, sep="\\"))

head(tddf4,100)





