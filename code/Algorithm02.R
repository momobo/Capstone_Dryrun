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
library(plyr)
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
APO <- "__ap__"
CFILES <- "stucknames.save"
limit <- 10000
mapLen <- 2000
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


#############    create and prefilter corpus   ###################


brokeInPhrases <- function(vector, regex="[.!?]", beg=BBG, end=EEN){
    #--------------------------------------------------------------------------
    # FUN: Broke the input in phrases
    #--------------------------------------------------------------------------
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
# docs <- trainfile
createCorpus <- function(docs, limit=10000, name="traincorpus.save", lang="en_US", use="LOAD"){
    #--------------------------------------------------------------------------
    # FUN: create corpus from docs or vector. Use file division.
    #--------------------------------------------------------------------------

    corp <- readLines(docs, encoding="UTF-8", n=limit)
    
    # calculate number of chunk
    l <- length(corp)
    num <- ceiling(l/mapLen) 

    cc <- cut(1:l, num)
    
    chunks <- split(corp, cc)
  
    corpfiles <- NULL
    for(i in  1:length(chunks)){
        # beware, all the chunks are loaded with the same name
        chunk_name <- sprintf("%02d_%s", i, name)
        corpfiles[i] <- chunk_name
        cat("doing", chunk_name, "\n")
        chunk_corpus <- vecToCorpus(chunks[[i]], lang)
        save(chunk_corpus, file=paste(datadir, chunk_name, sep="\\"))
    }
    # save the list (more practical than read the dir)
    save(corpfiles, file=paste(datadir, CFILES, sep="\\"))
}

vecToCorpus <- function(vec, lang){
    #--------------------------------------------------------------------------
    # FUN: from character vector to corpus
    #--------------------------------------------------------------------------

    # special function for apostrophe
    #--------------------------------------------------------------------------
    substApostrophe <- function(x) UseMethod("substApostrophe", x)
    substApostrophe <- substApostrophe  <- function(x) {
        x <- stri_replace_all_regex(x, "['`´\u2018\u2019\u2027\u201B]", "__AP__")
        return(x)
    }
    #--------------------------------------------------------------------------
    x <- brokeInPhrases(vec)
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

createCorpus(trainfile, limit=limit)

# load list of corpus files
load(file=paste(datadir, CFILES, sep="\\"))
# corpfiles
# load saved corpus
load(file=paste(datadir, "01_traincorpus.save", sep="\\"))
rm(chunk_corpus)
# inspect(chunk_corpus[1:100])


#############    tokenize  ###############
# NB: it is possible to parallelize the tokenization. Just divide the corpus in slices
# then put togheter the resultant data frame. Then group by over the ngram (with ddply)
# TBD parametrize.  
loadNGram <- function(corpus, N){
    ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
    if(N==1){
        tdm <-  TermDocumentMatrix(corpus)
    }else{
        tdm <-   TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=N)))
    }
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
pasteNGram <- function(corpfiles, N){
    # should be faster with data table
    # should be much faster filtering hapax
    ldf <- list(NULL)
    for(i in 1:length(corpfiles)){
        cat(corpfiles[i], "gram:",N,"\n")
        load(file=paste(datadir, corpfiles[i], sep="\\"))
        df <- loadNGram(chunk_corpus, N)
        ldf[[i]] <- df
    }
    tddf <- do.call(rbind, ldf)
    gr_tddf <- ddply(tddf, .(term), function(df) sum(df$cnt))
    cat("putting all togheter \n")
    gr_tddf <- arrange(gr_tddf, desc(V1))
    names(gr_tddf) <- c("term", "cnt")
    return(gr_tddf)
}
#-----------------------------------------------------------------
# create td data frame

system.time(tddf  <- pasteNGram(corpfiles, 1))
system.time(tddf2 <- pasteNGram(corpfiles, 2))
system.time(tddf3 <- pasteNGram(corpfiles, 3))
# system.time(tddf4 <- pasteNGram(corpfiles, 4))

save(tddf,  file= paste(datadir, "tddf.save",  sep="\\"))
save(tddf2, file= paste(datadir, "tddf2.save", sep="\\"))
save(tddf3, file= paste(datadir, "tddf3.save", sep="\\"))
# save(tddf4, file= paste(datadir, "tddf4.save", sep="\\"))


load(file=paste(datadir, "tddf.save",  sep="\\"))
load(file=paste(datadir, "tddf2.save", sep="\\"))
load(file=paste(datadir, "tddf3.save", sep="\\"))
load(file=paste(datadir, "tddf4.save", sep="\\"))

#head(tddf, 100)
#head(tddf4, 20)
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


rm(ntddf, ntddf2, ntddf3, ntddf4)


#############    set start and end fields               ########################
addIndexN <- function(df, N){
    # first term
    term <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), 1)
    if(N!=2){
        for(i in 2:(N-1)){
            nexterm <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), i)
            term <- paste(term, nexterm, sep=" ")
        }
    }
    df$start <- term
    df$end   <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), N)
    return(df)
}
# ------------------------------------------------------------------------------

load(file=paste(datadir, "ntddf.save",  sep="\\"))
load(file=paste(datadir, "ntddf2.save", sep="\\"))
load(file=paste(datadir, "ntddf3.save", sep="\\"))
load(file=paste(datadir, "ntddf4.save", sep="\\"))

# fast, no need to save again
ntddf2 <- addIndexN(ntddf2 ,2)
ntddf3 <- addIndexN(ntddf3 ,3)
ntddf4 <- addIndexN(ntddf4 ,4)
# head(ntddf2)
resetApo <- function(string){
    string <- stri_replace_all_regex(string, APO, "'")
}
#############    set prediction from a prhase     ########################

test = "Baby. relax. it`s not time to make a change. at the same"
test = "want to break"
teststring2("should go to")
teststring2 <- function(test){
    ctest <- vecToCorpus(test, lang="en_US")
    ltest <- as.list(ctest)
    # last phrase (no prediction after a ".")
    last <- ltest[[length(ltest)]]
    # convert to char
    charlast <- as.character(last)
    # split in words
    a <- strsplit(charlast, " " )[[1]]
    numw <- length(a)

    if(numw >= 4){
        x4 <- paste(a[(numw-3):(numw-1)], collapse=" ")        
        x3 <- paste(a[(numw-2):(numw-1)], collapse=" ")        
        x2 <- a[numw-1] 
        print(x4)
        best4 <- ntddf4[ntddf4$start==x4,][1,"end"]
        best3 <- ntddf3[ntddf3$start==x3,][1,"end"]
        best2 <- ntddf2[ntddf2$start==x2,][1,"end"]
        p4 <- ntddf4[ntddf4$start==x4,][1,"logprob"]
        p3 <- ntddf3[ntddf3$start==x3,][1,"logprob"]
        p2 <- ntddf2[ntddf2$start==x2,][1,"logprob"]
        best4 <- resetApo(best4)
        best3 <- resetApo(best3)
        best2 <- resetApo(best2)
    }
    if(!is.na(best4)){
        cat(sprintf("[%s] use 3 word pred with prob %e\n",best4, exp(p4)))
    }else if(!is.na(best3)){
        cat(sprintf("[%s] use 2 word pred with prob %e\n",best3, exp(p3)))
    }else if(!is.na(best2)){
        cat(sprintf("[%s] use 1 word pred with prob %e\n",best2, exp(p2)))
    }else{
        cat("no match\n")
    }
}
x4 <- "a b c"
head()
head(ntddf4, 100)
# need complete ntddfx and functions brokeInPhases, vecToCorpus
teststring2("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
teststring2("You're the reason why I smile everyday. Can you follow me please? It would mean the")
teststring2("Hey sunshine, can you follow me and make me the")
teststring2("Very early observations on the Bills game: Offense still struggling but the")
teststring2("Go on a romantic date at the")
teststring2("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
teststring2("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
teststring2("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
teststring2("Be grateful for the good times and keep the faith during the")
teststring2("If this isn't the cutest thing you've ever seen, then you must be")

teststring2("I walk to")
teststring2("I was in")

head(ntddf4, 2000)

#


?TermDocumentMatrix










#-----------------------------------------------------------------
#############    bla bla     ########################
#-----------------------------------------------------------------


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
