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
NOHAPAX <- T
D <- 0.75
lang <- "en_US"
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
  write(valid, file = validfile) # verify encoding
  write(test,  file = testfile ) # verify encoding
  
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
    killCommas <- function(x) UseMethod("killCommas", x)
    killCommas <- killCommas  <- function(x) {
        x <- stri_replace_all_regex(x, "[,]", " ")
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
#corpfiles

# load list of corpus files
load(file=paste(datadir, CFILES, sep="\\"))
# corpfiles
# load saved corpus
#load(file=paste(datadir, "01_traincorpus.save", sep="\\"))
#rm(chunk_corpus)
# inspect(chunk_corpus[1:100])


#############    tokenize  ###############
# NB: it is possible to parallelize the tokenization. Just divide the corpus in slices
# then put togheter the resultant data frame. Then group by over the ngram (with ddply)
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
  # purge hapax
  if(NOHAPAX==T){
      dfft <- dfft[dfft$cnt>1,]
  }

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
#load(file=paste(datadir, "tddf4.save", sep="\\"))

#############    set start and end fields               ########################
#vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 2)
addIndexN <- function(df, N){
    # first term
    first <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), 1)
    # last term
    last <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), N)
   
    mid <- NULL
    if(N==3){
        mid   <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), 2)
        df$mid   <- mid
        df$pre   <- paste(first, mid, sep=" ")
        df$post  <- paste(mid, last, sep=" ")
        
    }else if(N>3){
        for(i in 2:(N-1)){
            nexterm <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), i)
            mid <- paste(mid, nexterm, sep=" ")
        }
        df$mid   <- mid
        df$pre   <- paste(first, mid, sep=" ")
        df$post  <- paste(mid, last, sep=" ")
        
    }
    df$start <- first
    df$end   <- last
    return(df)
}
removeStartOfPhrase <- function(df){
    term <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), 1)
    df <- df[term!=BBG,]
}
# ------------------------------------------------------------------------------
#   remove __s__ that mess the count
tddf  <- removeStartOfPhrase(tddf)
tddf2 <- removeStartOfPhrase(tddf2)
tddf3 <- removeStartOfPhrase(tddf3)

# fast, no need to save again
tddf2 <- addIndexN(tddf2 ,2)
tddf3 <- addIndexN(tddf3 ,3)

save(tddf,  file= paste(datadir, "tddf.save",  sep="\\"))
save(tddf2, file= paste(datadir, "tddf2.save", sep="\\"))
save(tddf3, file= paste(datadir, "tddf3.save", sep="\\"))

load(file=paste(datadir, "tddf.save",  sep="\\"))
load(file=paste(datadir, "tddf2.save", sep="\\"))
load(file=paste(datadir, "tddf3.save", sep="\\"))

#############    Calculate log prob with kneser ney       ########################

addKNUnigram <- function(df, df2, df3){
    # modify df. we operate for each term in df
    bigrams <- nrow(df2)
    
    #    CDotWord <- function(word) nrow(df2[df2$start==word,]) 
    CDotWord <- function(word) sum(df2$start==word) 

    print("calculate word dot")
    CWordDot <- function(word) sum(df2$end==word) # how many bigram begins with word
    NWordDot <- vapply(df$term, CWordDot, numeric(1))

    print("calculate dot word")
    CDotWord <- function(word) sum(df2$start==word) # how many bigram ends with word
    NDotWord <- vapply(df$term, CDotWord, numeric(1))
    
    df$Pcont <- NDotWord/ bigrams
    
    print("calculate dot word dot")
    #NWork, in how many trigram the word ist (in the middle)
    CDotWordDot <- function(word) sum(df3$mid==word)
    NDotWordDot <- vapply(df$term, CDotWordDot, numeric(1))
      
    df$lambda <- (D /NWordDot) * NDotWordDot
    # TBD eliminate Nan
    
    df$NDotWordDot <- NDotWordDot
    df[is.nan(df$lambda),"lambda"] <- 0
    return(df)
}


addKNBigram <- function(df, df2, df3){
    # modify df2. we operate for each term in df2
    # need df2 already processed
    
    CDotW1W2 <- function(w1w2) sum(df3$post==w1w2)
    NDotW1W2 <- vapply(df2$term, CDotW1W2, numeric(1))
    
    CW1W2Dot <- function(w1w2) sum(df3$pre==w1w2)
    NW1W2Dot <- vapply(df2$term, CW1W2Dot, numeric(1))
    
    w2 <- vapply(strsplit(as.character(df2$term), split=" "), '[', character(1L), 1)
    w3 <- vapply(strsplit(as.character(df2$term), split=" "), '[', character(1L), 2)
    
    Clambda1 <- function(word) df[df$term==word, "lambda"]
    NlambdaW2 <- vapply(w2, Clambda1, numeric(1))
    
    
    CPcont1 <- function(word) df[df$term==word, "Pcont"]
    NPcontW3 <- vapply(w3, CPcont1, numeric(1))
    
    CDotWordDot <- function(word) df[df$term==word, "NDotWordDot"]
    PDotWordDot <- vapply(w2, CDotWordDot, numeric(1))
    
    df2$lambda2 <- (D /df2$cnt) * NW1W2Dot
    
    df2$Pcont2 <- pmax(NDotW1W2 -D, 0) / PDotWordDot + NlambdaW2 * NPcontW3
    df2[is.nan(df2$Pcont2),"lambda2"] <- 0
    
    return(df2)
}

addKNTrigram <- function(df2, df3){
    # modify df3. we operate for each term in df3
    w1 <- vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 1)
    w2 <- vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 2)
    w3 <- vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 3)
    w1w2 <- paste(w1, w2, sep=" ")
    w2w3 <- paste(w2, w3, sep=" ")
    
    FcW1W2 <- function(w1w2) df2[df2$term==w1w2, "cnt"]
    NcW1W2 <- vapply(w1w2, FcW1W2, numeric(1))
    
    FlambdaW1W2 <- function(w1w2) df2[df2$term==w1w2, "lambda2"]
    NlambdaW1W2 <- vapply(w1w2, FlambdaW1W2, numeric(1))
    
    FprobW2W3 <- function(w2w3) df2[df2$term==w2w3, "Pcont2"]
    NprobW2W3 <- vapply(w2w3, FprobW2W3, numeric(1))
    
    MaxLikelTerm <- pmax(df3$cnt-D,0)/NcW1W2
    
    df3$PKN <- MaxLikelTerm + NlambdaW1W2 * NprobW2W3

    return(df3)
}

#x <- addKNTrigram(ntddf2, ntddf3[1:1000,])
#-------------------------


system.time(ntddf <-  addKNUnigram(tddf, tddf2, tddf3))
system.time(ntddf2 <- addKNBigram(ntddf, tddf2, tddf3))
system.time(ntddf3 <- addKNTrigram(ntddf2, tddf3))

save(ntddf,  file= paste(datadir, "ntddf.save",  sep="\\"))
save(ntddf2, file= paste(datadir, "ntddf2.save", sep="\\"))
save(ntddf3, file= paste(datadir, "ntddf3.save", sep="\\"))

load(file=paste(datadir, "ntddf.save",  sep="\\"))
load(file=paste(datadir, "ntddf2.save", sep="\\"))
load(file=paste(datadir, "ntddf3.save", sep="\\"))
#


resetApo <- function(string){
    string <- stri_replace_all_regex(string, APO, "'")
}
probTrigram <- function(trigram){
    # tbd add treatment of apostrophe
    # calculate the trigram probability
    first <- strsplit(trigram, split=" ")[[1]][1]
    secon <- strsplit(trigram, split=" ")[[1]][2]
    third <- strsplit(trigram, split=" ")[[1]][3]
    
    big1 <- paste(first, secon)
    big2 <- paste(secon, third)
    goodTrigrams <- nrow(ntddf3[ntddf3$end != EEN & substr(ntddf3$start, 1, 5)!=BBG,])

    type = "no prediction"
    cnt <- 0
    P <- 0
    if(sum(ntddf3$term==trigram)!=0){
        type<-"trigram"
        cnt <- ntddf3[ntddf3==trigram,"cnt"]
        P   <- ntddf3[ntddf3==trigram,"PKN"]
 
    }else if(sum(ntddf2$term==big2)!=0 ){
        type<-"bigram"
        cnt <- ntddf2[ntddf2==big2,"cnt"]
        P   <- ntddf2[ntddf2==big2,"Pcont2"]
        
    }else if(sum(ntddf$term==third)!=0 ){
        type<-"unigram"
        cnt <- ntddf[ntddf==third,"cnt"]
        P   <- ntddf[ntddf==third,"Pcont"]
    }
    rc <- list(type, cnt, P)
    return(rc)
}
quiz <- function(bigram, cont){
    df <- data.frame(word=NULL, type=NULL, cnt=NULL, prob=NULL)
    for(i in seq_along(cont)){
        trigram <- paste(bigram, cont[i], sep=" ")
        print(trigram)
        lis <- probTrigram(trigram)
        df[i,"word"] <- cont[i]
        df[i,"type"] <- lis[[1]]
        df[i,"cnt"]  <- lis[[2]]
        df[i,"prob"] <- lis[[3]]
    }
    return(df)
}
# ****************************************************    SONO QUI
quiz("case of", c("beer", "soda", "cheese", "pretzel"))
#-----------------------------------------------------------------




#ntddf4 <- addIndexN(ntddf4 ,4)
# head(ntddf2)

#############    set prediction from a prhase     ########################







#-----------------------------------------------------------------
#############    bla bla     ########################
#-----------------------------------------------------------------


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
