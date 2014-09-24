#------------------------------------------------------------------------------------------------
# Algorithm
# Note:
#   first Data Dable version
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
library(data.table)
#-----------------------------------------------------------------------------------------------

#############    initialize variables ########################


datadir <- ".\\data\\final\\en_US"
fileb   <- "en_US.blogs.txt"
# list of bad words
LOADBAD <- F
if(LOADBAD){ 
# modify to reload, badword not used at the moment
  urlbad <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  download.file(urlbad, ".\\badWords", method = "auto", quiet=FALSE)
  badwords <- readLines(".\\badWords")
}

file <- paste(datadir, fileb, sep="\\")
trainfile <- paste(datadir, "train.txt", sep="\\")
BBG     <- "__s__"
EEN     <- "__es__"
APO     <- "__ap__"
CFILES  <- "stucknames.save"
limit   <-  10000000
mapLen  <- 2000
NOHAPAX <- T
D       <- 0.75
MINPROB <- 1E-8

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
  testfile  <- file(paste(datadir, "test.txt",  sep="\\"), encoding="utf8")
  
  write(train, file = trainfile) # verify encoding
  write(valid, file = validfile) # verify encoding
  write(test,  file = testfile ) # verify encoding
  
  close(trainfile)
  close(validfile)
  close(testfile)
}

#----------------------------------------------------------------

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
    return(corpfiles)
}

vecToCorpus <- function(vec, lang){
    #--------------------------------------------------------------------------
    # FUN: from character vector to corpus
    #--------------------------------------------------------------------------

    # special function for apostrophe and comma
    #--------------------------------------------------------------------------
    substApostrophe <- function(x) UseMethod("substApostrophe", x)
    substApostrophe <- substApostrophe  <- function(x) {
        x <- stri_replace_all_regex(x, "['`´\u2018\u2019\u2027\u201B]", APO)
        return(x)
    }
    killCommas <- function(x) UseMethod("killCommas", x)
    killCommas <- killCommas  <- function(x) {
        x <- stri_replace_all_regex(x, "[,]", " ")
        return(x)
    }    
    myTolower <- function(x) stri_trans_tolower(x, lang)
    #--------------------------------------------------------------------------
    x <- brokeInPhrases(vec)
    x <- paste(BBG, x, EEN)
    
    myCorpus <- Corpus(VectorSource(x), list(language = lang))
    # here apply filtering (pay attention at the <)
    myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
    myCorpus <- tm_map(myCorpus, content_transformer(stripWhitespace))
    # tolower 
    myCorpus <- tm_map(myCorpus, content_transformer(myTolower))
    
    # treatment of apostrophe
    myCorpus <- tm_map(myCorpus, content_transformer(substApostrophe) )
    

    return(myCorpus)    
}

fakeCorp <- function(n, name="traincorpus.save"){
    c <- NULL
    for(i in 1:n){
        n <- sprintf("%02d_%s", i, name)
        c[i] <- n
    }
    return(c)
}


#-----------------------------------------------------------------

#############    tokenize  ###############
# NB: it is possible to parallelize the tokenization. Just divide the corpus in slices
# then put togheter the resultant data frame. Then group by over the ngram (with ddply)
loadNGram <- function(corpus, N){
    ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
    tdm <-   TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=N),wordLengths=c(1,Inf)))
    
    df <- tdm2df(tdm, N)
    return(df)
}

tdm2df <- function(tdm, N){
  #------------------------------------------------------------------------------
  # function. Create a data table from a tdm (BEWARE, cannot be too big)
  #------------------------------------------------------------------------------
  ft <- rowapply_simple_triplet_matrix(tdm, sum)
  dfft <- data.table(term = Terms(tdm), cnt = ft)
  setkey(dfft, term)
  # purge hapax (keep always unigram hapax)
  if(NOHAPAX==T&N>1){
      dfft <- dfft[dfft$cnt>1,]
  }
  
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
    cat("putting all togheter \n")
    tddf <- rbindlist(ldf)
    gr_tddf <- tddf[, list(cnt=sum(cnt)), by=term]
    setkey(gr_tddf, term)
    return(gr_tddf)
}
#-----------------------------------------------------------------

#############    set start and end fields               ########################

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

removeStartOfPhrase <- function(df, N){
    term <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), 1)
    if(N==1){
        df <- df[df$term != BBG,]
    }else{
        df <- df[df$start != BBG,]
    }
}

# ------------------------------------------------------------------------------

#############    Calculate log prob with kneser ney       ########################

addKNUnigram <- function(df, df2, df3, D1=D){
    # modify df. we operate for each term in df
    bigrams <- nrow(df2)
    
    #    CDotWord <- function(word) nrow(df2[df2$start==word,]) 
    CDotWord <- function(word) sum(df2$start==word) 
    
    print("calculate word dot")

    setkey(df2, end)    
    CWordDot <- function(word) nrow(df2[word]) # how many bigram begins with word
    NWordDot <- vapply(df$term, CWordDot, numeric(1))
    
    print("calculate dot word")
    setkey(df2, start)
    CDotWord <- function(word) nrow(df2[word]) # how many bigram ends with word
    NDotWord <- vapply(df$term, CDotWord, numeric(1))
    
    df$Pcont <- NDotWord/ bigrams
    
    print("calculate dot word dot")
    #NWork, in how many trigram the word ist (in the middle)
    
    setkey(df3, mid)
    CDotWordDot <- function(word) nrow(df3[word])
    NDotWordDot <- vapply(df$term, CDotWordDot, numeric(1))
    
    
    df$lambda <- (D1 /NWordDot) * NDotWordDot
    # TBD eliminate Nan
    
    df$NDotWordDot <- NDotWordDot
    df[is.nan(df$lambda),"lambda"] <- 0
    
    # reset the keys
    setkey(df2, term)  
    setkey(df3, term)  
    return(df)
}

addKNBigram <- function(df, df2, df3, D2=D){
    # modify df2. we operate for each term in df2
    # need df2 already processed
    

    setkey(df3, post)
    CDotW1W2 <- function(w1w2) nrow(df3[w1w2])
    NDotW1W2 <- vapply(df2$term, CDotW1W2, numeric(1))
    
    setkey(df3, pre)
    CW1W2Dot <- function(w1w2) nrow(df3[w1w2])
    NW1W2Dot <- vapply(df2$term, CW1W2Dot, numeric(1))
    
    w2 <- vapply(strsplit(as.character(df2$term), split=" "), '[', character(1L), 1)
    w3 <- vapply(strsplit(as.character(df2$term), split=" "), '[', character(1L), 2)
    
#     # df already indexed on term (doing 3 times is ineffic.)-------------------
#     Clambda1 <- function(word) df[word]$lambda
#     NlambdaW2 <- vapply(w2, Clambda1, numeric(1))
#     CPcont1 <- function(word) df[word]$Pcont 
#     NPcontW3 <- vapply(w3, CPcont1, numeric(1))
#     CDotWordDot <- function(word) df[word]$NDotWordDot
#     NDotWordDot <- vapply(w2, CDotWordDot, numeric(1))
    
    #--------------------------------------------------------------------------
    CWord <- function(word) (df[word])
    PWord <- sapply(w2, CWord, simplify = "array")
    NDotWordDot <- unlist(PWord[,"NDotWordDot",])
    NlambdaW2   <- unlist(PWord[,"lambda",])
    NPcontW3    <- unlist(PWord[,"Pcont",])
    
    df2$lambda2 <- (D2 /df2$cnt) * NW1W2Dot
    
    df2$Pcont2 <- pmax(NDotW1W2 -D2, 0) / NDotWordDot + NlambdaW2 * NPcontW3
    df2[is.nan(df2$Pcont2),"lambda2"] <- 0

    setkey(df3, term)  
    return(df2)
}

addKNTrigram <- function(df2, df3, D3=D){
    # modify df3. we operate for each term in df3
    w1 <- vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 1)
    w2 <- vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 2)
    w3 <- vapply(strsplit(as.character(df3$term), split=" "), '[', character(1L), 3)
    w1w2 <- paste(w1, w2, sep=" ")
    w2w3 <- paste(w2, w3, sep=" ")

    # semplify like bigram (beware that are two distinct index)
    FcW1W2 <- function(w1w2) df2[w1w2]$cnt
    NcW1W2 <- vapply(w1w2, FcW1W2, numeric(1))
    
    FlambdaW1W2 <- function(w1w2)  df2[w1w2]$lambda2
    NlambdaW1W2 <- vapply(w1w2, FlambdaW1W2, numeric(1))
    
    FprobW2W3 <- function(w2w3) df2[w2w3]$Pcont2
    NprobW2W3 <- vapply(w2w3, FprobW2W3, numeric(1))
    
    
    MaxLikelTerm <- pmax(df3$cnt-D3,0)/NcW1W2
    
    df3$PKN <- MaxLikelTerm + NlambdaW1W2 * NprobW2W3

    return(df3)
}

#-------------------------

#############    check model with perplexity              ########################
resetApo <- function(string){
    # not really used
    string <- stri_replace_all_regex(string, APO, "'")
}

probTrigram <- function(trigram, minProb = MINPROB){
    # tbd add treatment of apostrophe
    # calculate the trigram probability
    first <- strsplit(trigram, split=" ")[[1]][1]
    secon <- strsplit(trigram, split=" ")[[1]][2]
    third <- strsplit(trigram, split=" ")[[1]][3]
    
    big1 <- paste(first, secon)
    big2 <- paste(secon, third)

    type = "no prediction"
    cnt <- 0
    P <- minProb
    if(sum(ntddf3$term==trigram)!=0){
        type<-"trigram"
        cnt <- ntddf3[trigram]$cnt
        P   <- ntddf3[trigram]$PKN
 
    }else if(sum(ntddf2$term==big2)!=0 ){
        type<-"bigram"
        cnt <- ntddf2[big2]$cnt
        P   <- ntddf2[big2]$Pcont2 * D
        
    }else if(sum(ntddf$term==third)!=0 ){
        type<-"unigram"
        cnt <- ntddf[third]$cnt
        P   <- ntddf[third]$Pcont * D* D
    }else if(sum(ntddf3$start!=0)){
        # still no prediction
        type<-"leave one out trigram"
        a <- ntddf3[ntddf3$start == first,]
        cnt <- a[order(-cnt)][1]$cnt
        P   <- minProb * 0.5 # little more than the minimum
    }
    rc <- list(type, cnt, P)
    return(rc)
}

nextWord <- function(sentence, minProb=MINPROB){
    # tbd add treatment of apostrophe
    # calculate the trigram probability
    sentence <- stri_replace_all_regex(sentence, "['`´\u2018\u2019\u2027\u201B]", APO)
    
    # gracefully handle the case of a long phrase
    reverse <- rev(strsplit(sentence, split=" ")[[1]])
    
    if(length(reverse)==0){
        print("no string")
        rc <- ntddf[order(-Pcont)][1:3,]$term
        return(rc)
    }
    
    secon <- reverse[1]
    first <- reverse[2]
    bigram <- paste(first, secon)

    if(sum(ntddf3$pre==bigram)!=0){
        print("trigram")
        a <- ntddf3[ntddf3$pre==bigram & ntddf3$end != EEN,]
        print( a[order(-PKN)][1:3,]$term)
        rc <- a[order(-PKN)][1:3,]$end
        
    }else if(sum(ntddf2$start==secon)!=0 ){
        print("bigram")
        a <- ntddf2[ntddf2$start==secon & ntddf2$end != EEN,]
        rc  <- a[order(-Pcont2)][1:3,]$end

    }else if(sum(ntddf3$start==first)){
        # still no prediction
        print("jump over one")
        a <- ntddf3[ntddf3$start== first & ntddf3$end != EEN,]
        print( a[order(-cnt)][1:3,]$term)
        rc  <- a[order(-cnt)][1:3,]$end

    }else{
        print("you know nothing Jon Snow")
        # choose the first thre with biggest continuation prob
        rc <- ntddf[order(-Pcont)][1:3,]$term
    }
    rc <- stri_replace_all_regex(rc, APO, "'")
    return(rc)
}

probCalc <- function(vec, minProb = MINPROB){
    # tbd add treatment of apostrophe
    first <- vec[1]
    secon <- vec[2]
    third <- vec[3]
    
    trigram <- paste(first, secon, third)
    big1 <- paste(first, secon)
    big2 <- paste(secon, third)
    #    goodTrigrams <- nrow(ntddf3[ntddf3$end != EEN & substr(ntddf3$start, 1, 5)!=BBG,])
    
    type = "no prediction"
    cnt <- 0
    P <- minProb
    if(sum(ntddf3$term==trigram)!=0){
        P   <- ntddf3[trigram]$PKN
        
    }else if(sum(ntddf2$term==big2)!=0 ){
        P   <- ntddf2[big2]$Pcont2 * D
        
    }else if(sum(ntddf$term==third)!=0 ){
        P   <- ntddf[third]$Pcont * D* D
    }
    #rc <- list(type, cnt, P)
    return(P)
}

perplexity <- function(logP,N){
    # wants a vector of logP
    return(exp((-1/N)*sum(logP)))
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


#-----------------------------------------------------------------


measurePerp <- function(dir=datadir, file="valid.txt", NN=100, slot=1){
    validfile <- paste(datadir, "valid.txt", sep="\\")
    # read validfile
    validVec <- readLines(validfile, encoding="UTF-8", n=(10*NN))

    ctest <- vecToCorpus(validVec[((slot-1)*NN):(slot*NN)], lang="en_US")    
    
    logVec <- NULL
    for(i in 1:length(ctest)){
        satz <- as.character(ctest[[1]])
        vecSatz <- strsplit(satz, split=" ")[[1]]
        
        vecProb <- NULL
        l <- length(vecSatz)
        for(j in 2:l){
            pred <- vecSatz[(j-2):j]
            if(length(pred) == 2) pred <- c(BBG, pred)
            suppressWarnings(prob <- probCalc(pred))
            vecProb[j-1] <- log(prob)
        }
        logVec <- c(logVec, vecProb)
    }
    # vectorize phrase
    perplexity(logVec, length(logVec))
}

#############    set prediction from a phrase     ########################


#-----------------------------------------------------------------
#############   determine next word   ########################
#-----------------------------------------------------------------
# next word model


prepareWords <- function(unigrams){
    # prepare indexed dictionary of words starting from unigrams
    words <- copy(unigrams)
    words$l1 <- substr(words$term,1,1)
    words$l2 <- substr(words$term,1,2)
    words$l3 <- substr(words$term,1,3)
    words$l4 <- substr(words$term,1,4)
    words$l5 <- substr(words$term,1,5)
    words$l6 <- substr(words$term,1,6)
    words$l7 <- substr(words$term,1,7)
    words$l8 <- substr(words$term,1,8)
    words$l9 <- substr(words$term,1,9)
    return(words)
}



completeWord <- function(word, dictionary){  
    substApostrophe  <- function(x) {
        x <- stri_replace_all_regex(x, "['`´\u2018\u2019\u2027\u201B]", APO)
        return(x)
    }
    word <- substApostrophe(word)
    lw <- nchar(word)
    if(lw==1) setkey(dictionary, l1)
    else if(lw==2) setkey(dictionary, l2)
    else if(lw==3) setkey(dictionary, l3)
    else if(lw==4) setkey(dictionary, l4)
    else if(lw==5) setkey(dictionary, l5)
    else if(lw==6) setkey(dictionary, l6)
    else if(lw==7) setkey(dictionary, l7)
    else if(lw==8) setkey(dictionary, l8)
    else if(lw>=9) setkey(dictionary, l9)
    
    # had to suppress the warning of mixed ascii utf-8
    suppressWarnings(sel <- dictionary[word])
    rc <- sel[order(-cnt),][1:3,]$term
    
    # reset apostrophe
    rc <- stri_replace_all_regex(rc, APO, "'")
    setkey(dictionary, term)
    
    print(sel[order(-cnt),][1:3,]$cnt)
    
    return(rc)
}
