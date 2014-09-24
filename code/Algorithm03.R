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
NOHAPAX <- F
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
corpfiles

# load list of corpus files
load(file=paste(datadir, CFILES, sep="\\"))
# corpfiles
# load saved corpus
load(file=paste(datadir, "01_traincorpus.save", sep="\\"))
#rm(chunk_corpus)
# inspect(chunk_corpus[1:100])


#############    tokenize  ###############
# NB: it is possible to parallelize the tokenization. Just divide the corpus in slices
# then put togheter the resultant data frame. Then group by over the ngram (with ddply)
# TBD parametrize.  
loadNGram <- function(corpus, N){
    ntokenizer <- function(x, toks) function(x) NGramTokenizer(x, Weka_control(min = toks, max = toks))
#     if(N==1){
#         tdm <-  TermDocumentMatrix(corpus)
#     }else{
        tdm <-   TermDocumentMatrix(corpus, control = list(tokenize = ntokenizer(toks=N),wordLengths=c(1,Inf)))
#     }
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
addIndexN <- function(df, N){
    # first term
    first <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), 1)
    # last term
    last <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), N)
   
    mid <- NULL
    if(N!=2){
        for(i in 2:(N-1)){
            nexterm <- vapply(strsplit(as.character(df$term), split=" "), '[', character(1L), i)
            mid <- paste(mid, nexterm, sep=" ")
        }
        df$mid   <- mid
 #       df$pre   <- paste(first, mid, sep=" ")
 #       df$post  <- paste(mid, last, sep=" ")
        
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


#############    Calculate log prob with kneser ney       ########################
addCountLambda1 <- function(df, df2, df3){
    # modify df
    bigrams <- nrow(df2)
    
    CDotWord <- function(word) nrow(df2[df2$start==word,]) 
 
    print("calculate word dot")
    CWordDot <- function(word) nrow(df2[df2$end==word,]) # how many bigram begins with word
    NWordDot <- vapply(df$term, CWordDot, numeric(1))

    print("calculate dot word")
    CDotWord <- function(word) nrow(df2[df2$start==word,]) # how many bigram ends with word
    NDotWord <- vapply(df$term, CDotWord, numeric(1))
    
    df$Pcont <- NDotWord/ bigrams
    
    print("calculate dot word dot")
    #NWork, in how many trigram the word ist (in the middle)
    CDotWordDot <- function(word) nrow(df3[df2$mid==word,])
    NDotWordDot <- vapply(df$term, CdotWordDot, numeric(1))
        
    lambda <- (D /NDotWordDot) * DWordDot
    df$lambda <- lambda
    df$NDotWordDot <- NNDotWordDot
 
    return(df)
}


addCount2 <- function(df2, df3){
    print("doing count")  
    goodTrigrams <- nrow(df3[substr(df3$start, 1, 5)!=BBG,])
    
    vec <- as.character(df3$term)
    firstTerm <- vapply(strsplit(vec, split=" "), '[', character(1L), 1)
    rest <- substr(vec, nchar(firstTerm)+2 , 1000)
    
    fw3 <- function(bigram)  sum(rest==bigram)
    df2$contin <- vapply(df2$term, fw3, numeric(1)) # 12 min
    df2$logPrCon <- log(df2$contin/ df2$cnt)
    #
    return(df2)
}
addLambda2 <- function(df2, df3){
    print("now doing lambda")
    
    goodTrigrams <- nrow(df3[substr(df3$start, 1, 5)!=BBG,])
    
    flw3 <- function(bigram) nrow(df3[df3$start==bigram & df3$end!=EEN,])
    ss <- vapply(df2$term, flw3, numeric(1))
    lambda <- (D / goodTrigrams) * ss
    df2$lambda <- lambda
    return(df2)
}

addCountLambda2 <- function(df2, df3){
    # 
}

#-------------------------
# this is so slow.
# fast, no need to save again
tddf2 <- addIndexN(tddf2 ,2)
tddf3 <- addIndexN(tddf3 ,3)

system.time(ntddf <-  addCountLambda1(tddf, tddf2, tddf3))
system.time(ntddf2 <- addCount2(tddf2, tddf3))
system.time(ntddf2 <- addLambda2(ntddf2, tddf3))
ntddf3 <- tddf3

save(ntddf, ntddf2, ntddf3, file=paste(datadir, "ntddf.save",  sep="\\"))

head(ntddf2)

#trigram <- "i am tired"

probTrigram <- function(trigram){
# calculate the trigram probability
    first <- strsplit(trigram, split=" ")[[1]][1]
    secon <- strsplit(trigram, split=" ")[[1]][2]
    third <- strsplit(trigram, split=" ")[[1]][3]
    
    big1 <- paste(first, secon)
    big2 <- paste(secon, third)
    goodTrigrams <- nrow(ntddf3[ntddf3$end != EEN & substr(ntddf3$start, 1, 5)!=BBG,])
    
    if(sum(ntddf3$term==trigram)==0){
        triTerm <-0
    }else{    
        triTerm <- max(ntddf3[ntddf3==trigram,"cnt"]-D, 0) / goodTrigrams
    }
    
    if(sum(ntddf2$term==big1)==0 | sum(ntddf2$term==big2)==0){
        biTerm <-0
    }else{ 
        biTerm  <- ntddf2[ntddf2$term==big1,"lambda"] * exp(ntddf2[ntddf2$term==big2,"logPrCon"])
    }
    
    if(sum(ntddf$term==secon) | sum(ntddf$term==third)==0 ){
        monTerm <-0
    }else{
        monTerm <- ntddf[ntddf$term==secon,"lambda"]*exp(ntddf[ntddf$term==third,"logPrCon"])
    }
    cat("first term: ", triTerm, "second term: ", biTerm, "third term: ", monTerm)
    return(triTerm + biTerm+ monTerm)
}


# ****************************************************    SONO QUI

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



#############    Calculate log prob with discounting       ########################
# ---- cstar
# TBD: replace with Kneser-Ney

# getc <- function(i, df, cntZero){
#     # poor man algorithm.
#     # c0 = v**N, c1-c4 simple good turing, then subtract 0.75
#     if(i == 1){
# # assign to the hapax the 0 count
#         cstar <- cntZero
#     }else if(i <= 6){
#         Ni  <- sum(df$cnt ==i)
#         Ni1 <- sum(df$cnt == (i+1))
#         cstar<-(df[i,"cnt"] +1)*(Ni1/Ni)
#     }else{
#         cstar <- i - 0.75
#     }
#     return(cstar)
# }
# addcstar <-function(df, bigN){
#     # bigN denominator for N0 calc. Types for unigram, power of V for multigram
#     hapax <- sum(df$cnt == 1) * 1.0
#     df$cstar <- sapply(df[,"cnt"], function(x) getc(x,df, hapax/bigN) )
#     return(df)
# }
# 
# addLogProb <- function(df){
#     base <- sum(df$cstar)
#     df$logprob = log(df$cstar/base)
#     return(df)
# }

#-----------------------------------------------------------------
# -- total probability of unseen = Hapax / type
# Types <- sum(tddf$cnt)
# V <- nrow(tddf)
# 
# # 1-gram
# system.time(ntddf <- addcstar(tddf, Types))
# ntddf <- addLogProb(ntddf)
# 
# # 2,3,4-gram
# system.time(ntddf2 <- addcstar(tddf2, V**2))
# system.time(ntddf3 <- addcstar(tddf3, V**3))
# system.time(ntddf4 <- addcstar(tddf4, V**4))
# 
# ntddf2 <- addLogProb(ntddf2)
# ntddf3 <- addLogProb(ntddf3)
# ntddf4 <- addLogProb(ntddf4)

# save(ntddf,  file=paste(datadir, "ntddf.save",  sep="\\"))
# save(ntddf2, file=paste(datadir, "ntddf2.save", sep="\\"))
# save(ntddf3, file=paste(datadir, "ntddf3.save", sep="\\"))
# save(ntddf4, file=paste(datadir, "ntddf4.save", sep="\\"))
# 
# 
# rm(ntddf, ntddf2, ntddf3, ntddf4)



load(file=paste(datadir, "ntddf.save",  sep="\\"))
load(file=paste(datadir, "ntddf2.save", sep="\\"))
load(file=paste(datadir, "ntddf3.save", sep="\\"))
load(file=paste(datadir, "ntddf4.save", sep="\\"))

#ntddf4 <- addIndexN(ntddf4 ,4)
# head(ntddf2)

#############    set prediction from a prhase     ########################
resetApo <- function(string){
    string <- stri_replace_all_regex(string, APO, "'")
}

teststring2 <- function(test){
    #-------------------------------------------------------------------------------
    #  test predictions 
    #-------------------------------------------------------------------------------
    ctest <- vecToCorpus(test, lang="en_US")
    ltest <- as.list(ctest)
    # last phrase (no prediction after a ".")
    last <- ltest[[length(ltest)]]
    # convert to char
    charlast <- as.character(last)
    # split in words
    a <- strsplit(charlast, " " )[[1]]
    numw <- length(a)
    
    #    if(numw >= 4){
    if(numw >= 3){
        #        x4 <- paste(a[(numw-3):(numw-1)], collapse=" ")        
        x3 <- paste(a[(numw-2):(numw-1)], collapse=" ")        
        x2 <- a[numw-1] 
        #  best4 <- ntddf4[ntddf4$start==x4,][1,"end"]
        best3 <- ntddf3[ntddf3$start==x3,][1,"end"]
        best2 <- ntddf2[ntddf2$start==x2,][1,"end"]
#        p4 <- ntddf4[ntddf4$start==x4,][1,"logprob"]
        p3 <- ntddf3[ntddf3$start==x3,][1,"logprob"]
        p2 <- ntddf2[ntddf2$start==x2,][1,"logprob"]
#        best4 <- resetApo(best4)
        best3 <- resetApo(best3)
        best2 <- resetApo(best2)
    }
    #    if(!is.na(best4)){
    #        cat(sprintf("[%s] use 3 word pred with prob %e\n",best4, exp(p4)))
    #    }else if(!is.na(best3)){
    if(!is.na(best3)){
        cat(sprintf("[%s] use 2 word pred with prob %e\n",best3, exp(p3)))
    }else if(!is.na(best2)){
        cat(sprintf("[%s] use 1 word pred with prob %e\n",best2, exp(p2)))
    }else{
        cat("no match\n")
    }
}
head(ntddf3$start)
head(ntddf2)
sort()
ntddf3[ntddf3$start == "mean the",]

# ---- check manually
bigram <- "me the"
ntddf3[ntddf3$start == bigram,]

monogram <- "the"
contin <- function(monogram){
    sus <- ntddf2[ntddf2$start == monogram,]
    sus[order(-sus[5]), ]
}
a <- contin(monogram)
a[grep("est", a$end),]
c("happiest", "smelliest", "bluest", "saddest")
ntddf2[ntddf2$end =="saddest",]


?sort
teststring3(trigram){
    
}
#x4 <- "a b c"
corp <- readLines(trainfile, encoding="UTF-8")
?grep
grep("a case of beer", corp) # 2
grep("a case of soda", corp) # 0
grep("a case of cheese", corp) # 0
grep("a case of pretzels", corp) # 0

grep("would mean the universe", corp)
grep("would mean the world", corp) # 1
grep("would mean the most", corp)
grep("would mean the best", corp)


grep("make me the happiest", corp) #1
grep("make me the smelliest", corp)
grep("make me the bluest", corp)
grep("make me the saddest", corp)


grep("struggling but the crowd", corp)
grep("struggling but the referees", corp)
grep("struggling but the players", corp)
grep("struggling but the defense", corp)
grep("but the crowd", corp) #6
grep("but the referees", corp)
grep("but the players", corp) #1
grep("but the defense", corp)

grep("date at the mall", corp)
grep("date at the grocery", corp)
grep("date at the movies", corp)
grep("date at the beach", corp)
grep("at the mall", corp) #36
grep("at the grocery", corp) #94
grep("at the movies", corp) #8
grep("at the beach", corp) #111


grep("be on my motorcycle", corp)
grep("be on my horse", corp)
grep("be on my way", corp) #6
grep("be on my phone", corp)


grep("in quite some time", corp)
grep("in quite some thing", corp)
grep("in quite some years", corp)
grep("in quite some weeks", corp)
grep("quite some time", corp) #154
grep("quite some thing", corp) #1
grep("quite some years", corp) #1
grep("quite some weeks", corp)

# candidate to KN
grep("with his little toes", corp)
grep("with his little eyes", corp)
grep("with his little fingers", corp)
grep("with his little ears", corp)
grep("his little toes", corp)
grep("his little eyes", corp) #3
grep("his little fingers", corp) #3
grep("his little ears", corp)
grep("little eyes", corp)    # 15
grep("little fingers", corp) # 16


grep("faith during the worse", corp)
grep("faith during the sad", corp)
grep("faith during the bad", corp)
grep("faith during the hard", corp)
grep("during the worse", corp)
grep("during the sad", corp)
grep("during the bad", corp)
grep("during the hard", corp) #1



grep("you must be asleep", corp)
grep("you must be insane", corp)
grep("you must be callous", corp)
grep("you must be insensitive", corp)
grep("must be asleep", corp)
grep("must be insane", corp) #1
grep("must be callous", corp) # 
grep("must be insensitive", corp)




head()
head(ntddf4, 100)
# need complete ntddfx and functions brokeInPhases, vecToCorpus
rm(x4)
teststring2("The guy in front of me just bought a pound of bacon, a bouquet, and a case of")
# ???
teststring2("You're the reason why I smile everyday. Can you follow me please? It would mean the")
# world
teststring2("Hey sunshine, can you follow me and make me the")
# ???
teststring2("Very early observations on the Bills game: Offense still struggling but the")
teststring2("Go on a romantic date at the")
teststring2("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my")
teststring2("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some")
teststring2("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little")
teststring2("Be grateful for the good times and keep the faith during the")
teststring2("If this isn't the cutest thing you've ever seen, then you must be")

teststring2("case of soda")
teststring2("I was in")

head(ntddf4, 2000)

#
test

?TermDocumentMatrix
matrix <- TermDocumentMatrix(chunk_corpus,control=list(wordLengths=c(1,Inf)))
inspect(matrix[1:10,1:10])

a <- exp(Inf)
a






#-----------------------------------------------------------------
#############    bla bla     ########################
#-----------------------------------------------------------------


#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
