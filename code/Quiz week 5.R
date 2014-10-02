Capstone_Main_Dir <- "C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone"
Github_URL <- "https://raw.githubusercontent.com/momobo/Capstone_Dryrun/master/code/CapstoneBase.R"

# decide the language to train for
lang <- "en_US"
#lang <- "de_DE"

lastBigram <- function(phrase){
# helper function
    a <- strsplit(phrase, split=" ")[[1]]
    l <- length(a)
    return(paste(a[(l-1):l], collapse=" "))
}
lastNgram <- function(phrase, n){
    # helper function
    a <- strsplit(phrase, split=" ")[[1]]
    l <- length(a)
    return(paste(a[(l-(n-1)):l], collapse=" "))
}
quizsearch <- function(four, altern, corp, n){
    for(i in 1:4){
        grepped <- paste(lastNgram(four, n), altern[i])
        print(grepped)
        print(grep(grepped, corp,  ignore.case = T))
    }
}
#############    initialize variables ########################

setwd(Capstone_Main_Dir)
source(Github_URL)

datadir <- paste0(".\\data\\final\\", lang)
fileb   <- paste0(lang, ".blogs.txt")
file <- paste(datadir, fileb, sep="\\")
trainfile <- paste(datadir, "train.txt", sep="\\")
twitfile <- paste(datadir, "en_US.twitter.txt", sep="\\")


load(file=paste(datadir, "ntddf.save",  sep="\\"))
load(file=paste(datadir, "ntddf2.save", sep="\\"))
load(file=paste(datadir, "ntddf3.save", sep="\\"))

# read all corpus
corp <- readLines(trainfile, encoding="UTF-8")
corpt <- readLines(twitfile, encoding="UTF-8")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

#probTrigram("I'd live and I'd", ntddf, ntddf2, ntddf3)

#1
four <- "I'd live and I'd"
altern <- c("eat", "sleep", "give", "die")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# unigram give 85 5.246386e-04
quizsearch(four, altern, corp, 3)
quizsearch(four, altern, corp, 2) # eat 1 give 1

quizsearch(four, altern, corpt, 3)
quizsearch(four, altern, corpt, 2)
# give wrong
# no good result from twitter
# eat ! wrong
# die ok


#2
four <- "he started telling me about his"
altern <- c("horticultural", "spiritual", "financial", "marital")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# spiritual                bigram   2 2.141198e-02
quizsearch(four, altern, corp, 3)
quizsearch(four, altern, corp, 2) # spiritual 1
# spiritual wrong
#
# financial? wrong
# marital ok


#3
four <- "see arctic monkeys this"
altern <- c("morning", "decade", "month", "weekend")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
#1 morning  bigram  24 6.780462e-03
#3   month  bigram   8 6.780462e-03
#4 weekend  bigram   7 6.780462e-03
# aex aequo, use cnt to break tie
quizsearch(four, altern, corp, 3)
quizsearch(four, altern, corp, 2)
quizsearch(four, altern, corp, 1) # morning 1858
# morning wrong
# still wrong
# weekend second choice ok

#4
four <- "and helps reduce your"
altern <- c("hunger", "stress", "happiness", "sleepiness")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
#  stress unigram  10 8.743976e-05
quizsearch(four, altern, corp, 3)
quizsearch(four, altern, corp, 2) # stress 2
# stress OK


#5
c(four <- "time to take a")
altern <- c("picture", "look", "walk", "minute")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# 2    look trigram   5 0.14832544
quizsearch(four, altern, corp, 3) # picture 79, look 79 (grrr..)
# look wrong
# still wrong. second is picture ok


#6
four <- "jury to settle the"
altern <- c("matter", "case", "incident", "account")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# 1   matter  bigram   2 4.509242e-02
# 2     case  bigram   5 4.509242e-02
quizsearch(four, altern, corp, 3) # matter 2 case 1 
# case wrong. matter second ok

#7
four <- "of groceries in each"
altern <- c("toe", "arm", "finger", "hand")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# 4   hand unigram  50 2.623193e-04
quizsearch(four, altern, corp, 3) 
quizsearch(four, altern, corp, 2) # hand 6
# hand OK

#8
four <- "the bottom to the"
altern <- c("center", "top", "side", "middle")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# 1 center bigram   7 0.04509242
# 2    top bigram  12 0.04509242
# 3   side bigram  10 0.04509242
# 4 middle bigram  10 0.04509242
quizsearch(four, altern, corp, 3) # top 5
# top OK

#9
four <- "and bruises from playing"
altern <- c("outside", "daily", "inside", "weekly")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# 3  inside unigram  33 1.748795e-04
quizsearch(four, altern, corp, 3)
quizsearch(four, altern, corp, 2) # outside 2
# inside wrong
# still wrong. Outside second OK

#10
four <- "all of Adam Sandler's"
altern <- c("movies", "stories", "pictures", "movies")
quiz(lastBigram(four), altern, ntddf, ntddf2, ntddf3)
# 3 pictures unigram  30 2.185994e-04
quizsearch(four, altern, corp, 3)
quizsearch(four, altern, corp, 2)
quizsearch(four, altern, corp, 1)
# pictures wrong
# stories? wrong
# movies ok




