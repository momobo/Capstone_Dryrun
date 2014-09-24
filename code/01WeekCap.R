# week 01 capstone
getwd()
setwd("C:\\Users\\massimo\\Google Drive\\Data Science\\10_Capstone")
data <- ".\\data\\final\\en_US"
fileb <- "en_US.blogs.txt"
filen <- "en_US.news.txt"
filet <- "en_US.twitter.txt"

# Question 1: The en_US_blogs.txt file is how many megabytes?
# 205

# Question 2: The en_US.twitter.txt has how many lines of text?
# 2360148
bcorp <- readLines(paste(data,fileb, sep="\\"))
-- 899288 lines
tcorp <- readLines(paste(data,filet, sep="\\"))
-- 2360148 lines
ncorp <- readLines(paste(data,filen, sep="\\"))
-- 77259 lines
a <- head(bcorp)


# Question 3: What is the length of the longest line seen in any of the three en_US data sets?
# > 40 in bolgs
max(sapply(bcorp, nchar))
-- 40835
max(sapply(tcorp, nchar))
--
max(sapply(ncorp, nchar))
-- 

#  Question 4: In the en_US twitter data set, if you divide the number of lines where the word "love"
# (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, 
# about what do you get?

love.hate <- sum(grepl("love", tcorp)) / sum(grepl("hate", tcorp))
# 4.1
  
# Question 5: The one tweet in the en_US twitter data set that matches the word "biostats" says what?
tcorp[grep("biostats", tcorp)]
# "i know how you feel.. i have biostats on tuesday and i have yet to study =/"

# Question 6
# How many tweets have the exact characters 
# "A computer once beat me at chess, but it was no match for me at kickboxing"
# . (I.e. the line matches those characters exactly.)
grep("A computer once beat me at chess, but it was no match for me at kickboxing", tcorp)
# [1]  519059  835824 2283423

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#install.packages("tm")
library(tm)

# reduced twitter
twitmini <- head(tcorp, 1000000)

myCorpus <- Corpus(VectorSource(twitmini))

# tolower is exclused
#myCorpus <- tm_map(myCorpus, tolower)
 # remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'))
# tm_map: Interface to apply transformation functions (also denoted as mappings) to corpora.
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# check
inspect(myCorpus[1:100])
# convert to plaintext (see stack overflow)
myCorpus <- tm_map(myCorpus, PlainTextDocument)

date()
tdm <- TermDocumentMatrix(myCorpus)
date()

# findFreqTerms(tdm, lowfreq=1)

# dimnames(tdm)[[1]]
lhtdm <- tdm[c("love", "hate"), ]
a <- inspect(lhtdm)
apply(a, 1, sum)


  sum(grepl("love", twitmini))  
  sum(grepl("hate", twitmini))

twitmini[grep("love", twitmini)]
twitmini[grep("hate", twitmini)]
