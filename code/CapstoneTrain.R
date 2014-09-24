setwd("C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone")
source(".\\github\\Capstone_Dryrun\\code\\CapstoneBase.R")

source_https <- function(url, ...) {
    # source from github. Courtesy of: http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/
        # load package
    library(RCurl)
    
    # parse and evaluate each .R script
    sapply(c(url, ...), function(u) {
        eval(parse(text = getURL(u, followlocation = TRUE, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))), envir = .GlobalEnv)
    })
}
source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R"

#-----------------------------------------------------------------
# divide between train, validation, test
RELOAD01 <- F
if(RELOAD01){
    partitionData(datadir, fileb)
}
#-----------------------------------------------------------------
# create corpus from training data
RELOAD02 <- F
if(RELOAD02){
    corpfiles <- createCorpus(trainfile, limit=limit)
    save(corpfiles, file=paste(datadir, CFILES, sep="\\"))
}else{
    load(file=paste(datadir, CFILES, sep="\\"))
}
#  (use fakeCorp to load only a handful of corpus chunks)
# corpfiles <- fakeCorp(2)

# load list of corpus files
# corpfiles
# load saved corpus
#load(file=paste(datadir, "01_traincorpus.save", sep="\\"))
# inspect(chunk_corpus[1:100])



#-----------------------------------------------------------------
# create td data table from corpus
RELOAD03 <- F
if(RELOAD03){
    system.time(tddf  <- pasteNGram(corpfiles, 1))
    system.time(tddf2 <- pasteNGram(corpfiles, 2))
    system.time(tddf3 <- pasteNGram(corpfiles, 3))
    
    #  add index and remove start of phrase
    tddf2 <- addIndexN(tddf2 ,2)
    tddf3 <- addIndexN(tddf3 ,3)

    tddf  <- removeStartOfPhrase(tddf,  1)
    tddf2 <- removeStartOfPhrase(tddf2, 2)
    tddf3 <- removeStartOfPhrase(tddf3, 3)

    
    save(tddf,  file= paste(datadir, "tddf.save",  sep="\\"))
    save(tddf2, file= paste(datadir, "tddf2.save", sep="\\"))
    save(tddf3, file= paste(datadir, "tddf3.save", sep="\\"))
}else{
    
    load(file=paste(datadir, "tddf.save",  sep="\\"))
    load(file=paste(datadir, "tddf2.save", sep="\\"))
    load(file=paste(datadir, "tddf3.save", sep="\\"))
    #load(file=paste(datadir, "tddf4.save", sep="\\"))
}


#-------------------------

# fourth phase: calculate probability
RELOAD04 <- F
if(RELOAD04){
    system.time(ntddf <-  addKNUnigram(tddf, tddf2, tddf3))
    system.time(ntddf2 <- addKNBigram(ntddf, tddf2, tddf3))
    system.time(ntddf3 <- addKNTrigram(ntddf2, tddf3))
    
    save(ntddf,  file= paste(datadir, "ntddf.save",  sep="\\"))
    save(ntddf2, file= paste(datadir, "ntddf2.save", sep="\\"))
    save(ntddf3, file= paste(datadir, "ntddf3.save", sep="\\"))
}else{
    load(file=paste(datadir, "ntddf.save",  sep="\\"))
    load(file=paste(datadir, "ntddf2.save", sep="\\"))
    load(file=paste(datadir, "ntddf3.save", sep="\\"))
}
#


# ****************************************************    SONO QUI
quiz("case of", c("beer", "soda", "cheese", "pretzel"))
#-----------------------------------------------------------------

# ******************* END OF TRAINING  *************************************

system.time(perp1 <- measurePerp(NN=100, slot=1))
system.time(perp2 <- measurePerp(NN=100, slot=2))
system.time(perp3 <- measurePerp(NN=100, slot=3))
system.time(perp4 <- measurePerp(NN=100, slot=4))
system.time(perp5 <- measurePerp(NN=100, slot=5))


perp1
perp2
perp3
perp4


#-----------------------------------------------------------------

words <- prepareWords(tddf)
prediction <- findNextWord("let is", words)
prediction

a <- tddf[substr(tddf$term, 1, 3)=="let",]
a$term

findNextWord("the", words)
