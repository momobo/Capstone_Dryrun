#-------------------------------------------------------------------------------
# Capstone Project
# Training language file
#-------------------------------------------------------------------------------
# (modify the following dir to your working directory)
Capstone_Main_Dir <- "C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone"
Github_URL <- "https://raw.githubusercontent.com/momobo/Capstone_Dryrun/master/code/CapstoneBase.R"

# decide the language to train for
lang <- "en_US"
#lang <- "de_DE"

#############    initialize variables ########################

setwd(Capstone_Main_Dir)
source(Github_URL)



if(lang=="en_US"){
    TOLOWER <- T
}else if(lang=="de_DE"){
    TOLOWER <- F
}


datadir <- paste0(".\\data\\final\\", lang)
fileb   <- paste0(lang, ".blogs.txt")
file <- paste(datadir, fileb, sep="\\")
trainfile <- paste(datadir, "train.txt", sep="\\")

#     LOADBAD <- F # load bad words?
#     if(LOADBAD){ 
#         # modify to reload, badword not used at the moment
#         urlbad <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
#         download.file(urlbad, ".\\badWords", method = "auto", quiet=FALSE)
#         badwords <- readLines(".\\badWords")
#     }
#############################   PARAMETERS   ###################################
BBG     <- "__s__"                        # begin of phrase token
EEN     <- "__es__"                       # end of phrase token
APO     <- "__ap__"                       # special token for apostrophe
CFILES  <- "stucknames.save"              # the list of corpus pieces
limit   <-  5000                          # (short sample for testing purposes) 
#limit   <-  1E10                         # (no limit, full sample) 
mapLen  <- 2000                           # corpus piece length
NOHAPAX <- F                              # if True ignore Hapax in multigram
D       <- 0.75                           # Discount parameter
MINPROB <- 1E-8                           # default probability (low) 
################################################################################


#-----------------------------------------------------------------

#-------------------------------
----------------------------------
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
# 
# corpfiles <- fakeCorp(270)

# load list of corpus files
# corpfiles
# load saved corpus
#load(file=paste(datadir, "01_traincorpus.save", sep="\\"))
# inspect(chunk_corpus[1:100])



#-----------------------------------------------------------------
# create td data table from corpus
RELOAD03 <- T
if(RELOAD03){
    
    # create data tables
    system.time(tddf  <- pasteNGram(corpfiles, 1, tolower=TOLOWER))
    system.time(tddf2 <- pasteNGram(corpfiles, 2, tolower=TOLOWER))
    system.time(tddf3 <- pasteNGram(corpfiles, 3, tolower=TOLOWER))
    
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
RELOAD04 <- T
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
ntddf$term

system.time(perp1 <- measurePerp(NN=100, slot=1))
perp1
system.time(perp2 <- measurePerp(NN=100, slot=2))
perp2
system.time(perp3 <- measurePerp(NN=100, slot=3))
perp3
system.time(perp4 <- measurePerp(NN=100, slot=4))
perp4
system.time(perp5 <- measurePerp(NN=100, slot=5))
perp5

perp1
perp3
perp4


#-----------------------------------------------------------------

#-----------------------------------------------------------------
here tests

nextWord("fewfwew")

words <- prepareWords(ntddf)
prediction <- findNextWord("is", words)
prediction


findNextWord("the")
a <- nextWord("let's just")
a


completeWord("res", words)





