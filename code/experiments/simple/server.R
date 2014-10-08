library(shiny)
library(datasets)

# load functions from github
source("https://raw.githubusercontent.com/momobo/Capstone_Dryrun/master/code/CapstoneBase.R", local=T, encoding="UTF-8")

datadir <- "."
# choose the characters that select the word (1,2,3)
CHOICE1 <- ","
CHOICE2 <- "."
CHOICE3 <- "/"

# load english language
lang <- "en_US"
load(file=paste(datadir, lang, "ntddf.save",  sep="/"), verbose=T)
load(file=paste(datadir, lang, "ntddf2.save", sep="/"), verbose=T)
load(file=paste(datadir, lang, "ntddf3.save", sep="/"), verbose=T)
df1E <- copy(ntddf)
df2E <- copy(ntddf2)
df3E <- copy(ntddf3)
wordsE <- prepareWords(ntddf)

# load german language
lang <- "de_DE"
load(file=paste(datadir, lang, "ntddf.save",  sep="/"), verbose=T)
load(file=paste(datadir, lang, "ntddf2.save", sep="/"), verbose=T)
load(file=paste(datadir, lang, "ntddf3.save", sep="/"), verbose=T)
df1D <- copy(ntddf)
df2D <- copy(ntddf2)
df3D <- copy(ntddf3)
wordsD <- prepareWords(ntddf)

myTolower <- function(x) stri_trans_tolower(x, lang)

fun <-function(string, df1, df2, df3, dict, tolower=T){
    # get next word, depending on language
    # function nextword came from github
    if(tolower){
        string <- myTolower(string)
    }
    if(" " == substr(string, nchar(string),nchar(string))){
        res <- nextWord(string, df1, df2, df3)
        return(res)
    }else{
        res <- completeWord(string, dict)
        return(res)
    }
}

purgeLastWord <- function(string){
# purge last word that has to be rewrited
    a <-strsplit(string, split=" ")[[1]]
    a <- a[1:length(a)-1]
    return(paste(a, collapse=" "))
}

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output, session) {
    updEN <- function(val) updateTextInput(session, "captionEN", value = val)
    updDE <- function(val) updateTextInput(session, "captionDE", value = val)
    
    observe({
        
        # ENGLISH
        cEN <- input$captionEN
        nextCharE <- substring(cEN, nchar(cEN), nchar(cEN))
        if(nextCharE == CHOICE1 | nextCharE == CHOICE2| nextCharE == CHOICE3){
            cEN_clean <- substring(cEN, 1, nchar(cEN)-1)
            
            # choose the next word
            if(nextCharE == CHOICE1){
                hint <- fun(cEN_clean, df1E, df2E, df3E, wordsE)[1]
            }else if(nextCharE == CHOICE2){
                hint <- fun(cEN_clean, df1E, df2E, df3E, wordsE)[2]
            }else if(nextCharE == CHOICE3){
                hint <- fun(cEN_clean, df1E, df2E, df3E, wordsE)[3]
            } # no else here
            
            hint <- paste0(hint, " ")
            if(substring(cEN_clean, nchar(cEN_clean), nchar(cEN_clean)) == " "){
                # if last word is a space no need to purge last word
                updEN(paste0(cEN_clean, hint))
            }else{
                cEN_purged <- purgeLastWord(cEN_clean)
                updEN(paste(cEN_purged, hint))
            }
        }
        
        # GERMAN
        cDE <- input$captionDE
        nextCharD <- substring(cDE, nchar(cDE), nchar(cDE))
        if(nextCharD == CHOICE1 | nextCharD == CHOICE2| nextCharD == CHOICE3){
            cDE_clean <- substring(cDE, 1, nchar(cDE)-1)
            
            
            if(nextCharD == CHOICE1){
                hint <- fun(cDE_clean, df1D, df2D, df3D, wordsD, tolower=F)[1]
            }else if(nextCharD == CHOICE2){
                hint <- fun(cDE_clean, df1D, df2D, df3D, wordsD, tolower=F)[2]
            }else if(nextCharD == CHOICE3){
                hint <- fun(cDE_clean, df1D, df2D, df3D, wordsD, tolower=F)[3]
            } # no else here
            
            hint <- paste0(hint, " ")   
            
            if(substring(cDE_clean, nchar(cDE_clean), nchar(cDE_clean)) == " "){
                # if last word is a space no need to purge last word
                updDE(paste0(cDE_clean, hint))
            }else{
                cDE_purged <- purgeLastWord(cDE_clean)
                updDE(paste(cDE_purged, hint))
            }
        }
    })
    output$captionEN <- renderText({
        a <- fun(input$captionEN, df1E, df2E, df3E, wordsE, tolower=T)
        a <- paste(a, collapse=" - ")
        paste0("-> ", a)
    })
    output$captionDE <- renderText({
        a <- fun(input$captionDE, df1D, df2D, df3D, wordsD, tolower=F)
        a <- paste(a, collapse=" - ")
        paste0("-> ", a)
        
    })

})
