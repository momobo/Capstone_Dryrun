library(shiny)
library(datasets)

source("https://raw.githubusercontent.com/momobo/Capstone_Dryrun/master/code/CapstoneBase.R", local=T, encoding="UTF-8")

#datadir <- "C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone\\github\\Capstone_Dryrun\\code\\experiments\\simple"
datadir <- "."
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
    if(tolower){
        string <- myTolower(string)
    }
    if(" " == substr(string, nchar(string),nchar(string))){
        res <- nextWord(string, df1, df2, df3)
        return(res[1])
    }else{
        res <- completeWord(string, dict)
        return(res[1])
    }
}

purgeLastWord <- function(string){
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
        cEN <- input$captionEN
        if(substring(cEN, nchar(cEN), nchar(cEN)) == ","){
            cEN_no_comma <- substring(cEN, 1, nchar(cEN)-1)
            hint <- fun(cEN_no_comma, df1E, df2E, df3E, wordsE)
            
            if(substring(cEN_no_comma, nchar(cEN_no_comma), nchar(cEN_no_comma)) == " "){
                # if last word is a space no need to purge last word
                updEN(paste0(cEN_no_comma, hint))
            }else{
                cEN_purged <- purgeLastWord(cEN_no_comma)
                updEN(paste(cEN_purged, hint))
            }
        }
        cDE <- input$captionDE
        if(substring(cDE, nchar(cDE), nchar(cDE)) == ","){
            cDE_no_comma <- substring(cDE, 1, nchar(cDE)-1)
            hint <- fun(cDE_no_comma, df1E, df2E, df3E, wordsE)
            
            if(substring(cDE_no_comma, nchar(cDE_no_comma), nchar(cDE_no_comma)) == " "){
                # if last word is a space no need to purge last word
                updDE(paste0(cDE_no_comma, hint))
            }else{
                cDE_purged <- purgeLastWord(cDE_no_comma)
                updDE(paste(cDE_purged, hint))
            }
        }
    })
    output$captionEN <- renderText({
        fun(input$captionEN, df1E, df2E, df3E, wordsE)
    })
    output$captionDE <- renderText({
        fun(input$captionDE, df1D, df2D, df3D, wordsD, tolower=F)
        
    })

})
