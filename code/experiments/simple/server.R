library(shiny)
library(datasets)

source("./CapstoneBaseCopy.R", local=T, encoding="UTF-8")

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

# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
    
    output$captionEN <- renderText({
        fun(input$captionEN, df1E, df2E, df3E, wordsE)
    })
    output$captionDE <- renderText({
        fun(input$captionDE, df1D, df2D, df3D, wordsD, tolower=F)
    })
    

})
