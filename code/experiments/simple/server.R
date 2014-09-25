library(shiny)
library(datasets)
source("https://raw.githubusercontent.com/momobo/Capstone_Dryrun/master/code/CapstoneBase.R")
datadir <- "C:\\Users\\mmorelli\\Google Drive\\Data Science\\10_Capstone\\github\\Capstone_Dryrun\\code\\experiments\\simple"
#datadir <- getwd()
lang <- "de_DE"
load(file=paste(datadir, lang, "ntddf.save",  sep="/"), verbose=T)
load(file=paste(datadir, lang, "ntddf2.save", sep="/"), verbose=T)
load(file=paste(datadir, lang, "ntddf3.save", sep="/"), verbose=T)
words <- prepareWords(ntddf)


funE <-function(string, df1, df2, df3, dict){
    # print(paste(datadir, lang, "ntddf3.save", sep="\\"))
    # if last char is space
    if(" " == substr(string, nchar(string),nchar(string))){
        res <- nextWord(string, df1, df2, df3)
        return(res[1])
    }else {
        res <- completeWord(string, dict)
        return(res[1])
    }
}
?load
# Define server logic required to summarize and view the selected
# dataset
shinyServer(function(input, output) {
    
    # By declaring datasetInput as a reactive expression we ensure 
    # that:
    #
    #  1) It is only called when the inputs it depends on changes
    #  2) The computation and result are shared by all the callers 
    #      (it only executes a single time)
    #
#     datasetInput <- reactive({
#         switch(input$dataset,
#                "rock" = rock,
#                "pressure" = pressure,
#                "cars" = cars)
#     })
#     
#     # The output$caption is computed based on a reactive expression
    # that returns input$caption. When the user changes the
    # "caption" field:
    #
    #  1) This function is automatically called to recompute the 
    #     output 
    #  2) The new caption is pushed back to the browser for 
    #     re-display
    # 
    # Note that because the data-oriented reactive expressions
    # below don't depend on input$caption, those expressions are
    # NOT called when input$caption changes.
    output$caption <- renderText({
        funE(input$caption, ntddf, ntddf2, ntddf3, words)
    })
    
    # The output$summary depends on the datasetInput reactive
    # expression, so will be re-executed whenever datasetInput is
    # invalidated
    # (i.e. whenever the input$dataset changes)
#     output$summary <- renderPrint({
#         dataset <- datasetInput()
#         summary(dataset)
#     })
#     
#     # The output$view depends on both the databaseInput reactive
#     # expression and input$obs, so will be re-executed whenever
#     # input$dataset or input$obs is changed. 
#     output$view <- renderTable({
#         head(datasetInput(), n = input$obs)
#     })
})
