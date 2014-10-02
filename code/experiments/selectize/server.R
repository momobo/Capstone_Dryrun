library(shiny)


shinyServer(function(input, output, session) {
    upd <- function(x, val) updateTextInput(session, "inText", value = val)
    
    observe({
        # We'll use the input$controller variable multiple times, so save it as x
        # for convenience.
        x <- input$inText
        
        # This will change the value of input$inText, based on x
        if(substring(x, nchar(x), nchar(x)) == ","){
            val <- paste(x, "abc")
            upd(x, val)
        }
     
     })
})

