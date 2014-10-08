library(shiny)
runApp( list(ui = bootstrapPage(
    verbatimTextOutput("results"),
    tags$script('
    $(document).on("keydown", function (e) {
       Shiny.onInputChange("mydata", e.which);
    });
  ') 
    )
    , server = function(input, output, session) {
        
        output$results = renderPrint({
            input$mydata
        })
    }
))