
library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Prediction"),
    
    # Sidebar with controls to provide a caption, select a dataset,
    # and specify the number of observations to view. Note that
    # changes made to the caption in the textInput control are
    # updated in the output area immediately as you type
    sidebarLayout(
        sidebarPanel(
            textInput("captionEN",  "Write here:"),
            textInput("captionDE",  "Schreib hier:")
        ),
        
        # Show the caption, a summary of the dataset and an HTML 
        # table with the requested number of observations
        mainPanel(
           br(),
            h3(textOutput("captionEN", container = span)),br(),
            h3(textOutput("captionDE", container = span))
            
        )
    )
))
