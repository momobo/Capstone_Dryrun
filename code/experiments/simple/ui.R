
library(shiny)
boxstyle <- "border: 1px solid;box-shadow: 2px 2px 1px #888888;"

# Define UI for dataset viewer application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Swiftkey JHU Capstone Project"),
    
    # Sidebar with controls to provide a caption, select a dataset,
    # and specify the number of observations to view. Note that
    # changes made to the caption in the textInput control are
    # updated in the output area immediately as you type
    
    sidebarLayout(
        sidebarPanel(
            img(src="united_kingdom_round_icon_64.png", height = 64, width = 64),
            br(),
            textInput("captionEN",  "Write here:"), br(),
            div(textOutput("captionEN", container = span), style=boxstyle),
            br(),
            br(),
            br(),
            br(),
            br(),
            img(src="germany_round_icon_64.png", height = 64, width = 64),
            textInput("captionDE",  "Schreib hier:"),
            br(),
            div(textOutput("captionDE", container = span), style=boxstyle)
        ),
        
        # Show the caption, a summary of the dataset and an HTML 
        # table with the requested number of observations
        mainPanel(
            
            p("p creates a paragraph of text. Note: this paragraph is followed by br(), which makes a blank line."),
            p("A new p() command starts a new paragraph. Supply a style attribute to change the format of the entire paragraph", style = "font-family: 'times'; font-si16pt"),
            strong("strong() makes bold text."),
            em("em() creates italicized (i.e, emphasized) text."),
            br(),
            code("code displays your text similar to computer code"),
            div("div creates segments of text with a similar style. This division of text is all blue because I passed the argument 'style = color:blue' to div", style = "color:blue"),
            br(),
            p("span does the same thing as div, but it works with",
              span("groups of words", style = "color:blue"),
              "that appear inside a paragraph.")
        
            #
        )
    )
))
