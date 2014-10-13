library(shiny)
boxstyle <- "border: 1px solid;box-shadow: 2px 2px 1px #888888;"

# Define UI for dataset viewer application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Swiftkey JHU Capstone Project"),
        
    sidebarLayout(
        sidebarPanel(
            img(src="united_kingdom_round_icon_64.png", height = 64, width = 64),
            br(),
            textInput("captionEN",  "Write here on English:"), br(),
            div(textOutput("captionEN", container = span), style=boxstyle),
            br(),
            br(),
            br(),
            br(),
            br(),
            img(src="germany_round_icon_64.png", height = 64, width = 64),
            textInput("captionDE",  "Schreib hier auf Deutsch:"),
            br(),
            div(textOutput("captionDE", container = span), style=boxstyle)
        ),
        
        mainPanel(
            
            strong("Prediction app instructions."),
            br(),
            p("Wait until the app is fully loaded (the prediction boxes become visible), then just type in the textbox for the desidered language. Under the textbox, in a box, three suggestion for the next word will appear."),
            p("If you want to choose one of the suggestions type a one (1) for the first, a two (2) for the second and a three (3) for the third."),
            br(),
            p("Be aware that there are two prediction engines: "),
            tags$ol(
                tags$li("at the beginning of the phrase and when jou just typed a space the engine is a Kneser-Ney trigram prediction algorithm,"), 
                tags$li("in the middle of a word the engine is a Maximum Likelihood estimation for the completion of the last word (this is fancy speak to say that I just propose the three most frequent completion of the word).")
            ),
            br(),
            p("All the code is available on my github:", a("https://github.com/momobo/Capstone_Dryrun"), " where a detailed report on the methods utilized is also present.")

        )
    )
))
