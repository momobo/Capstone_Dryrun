library(shiny)

shinyUI(fluidPage(
  title = 'Selectize examples',
  sidebarLayout(sidebarPanel(
    selectizeInput(
      'e2', '2. Multi-select', choices = state.name, multiple = TRUE
    ),
    selectizeInput(
      'e3', '3. Item creation', choices = state.name,
      options = list(create = TRUE)
    ),
    selectizeInput(
      'e4', '4. Max number of options to show', choices = state.name,
      options = list(maxOptions = 5)
    ),
    selectizeInput(
      'e5', '5. Max number of items to select', choices = state.name,
      multiple = TRUE, options = list(maxItems = 2)
    ),
    # I() indicates it is raw JavaScript code that should be evaluated, instead
    # of a normal character string
    selectizeInput(
      'e6', '6. Placeholder', choices = state.name,
      options = list(
        placeholder = 'Please select an option below',
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectInput(
      'e7', '7. selectInput() does not respond to empty strings',
      choices = state.name
    )

  ),
  mainPanel(
    helpText('Output of the examples in the left:'),
    verbatimTextOutput('ex_out'),
    # use Github instead
    
    helpText('If the above searching fails, it is probably the Github API limit
             has been reached (5 per minute). You can try later.'),
    verbatimTextOutput('github')
  )
)))

